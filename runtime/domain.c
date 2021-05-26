/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/domain.h"
#include "caml/domain_state.h"
#include "caml/platform.h"
#include "caml/custom.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/globroots.h"
#include "caml/signals.h"
#include "caml/alloc.h"
#include "caml/startup.h"
#include "caml/fiber.h"
#include "caml/callback.h"
#include "caml/minor_gc.h"
#include "caml/eventlog.h"
#include "caml/gc_ctrl.h"
#include "caml/osdeps.h"
#include "caml/weak.h"
#include "caml/finalise.h"
#include "caml/gc_ctrl.h"

#define BT_IN_BLOCKING_SECTION 0
#define BT_ENTERING_OCAML 1
#define BT_TERMINATE 2
#define BT_INIT 3

/* Since we support both heavyweight OS threads and lightweight
   userspace threads, the word "thread" is ambiguous. This file deals
   with OS-level threads, called "domains".
*/


/* control of interrupts */
struct interruptor {
  atomic_uintnat* interrupt_word;
  caml_plat_mutex lock;
  caml_plat_cond cond;

  int running;
  int terminating;
  /* unlike the domain ID, this ID number is not reused */
  uintnat unique_id;

  /* Queue of domains trying to send interrupts here */
  struct interrupt* qhead;
  struct interrupt* qtail;      /* defined only when qhead != NULL */
};

struct interrupt {
  /* immutable fields */
  domain_rpc_handler handler;
  void* data;

  atomic_uintnat acknowledged;

  /* accessed only when target's lock held */
  struct interrupt* next;
};

/* returns 0 on failure, if the target has terminated. */
CAMLcheckresult
int caml_send_interrupt(struct interruptor* self,
                        struct interruptor* target,
                        domain_rpc_handler handler,
                        void* data);
void caml_handle_incoming_interrupts(void);


struct dom_internal {
  /* readonly fields, initialised and never modified */
  atomic_uintnat* interrupt_word_address;
  int id;
  struct domain state;
  struct interruptor interruptor;

  /* backup thread */
  int backup_thread_running;
  pthread_t backup_thread;
  atomic_uintnat backup_thread_msg;
  caml_plat_mutex domain_lock;
  caml_plat_cond domain_cond;

  /* readonly */
  uintnat tls_area;
  uintnat tls_area_end;
  uintnat minor_heap_area;
  uintnat minor_heap_area_end;
};
typedef struct dom_internal dom_internal;

static uintnat handle_incoming(struct interruptor* s);

static caml_plat_mutex all_domains_lock = CAML_PLAT_MUTEX_INITIALIZER;
static caml_plat_cond all_domains_cond = CAML_PLAT_COND_INITIALIZER(&all_domains_lock);
static atomic_uintnat /* dom_internal* */ stw_leader = 0;
static struct dom_internal all_domains[Max_domains];

CAMLexport atomic_uintnat caml_num_domains_running;

CAMLexport uintnat caml_minor_heaps_base;
CAMLexport uintnat caml_minor_heaps_end;
CAMLexport uintnat caml_tls_areas_base;
static __thread dom_internal* domain_self;

static int64_t startup_timestamp;

#ifdef __APPLE__
/* OSX has issues with dynamic loading + exported TLS.
    This is slower but works */
CAMLexport pthread_key_t caml_domain_state_key;
static pthread_once_t key_once = PTHREAD_ONCE_INIT;

static void caml_make_domain_state_key ()
{
  (void) pthread_key_create (&caml_domain_state_key, NULL);
}

void caml_init_domain_state_key ()
{
  pthread_once(&key_once, caml_make_domain_state_key);
}

#else
CAMLexport __thread caml_domain_state* Caml_state;
#endif

asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t page_size = caml_mem_round_up_pages(1);
  asize_t bs, max;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = caml_mem_round_up_pages(Bsize_wsize (wsize));

  Assert(page_size * 2 < Minor_heap_max);
  max = Minor_heap_max - page_size * 2;

  if (bs > max) bs = max;

  return Wsize_bsize(bs);
}

int caml_reallocate_minor_heap(asize_t wsize)
{
  caml_domain_state* domain_state = Caml_state;
  Assert(domain_state->young_ptr == domain_state->young_end);

  /* free old minor heap.
     instead of unmapping the heap, we decommit it, so there's
     no race whereby other code could attempt to reuse the memory. */
  caml_mem_decommit((void*)domain_self->minor_heap_area,
                    domain_self->minor_heap_area_end - domain_self->minor_heap_area);

  wsize = caml_norm_minor_heap_size(wsize);

  if (!caml_mem_commit((void*)domain_self->minor_heap_area, Bsize_wsize(wsize))) {
    return -1;
  }

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)domain_self->minor_heap_area;
    for (; p < (uintnat*)(domain_self->minor_heap_area + Bsize_wsize(wsize)); p++)
      *p = Debug_uninit_align;
  }
#endif

  domain_state->minor_heap_wsz = wsize;

  domain_state->young_start = (char*)domain_self->minor_heap_area;
  domain_state->young_end = (char*)(domain_self->minor_heap_area + Bsize_wsize(wsize));
  domain_state->young_limit = (uintnat) domain_state->young_start;
  domain_state->young_ptr = domain_state->young_end;
  return 0;
}

/* must be run on the domain's thread */
static void create_domain(uintnat initial_minor_heap_wsize) {
  int i;
  dom_internal* d = 0;
  Assert (domain_self == 0);

  caml_plat_lock(&all_domains_lock);

  /* wait until any in-progress STW sections end */
  while (atomic_load_acq(&stw_leader)) caml_plat_wait(&all_domains_cond);

  for (i = 0; i < Max_domains && !d; i++) {
    struct interruptor* s = &all_domains[i].interruptor;
    caml_plat_lock(&s->lock);
    if (!s->running) {
      d = &all_domains[i];
      if (!d->interrupt_word_address) {
        caml_domain_state* domain_state;
        atomic_uintnat* young_limit;
        /* never been started before, so set up minor heap */
        if (!caml_mem_commit((void*)d->tls_area, (d->tls_area_end - d->tls_area))) {
          /* give up now: if we couldn't get memory for this domain, we're
             unlikely to have better luck with any other */
          d = 0;
          caml_plat_unlock(&s->lock);
          break;
        }
      	domain_state = (caml_domain_state*)(d->tls_area);
        young_limit = (atomic_uintnat*)&domain_state->young_limit;
        d->interrupt_word_address = young_limit;
        atomic_store_rel(young_limit, (uintnat)domain_state->young_start);
        s->interrupt_word = young_limit;
      }
      Assert(s->qhead == NULL);
      s->running = 1;
      atomic_fetch_add(&caml_num_domains_running, 1);
    }
    caml_plat_unlock(&s->lock);
  }
  if (d) {
    caml_domain_state* domain_state;
    d->state.internals = d;
    domain_self = d;
    SET_Caml_state((void*)(d->tls_area));
    domain_state = (caml_domain_state*)(d->tls_area);
    caml_plat_lock(&d->domain_lock);

    domain_state->id = d->id;
    domain_state->unique_id = d->interruptor.unique_id;
    d->state.state = domain_state;
    domain_state->critical_section_nesting = 0;

    if (caml_init_signal_stack() < 0) {
      goto init_signal_stack_failure;
    }

    domain_state->young_start = domain_state->young_end =
      domain_state->young_ptr = 0;
    domain_state->minor_tables = caml_alloc_minor_tables();
    if(domain_state->minor_tables == NULL) {
      goto alloc_minor_tables_failure;
    }

    d->state.state->shared_heap = caml_init_shared_heap();
    if(d->state.state->shared_heap == NULL) {
      goto init_shared_heap_failure;
    }

    if (caml_init_major_gc(domain_state) < 0) {
      goto init_major_gc_failure;
    }

    if(caml_reallocate_minor_heap(initial_minor_heap_wsize) < 0) {
      goto reallocate_minor_heap_failure;
    }

    domain_state->dls_root = Val_unit;
    caml_register_generational_global_root(&domain_state->dls_root);

    domain_state->unique_token_root = caml_alloc_shr_noexc(Abstract_tag, Val_unit);
    if(domain_state->unique_token_root == (value)NULL) {
      goto create_unique_token_failure;
    }

    caml_register_generational_global_root(&domain_state->unique_token_root);

    domain_state->stack_cache = caml_alloc_stack_cache();
    if(domain_state->stack_cache == NULL) {
      goto create_stack_cache_failure;
    }

    domain_state->current_stack =
        caml_alloc_main_stack(Stack_size / sizeof(value));
    if(domain_state->current_stack == NULL) {
      goto alloc_main_stack_failure;
    }

    domain_state->backtrace_buffer = NULL;
    domain_state->backtrace_last_exn = Val_unit;
    caml_register_generational_global_root(&domain_state->backtrace_last_exn);
#ifndef NATIVE_CODE
    domain_state->external_raise = NULL;
    domain_state->trap_sp_off = 1;
#endif


#if defined(NAKED_POINTERS_CHECKER) && !defined(_WIN32)
    domain_state->checking_pointer_pc = NULL;
#endif

    goto domain_init_complete;

  caml_free_stack(domain_state->current_stack);
alloc_main_stack_failure:
create_stack_cache_failure:
  caml_remove_generational_global_root(&domain_state->unique_token_root);
  caml_remove_generational_global_root(&domain_state->dls_root);
create_unique_token_failure:
reallocate_minor_heap_failure:
  caml_teardown_major_gc();
init_major_gc_failure:
  caml_teardown_shared_heap(d->state.state->shared_heap);
init_shared_heap_failure:
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = NULL;
alloc_minor_tables_failure:
  caml_free_signal_stack();
init_signal_stack_failure:
  domain_self = NULL;

  }
domain_init_complete:
  caml_plat_unlock(&all_domains_lock);
}

CAMLexport void caml_reset_domain_lock(void)
{
  dom_internal* self = domain_self;
  // This is only used to reset the domain_lock state on fork.
  caml_plat_mutex_init(&self->domain_lock);
  caml_plat_cond_init(&self->domain_cond, &self->domain_lock);

  return;
}

void caml_init_domains(uintnat minor_heap_wsz) {
  int i;
  uintnat size;
  uintnat tls_size;
  uintnat tls_areas_size;
  void* heaps_base;
  void* tls_base;

  /* sanity check configuration */
  if (caml_mem_round_up_pages(Minor_heap_max) != Minor_heap_max)
    caml_fatal_error("Minor_heap_max misconfigured for this platform");

  /* reserve memory space for minor heaps and tls_areas */
  size = (uintnat)Minor_heap_max * Max_domains;
  tls_size = caml_mem_round_up_pages(sizeof(caml_domain_state));
  tls_areas_size = tls_size * Max_domains;

  heaps_base = caml_mem_map(size*2, size*2, 1 /* reserve_only */);
  tls_base = caml_mem_map(tls_areas_size, tls_areas_size, 1 /* reserve_only */);
  if (!heaps_base || !tls_base) caml_raise_out_of_memory();

  caml_minor_heaps_base = (uintnat) heaps_base;
  caml_minor_heaps_end = (uintnat) heaps_base + size;
  caml_tls_areas_base = (uintnat) tls_base;

  for (i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];
    uintnat domain_minor_heap_base;
    uintnat domain_tls_base;

    caml_plat_mutex_init(&dom->interruptor.lock);
    caml_plat_cond_init(&dom->interruptor.cond,
                        &dom->interruptor.lock);
    dom->interruptor.qhead = dom->interruptor.qtail = NULL;
    dom->interruptor.running = 0;
    dom->interruptor.terminating = 0;
    dom->interruptor.unique_id = i;
    dom->id = i;

    caml_plat_mutex_init(&dom->domain_lock);
    caml_plat_cond_init(&dom->domain_cond, &dom->domain_lock);
    dom->backup_thread_running = 0;
    dom->backup_thread_msg = BT_INIT;

    domain_minor_heap_base = caml_minor_heaps_base +
      (uintnat)Minor_heap_max * (uintnat)i;
    domain_tls_base = caml_tls_areas_base + tls_size * (uintnat)i;
    dom->tls_area = domain_tls_base;
    dom->tls_area_end = domain_tls_base + tls_size;
    dom->minor_heap_area = domain_minor_heap_base;
    dom->minor_heap_area_end = domain_minor_heap_base + Minor_heap_max;
  }


  create_domain(minor_heap_wsz);
  if (!domain_self) caml_fatal_error("Failed to create main domain");

  caml_init_signal_handling();
  startup_timestamp = caml_time_counter();

  CAML_EVENTLOG_INIT();
}

void caml_init_domain_self(int domain_id) {
  Assert (domain_id >= 0 && domain_id < Max_domains);
  domain_self = &all_domains[domain_id];
  SET_Caml_state(domain_self->state.state);
}

enum domain_status { Dom_starting, Dom_started, Dom_failed };
struct domain_startup_params {
  struct interruptor* parent;
  enum domain_status status;
  value* callback;
  dom_internal* newdom;
  uintnat unique_id;
};

static void* backup_thread_func(void* v)
{
  dom_internal* di = (dom_internal*)v;
  uintnat msg;
  struct interruptor* s = &di->interruptor;

  domain_self = di;
  SET_Caml_state((void*)(di->tls_area));

  CAML_EVENTLOG_IS_BACKUP_THREAD();

  /* TODO: how does the backup thread interact with the eventlog infra?
   * caml_ev_tag_self_as_backup_thread(); */

  msg = atomic_load_acq (&di->backup_thread_msg);
  while (msg != BT_TERMINATE) {
    Assert (msg <= BT_TERMINATE);
    switch (msg) {
      case BT_IN_BLOCKING_SECTION:
        /* Handle interrupts on behalf of the main thread:
         *  - must hold domain_lock to handle interrupts
         *  - need to guarantee no blocking so that backup thread
         *    can be signalled from caml_leave_blocking_section
         */
        if (caml_incoming_interrupts_queued()) {
          if (caml_plat_try_lock(&di->domain_lock)) {
            caml_handle_incoming_interrupts();
            caml_plat_unlock(&di->domain_lock);
          }
        }
        /* Wait safely if there is nothing to do.
         * Will be woken from caml_leave_blocking_section
         */
        caml_plat_lock(&s->lock);
        msg = atomic_load_acq (&di->backup_thread_msg);
        if (msg == BT_IN_BLOCKING_SECTION &&
            !caml_incoming_interrupts_queued())
          caml_plat_wait(&s->cond);
        caml_plat_unlock(&s->lock);
        break;
      case BT_ENTERING_OCAML:
        /* Main thread wants to enter OCaml
         * Will be woken from caml_enter_blocking_section
         * or domain_terminate
         */
        caml_plat_lock(&di->domain_lock);
        msg = atomic_load_acq (&di->backup_thread_msg);
        if (msg == BT_ENTERING_OCAML)
          caml_plat_wait(&di->domain_cond);
        caml_plat_unlock(&di->domain_lock);
        break;
      default:
        cpu_relax();
        break;
    };
    msg = atomic_load_acq (&di->backup_thread_msg);
  }

  /* doing terminate */
  atomic_store_rel(&di->backup_thread_msg, BT_INIT);

  return 0;
}

static void install_backup_thread (dom_internal* di)
{
  int err;

  if (di->backup_thread_running == 0) {
    Assert (di->backup_thread_msg == BT_INIT ||     /* Using fresh domain */
            di->backup_thread_msg == BT_TERMINATE); /* Reusing domain */

    while (atomic_load_acq(&di->backup_thread_msg) != BT_INIT) {
      /* Give a chance for backup thread on this domain to terminate */
      caml_plat_unlock (&di->domain_lock);
      cpu_relax ();
      caml_plat_lock (&di->domain_lock);
    }

    atomic_store_rel(&di->backup_thread_msg, BT_ENTERING_OCAML);
    err = pthread_create (&di->backup_thread, 0, backup_thread_func, (void*)di);
    if (err)
      caml_failwith("failed to create domain backup thread");
    di->backup_thread_running = 1;
    pthread_detach(di->backup_thread);
  }
}

static void caml_domain_stop_default(void)
{
  return;
}

static void caml_domain_start_default(void)
{
  return;
}

CAMLexport void (*caml_domain_start_hook)(void) =
   caml_domain_start_default;

CAMLexport void (*caml_domain_stop_hook)(void) =
   caml_domain_stop_default;

static void domain_terminate();

static void* domain_thread_func(void* v)
{
  struct domain_startup_params* p = v;
  value *domain_callback = (value*) p->callback;

  create_domain(caml_params->init_minor_heap_wsz);
  p->newdom = domain_self;

  caml_plat_lock(&p->parent->lock);
  if (domain_self) {
    p->status = Dom_started;
    p->unique_id = domain_self->interruptor.unique_id;
  } else {
    p->status = Dom_failed;
  }
  caml_plat_broadcast(&p->parent->cond);
  caml_plat_unlock(&p->parent->lock);
  /* cannot access p below here */

  if (domain_self) {
    install_backup_thread(domain_self);
    caml_gc_log("Domain starting (unique_id = %"ARCH_INTNAT_PRINTF_FORMAT"u)",
                domain_self->interruptor.unique_id);
    caml_domain_start_hook();
    caml_callback(*domain_callback, Val_unit);
    caml_remove_generational_global_root(domain_callback);
    caml_stat_free(domain_callback);
    domain_terminate();
  } else {
    caml_gc_log("Failed to create domain");
  }
  return 0;
}

#define Domainthreadptr_val(val) ((struct domain_thread**)Data_custom_val(val))

CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  struct domain_startup_params p;
  pthread_t th;
  int err;

  CAML_EV_BEGIN(EV_DOMAIN_SPAWN);
  p.parent = &domain_self->interruptor;
  p.status = Dom_starting;

  p.callback = (value*) caml_stat_alloc_noexc(sizeof(value));
  *p.callback = callback;
  caml_register_generational_global_root(p.callback);

  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
  if (err) {
    caml_failwith("failed to create domain thread");
  }

  caml_plat_lock(&domain_self->interruptor.lock);
  while (p.status == Dom_starting) {
    if (handle_incoming(&domain_self->interruptor) == 0)
      caml_plat_wait(&domain_self->interruptor.cond);
  }
  caml_plat_unlock(&domain_self->interruptor.lock);

  if (p.status == Dom_started) {
    /* successfully created a domain.
       p.callback is now owned by that domain */
    pthread_detach(th);
  } else {
    Assert (p.status == Dom_failed);
    /* failed */
    pthread_join(th, 0);
    caml_remove_generational_global_root(p.callback);
    caml_stat_free(p.callback);
    caml_failwith("failed to allocate domain");
  }
  install_backup_thread(domain_self);
  CAML_EV_END(EV_DOMAIN_SPAWN);
  CAMLreturn (Val_long(p.unique_id));
}

CAMLprim value caml_ml_domain_join(value domain)
{
    caml_failwith("domain.join unimplemented");
}

struct domain* caml_domain_self()
{
  return domain_self ? &domain_self->state : 0;
}

struct domain* caml_owner_of_young_block(value v) {
  int heap_id;
  Assert(Is_young(v));
  heap_id = ((uintnat)v - caml_minor_heaps_base) / Minor_heap_max;
  return &all_domains[heap_id].state;
}

CAMLprim value caml_ml_domain_id(value unit)
{
  CAMLnoalloc;
  return Val_int(domain_self->interruptor.unique_id);
}

CAMLprim value caml_ml_domain_unique_token (value unit)
{
  CAMLnoalloc;
  return Caml_state->unique_token_root;
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1);

static void interrupt_domain(dom_internal* d) {
  atomic_store_rel(d->interrupt_word_address, INTERRUPT_MAGIC);
}

static struct {
  atomic_uintnat domains_still_running;
  atomic_uintnat num_domains_still_processing;
  void (*callback)(struct domain*, void*, int participating_count, struct domain** others_participating);
  void* data;
  int num_domains;
  atomic_uintnat barrier;
  void (*enter_spin_callback)(struct domain*, void*);
  void* enter_spin_data;

  struct interrupt reqs[Max_domains];
  struct domain* participating[Max_domains];
} stw_request = {
  ATOMIC_UINTNAT_INIT(0),
  ATOMIC_UINTNAT_INIT(0),
  NULL,
  NULL,
  0,
  ATOMIC_UINTNAT_INIT(0),
  NULL,
  NULL,
  { { 0 } },
  { 0 }
};

/* sense-reversing barrier */
#define BARRIER_SENSE_BIT 0x100000

barrier_status caml_global_barrier_begin()
{
  uintnat b = 1 + atomic_fetch_add(&stw_request.barrier, 1);
  return b;
}

int caml_global_barrier_is_final(barrier_status b)
{
  return ((b & ~BARRIER_SENSE_BIT) == stw_request.num_domains);
}

void caml_global_barrier_end(barrier_status b)
{
  uintnat sense = b & BARRIER_SENSE_BIT;
  if (caml_global_barrier_is_final(b)) {
    /* last domain into the barrier, flip sense */
    atomic_store_rel(&stw_request.barrier, sense ^ BARRIER_SENSE_BIT);
  } else {
    /* wait until another domain flips the sense */
    SPIN_WAIT {
      uintnat barrier = atomic_load_acq(&stw_request.barrier);
      if ((barrier & BARRIER_SENSE_BIT) != sense) break;
    }
  }
}

void caml_global_barrier()
{
  barrier_status b = caml_global_barrier_begin();
  caml_global_barrier_end(b);
}

int caml_global_barrier_num_domains()
{
  return stw_request.num_domains;
}

static void decrement_stw_domains_still_processing()
{
  /* we check if we are the last to leave a stw section
     if so, clear the stw_leader to allow the new stw sections to start.
   */
  intnat am_last = atomic_fetch_add(&stw_request.num_domains_still_processing, -1) == 1;

  if( am_last ) {
    /* release the STW lock to allow new STW sections */
    caml_plat_lock(&all_domains_lock);
    atomic_store_rel(&stw_leader, 0);
    caml_plat_broadcast(&all_domains_cond);
    caml_gc_log("clearing stw leader");
    caml_plat_unlock(&all_domains_lock);
  }
}

static void caml_poll_gc_work();
static void stw_handler(struct domain* domain, void* unused2, interrupt* done)
{
#ifdef DEBUG
  caml_domain_state* domain_state = Caml_state;
#endif

  CAML_EV_BEGIN(EV_STW_HANDLER);
  caml_acknowledge_interrupt(done);
  CAML_EV_BEGIN(EV_STW_API_BARRIER);
  {
    SPIN_WAIT {
      if (atomic_load_acq(&stw_request.domains_still_running) == 0)
        break;
      caml_handle_incoming_interrupts();

      if (stw_request.enter_spin_callback)
        stw_request.enter_spin_callback(domain, stw_request.enter_spin_data);
    }
  }
  CAML_EV_END(EV_STW_API_BARRIER);

  #ifdef DEBUG
  domain_state->inside_stw_handler = 1;
  #endif
  stw_request.callback(domain, stw_request.data, stw_request.num_domains, stw_request.participating);
  #ifdef DEBUG
  domain_state->inside_stw_handler = 0;
  #endif

  decrement_stw_domains_still_processing();

  CAML_EV_END(EV_STW_HANDLER);

  /* poll the GC to check for deferred work
     we do this here because blocking or waiting threads only execute
     the interrupt handler and do not poll for deferred work*/
  caml_poll_gc_work();
}


#ifdef DEBUG
int caml_domain_is_in_stw() {
  caml_domain_state* domain_state = Caml_state;

  return domain_state->inside_stw_handler;
}
#endif

static int caml_send_partial_interrupt(
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data,
                         struct interrupt* req);
static void caml_wait_interrupt_acknowledged(struct interruptor* self, struct interrupt* req);

int caml_try_run_on_all_domains_with_spin_work(
  void (*handler)(struct domain*, void*, int, struct domain**), void* data,
  void (*leader_setup)(struct domain*),
  void (*enter_spin_callback)(struct domain*, void*), void* enter_spin_data)
{
#ifdef DEBUG
  caml_domain_state* domain_state = Caml_state;
#endif
  int i;
  uintnat domains_participating = 0;

  caml_gc_log("requesting STW");

  // Don't take the lock if there's already a stw leader
  if (atomic_load_acq(&stw_leader)) {
    caml_handle_incoming_interrupts();
    return 0;
  }

  /* Try to take the lock by setting ourselves as the stw_leader.
     If it fails, handle interrupts (probably participating in
     an STW section) and return. */
  caml_plat_lock(&all_domains_lock);
  if (atomic_load_acq(&stw_leader)) {
    caml_plat_unlock(&all_domains_lock);
    caml_handle_incoming_interrupts();
    return 0;
  } else {
    atomic_store_rel(&stw_leader, (uintnat)domain_self);
  }
  caml_plat_unlock(&all_domains_lock);

  CAML_EV_BEGIN(EV_STW_LEADER);
  caml_gc_log("causing STW");

  /* setup all fields for this stw_request, must have those needed
     for domains waiting at the enter spin barrier */
  stw_request.enter_spin_callback = enter_spin_callback;
  stw_request.enter_spin_data = enter_spin_data;
  stw_request.callback = handler;
  stw_request.data = data;
  atomic_store_rel(&stw_request.barrier, 0);
  atomic_store_rel(&stw_request.domains_still_running, 1);

  if( leader_setup ) {
    leader_setup(&domain_self->state);
  }

  /* Next, interrupt all domains, counting how many domains received
     the interrupt (i.e. are not terminated and are participating in
     this STW section). */
  {
    struct interrupt* reqs = stw_request.reqs;
    struct domain** participating = stw_request.participating;

    for (i = 0; i < Max_domains; i++) {
      if (&all_domains[i] == domain_self) {
        participating[domains_participating] = &domain_self->state;
        domains_participating++;
        continue;
      }
      if (caml_send_partial_interrupt(
                              &all_domains[i].interruptor,
                              stw_handler,
                              0,
                              &reqs[domains_participating])) {
        participating[domains_participating] = &all_domains[i].state;
        domains_participating++;
      }
    }

    for(i = 0; i < domains_participating ; i++) {
      if( participating[i] && &domain_self->state != participating[i] ) {
        caml_wait_interrupt_acknowledged(&domain_self->interruptor, &reqs[i]);
      }
    }
  }

  Assert(domains_participating > 0);

  /* setup the domain_participating fields */
  stw_request.num_domains = domains_participating;
  atomic_store_rel(&stw_request.num_domains_still_processing,
                   domains_participating);

  /* release from the enter barrier */
  atomic_store_rel(&stw_request.domains_still_running, 0);

  #ifdef DEBUG
  domain_state->inside_stw_handler = 1;
  #endif
  handler(&domain_self->state, data, domains_participating, stw_request.participating);
  #ifdef DEBUG
  domain_state->inside_stw_handler = 0;
  #endif

  decrement_stw_domains_still_processing();

  CAML_EV_END(EV_STW_LEADER);

  return 1;
}

int caml_try_run_on_all_domains(void (*handler)(struct domain*, void*, int, struct domain**), void* data, void (*leader_setup)(struct domain*))
{
  return caml_try_run_on_all_domains_with_spin_work(handler, data, leader_setup, 0, 0);
}

void caml_interrupt_self() {
  interrupt_domain(domain_self);
}

/* Arrange for a major GC slice to be performed on the current domain
   as soon as possible */
void caml_request_major_slice (void)
{
  Caml_state->requested_major_slice = 1;
  caml_interrupt_self();
}

/* Arrange for a minor GC to be performed on the current domain
   as soon as possible */
void caml_request_minor_gc (void)
{
  Caml_state->requested_minor_gc = 1;
  caml_interrupt_self();
}

static void caml_poll_gc_work()
{
  CAMLalloc_point_here;

  if (((uintnat)Caml_state->young_ptr - Bhsize_wosize(Max_young_wosize) <
       (uintnat)Caml_state->young_start) ||
      Caml_state->requested_minor_gc) {
    /* out of minor heap or collection forced */
    CAML_EV_BEGIN(EV_MINOR);
    Caml_state->requested_minor_gc = 0;
    caml_empty_minor_heaps_once();
    CAML_EV_END(EV_MINOR);

    /* FIXME: a domain will only ever call finalizers if its minor
      heap triggers the minor collection
      Care may be needed with finalizers running when the domain
      is waiting in a critical_section or in a blocking section
      and serviced by the backup thread.
      */
    CAML_EV_BEGIN(EV_MINOR_FINALIZED);
    caml_final_do_calls();
    CAML_EV_END(EV_MINOR_FINALIZED);
  }

  if (Caml_state->requested_major_slice) {
    CAML_EV_BEGIN(EV_MAJOR);
    Caml_state->requested_major_slice = 0;
    caml_major_collection_slice(AUTO_TRIGGERED_MAJOR_SLICE);
    CAML_EV_END(EV_MAJOR);
  }
}

void caml_handle_gc_interrupt()
{
  atomic_uintnat* young_limit = domain_self->interrupt_word_address;
  CAMLalloc_point_here;

  CAML_EV_BEGIN(EV_INTERRUPT_GC);
  if (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    CAML_EV_BEGIN(EV_INTERRUPT_REMOTE);
    while (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
      uintnat i = INTERRUPT_MAGIC;
      atomic_compare_exchange_strong(young_limit, &i, (uintnat)Caml_state->young_start);
    }
    caml_handle_incoming_interrupts();
    CAML_EV_END(EV_INTERRUPT_REMOTE);
  }

  caml_poll_gc_work();
  CAML_EV_END(EV_INTERRUPT_GC);
}

CAMLexport inline int caml_bt_is_in_blocking_section(void)
{
  dom_internal* self = domain_self;
  uintnat status = atomic_load_acq(&self->backup_thread_msg);
  if (status == BT_IN_BLOCKING_SECTION)
    return 1;
  else
    return 0;

}

CAMLexport inline intnat caml_domain_is_multicore ()
{
  dom_internal *self = domain_self;
  return (!caml_domain_alone() || self->backup_thread_running);
}

CAMLexport void caml_acquire_domain_lock(void)
{
  dom_internal* self = domain_self;
  caml_plat_lock(&self->domain_lock);
  return;
}

CAMLexport void caml_bt_enter_ocaml(void)
{
  dom_internal* self = domain_self;

  Assert(caml_domain_alone() || self->backup_thread_running);

  if (self->backup_thread_running) {
    atomic_store_rel(&self->backup_thread_msg, BT_ENTERING_OCAML);
  }

  return;
}

CAMLexport void caml_release_domain_lock(void)
{
  dom_internal* self = domain_self;
  caml_plat_unlock(&self->domain_lock);
  return;
}

CAMLexport void caml_bt_exit_ocaml(void)
{
  dom_internal* self = domain_self;

  Assert(caml_domain_alone() || self->backup_thread_running);

  if (self->backup_thread_running) {
    atomic_store_rel(&self->backup_thread_msg, BT_IN_BLOCKING_SECTION);
    /* Wakeup backup thread if it is sleeping */
    caml_plat_signal(&self->domain_cond);
  }


  return;
}

static void caml_enter_blocking_section_default(void)
{
  caml_bt_exit_ocaml();
  caml_release_domain_lock();
  return;
}

static void caml_leave_blocking_section_default(void)
{
  caml_bt_enter_ocaml();
  caml_acquire_domain_lock();
  return;
}


CAMLexport void (*caml_enter_blocking_section_hook)(void) =
   caml_enter_blocking_section_default;
CAMLexport void (*caml_leave_blocking_section_hook)(void) =
   caml_leave_blocking_section_default;

CAMLexport void caml_leave_blocking_section() {
  caml_leave_blocking_section_hook();
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {

  caml_process_pending_signals();
  caml_enter_blocking_section_hook();
}

/* default handler for unix_fork, will be called by unix_fork. */
static void caml_atfork_default(void) {
  caml_reset_domain_lock();
  caml_acquire_domain_lock();
}

CAMLexport void (*caml_atfork_hook)(void) = caml_atfork_default;

void caml_print_stats () {
  struct gc_stats s;

  caml_gc_stat(Val_unit);
  caml_sample_gc_stats(&s);
  fprintf(stderr,"**** GC stats ****\n");
  fprintf(stderr, "Minor words:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.minor_words);
  fprintf(stderr, "Promoted words:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.promoted_words);
  fprintf(stderr, "Major words:\t\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.major_words);
  fprintf(stderr, "Minor collections:\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    (uintnat)s.minor_collections);
  fprintf(stderr, "Participated minor collections:\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n", (uintnat)s.participated_minor_collections);
  fprintf(stderr, "Major collections:\t%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    Caml_state->stat_major_collections);
}

/* Sending interrupts between domains.

   To avoid deadlock, some rules are important:

   - Don't hold interruptor locks for long
   - Don't hold two interruptor locks at the same time
   - Continue to handle incoming interrupts even when waiting for a response */

/* must be called with s->lock held */
static uintnat handle_incoming(struct interruptor* s)
{
  uintnat handled = 0;
  Assert (s->running);
  while (s->qhead != NULL) {
    struct interrupt* req = s->qhead;
    s->qhead = req->next;
    /* Unlock s while the handler runs, to allow other
       domains to send us messages. This is necessary to
       avoid deadlocks, since the handler might send
       interrupts */
    caml_plat_unlock(&s->lock);

    req->handler(caml_domain_self(), req->data, req);

    caml_plat_lock(&s->lock);
    handled++;
  }
  return handled;
}

void caml_acknowledge_interrupt(struct interrupt* req)
{
  atomic_store_rel(&req->acknowledged, 1);
}

static void acknowledge_all_pending_interrupts()
{
  Assert(Caml_state->critical_section_nesting == 0);
  while (Caml_state->pending_interrupts) {
    interrupt* curr = Caml_state->pending_interrupts;
    Caml_state->pending_interrupts = curr->next;
    caml_acknowledge_interrupt(curr);
  }
}

static void handover_ephemerons(caml_domain_state* domain_state)
{
  if (domain_state->ephe_info->todo == 0 &&
      domain_state->ephe_info->live == 0)
    return;

  caml_add_to_orphaned_ephe_list(domain_state->ephe_info);
  if (domain_state->ephe_info->todo != 0) {
    caml_ephe_todo_list_emptied();
  }
  domain_state->ephe_info->live = 0;
  domain_state->ephe_info->todo = 0;
}

static void handover_finalisers(caml_domain_state* domain_state)
{
  struct caml_final_info* f = domain_state->final_info;

  if (f->todo_head != NULL || f->first.size != 0 || f->last.size != 0) {
    /* have some final structures */
    if (caml_gc_phase != Phase_sweep_and_mark_main) {
      /* Force a major GC to simplify constraints for
      * handing over ephemerons. */
      caml_gc_major(Val_unit);
    }
    caml_add_orphaned_finalisers (f);
    /* Create a dummy final info */
    domain_state->final_info = caml_alloc_final_info();
  }
  caml_final_domain_terminate(domain_state);
}

int caml_domain_is_terminating ()
{
  struct interruptor* s = &domain_self->interruptor;
  return s->terminating;
}

static void domain_terminate()
{
  caml_domain_state* domain_state = domain_self->state.state;
  struct interruptor* s = &domain_self->interruptor;
  int finished = 0;

  caml_gc_log("Domain terminating");
  s->terminating = 1;

  while (!finished) {
    caml_orphan_allocated_words();
    caml_finish_sweeping();

    caml_empty_minor_heaps_once();

    caml_finish_marking();
    handover_ephemerons(domain_state);
    handover_finalisers(domain_state);

    caml_plat_lock(&s->lock);

    /* The interaction of termination and major GC is quite subtle.
     *
     * At the end of the major GC, we decide the number of domains to mark and
     * sweep for the next cycle. If the following [handle_incoming] participates
     * in a major GC cycle, then we need to finish marking and sweeping again in
     * order to decrement the globals [num_domains_to_mark] and
     * [num_domains_to_sweep] (see major_gc.c). Luckily, if the following
     * [handle_incoming] does participate in a major GC cycle, then
     * [Caml_state->sweeping_done] will be set to 0 making conditional check to
     * fail, which forces this domain to finish marking and sweeping again.
     */

    if (handle_incoming(s) == 0 &&
        Caml_state->marking_done &&
        Caml_state->sweeping_done) {

      finished = 1;
      s->terminating = 0;
      s->running = 0;
      s->unique_id += Max_domains;

      /* signal the interruptor condition variable
       * because the backup thread may be waiting on it
       */
      caml_plat_broadcast(&s->cond);
      Assert (domain_self->backup_thread_running);
      domain_self->backup_thread_running = 0;
    }
    caml_plat_unlock(&s->lock);
  }
  caml_sample_gc_collect(domain_state);
  caml_remove_generational_global_root(&domain_state->unique_token_root);
  caml_remove_generational_global_root(&domain_state->dls_root);
  caml_remove_generational_global_root(&domain_state->backtrace_last_exn);

  caml_stat_free(domain_state->final_info);
  // run the domain termination hook
  caml_domain_stop_hook();
  caml_stat_free(domain_state->ephe_info);
  caml_teardown_major_gc();
  CAML_EVENTLOG_TEARDOWN();
  caml_teardown_shared_heap(domain_state->shared_heap);
  domain_state->shared_heap = 0;
  caml_free_minor_tables(domain_state->minor_tables);
  domain_state->minor_tables = 0;
  caml_free_signal_stack();

  if(domain_state->current_stack != NULL) {
    caml_free_stack(domain_state->current_stack);
  }

  if (Caml_state->critical_section_nesting) {
    Caml_state->critical_section_nesting = 0;
    acknowledge_all_pending_interrupts();
  }

  atomic_store_rel(&domain_self->backup_thread_msg, BT_TERMINATE);
  caml_plat_signal(&domain_self->domain_cond);
  caml_plat_unlock(&domain_self->domain_lock);

  caml_plat_assert_all_locks_unlocked();
  /* This is the last thing we do because we need to be able to rely
     on caml_domain_alone (which uses caml_num_domains_running) in at least
     the shared_heap lockfree fast paths */
  atomic_fetch_add(&caml_num_domains_running, -1);
}

int caml_incoming_interrupts_queued()
{
  return domain_self->interruptor.qhead != NULL;
}

static inline void handle_incoming_interrupts(struct interruptor* s, int otherwise_relax)
{
  if (s->qhead != NULL) {
    caml_plat_lock(&s->lock);
    handle_incoming(s);
    caml_plat_unlock(&s->lock);
  } else if (otherwise_relax) {
    cpu_relax();
  }
}

static void handle_incoming_otherwise_relax (struct interruptor* self)
{
  handle_incoming_interrupts(self, 1);
}

void caml_handle_incoming_interrupts()
{
  handle_incoming_interrupts(&domain_self->interruptor, 0);
}

static void caml_wait_interrupt_acknowledged (struct interruptor* self,
                                           struct interrupt* req)
{
  int i;

  /* Often, interrupt handlers are fast, so spin for a bit before waiting */
  for (i=0; i<1000; i++) {
    if (atomic_load_acq(&req->acknowledged)) {
      return;
    }
    cpu_relax();
  }

  {
    SPIN_WAIT {
      if (atomic_load_acq(&req->acknowledged))
        return;
      handle_incoming_otherwise_relax(self);
    }
  }

  return;
}

int caml_send_partial_interrupt(
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data,
                         struct interrupt* req)
{
  req->handler = handler;
  req->data = data;
  atomic_store_rel(&req->acknowledged, 0);
  req->next = NULL;

  caml_plat_lock(&target->lock);
  if (!target->running) {
    caml_plat_unlock(&target->lock);
    return 0;
  }

  /* add to wait queue */
  if (target->qhead) {
    /* queue was nonempty */
    target->qtail->next = req;
    target->qtail = req;
  } else {
    /* queue was empty */
    target->qhead = target->qtail = req;
  }
  /* Signal the condition variable, in case the target is
     itself waiting for an interrupt to be processed elsewhere */
  caml_plat_broadcast(&target->cond); // OPT before/after unlock? elide?
  caml_plat_unlock(&target->lock);

  atomic_store_rel(target->interrupt_word, INTERRUPT_MAGIC); //FIXME dup

  return 1;
}

int caml_send_interrupt(struct interruptor* self,
                         struct interruptor* target,
                         domain_rpc_handler handler,
                         void* data)
{
  struct interrupt req;
  if (!caml_send_partial_interrupt(target, handler, data, &req))
    return 0;
  caml_wait_interrupt_acknowledged(self, &req);
  return 1;
}


CAMLprim value caml_ml_domain_critical_section(value delta)
{
  intnat crit = Caml_state->critical_section_nesting + Long_val(delta);
  Caml_state->critical_section_nesting = crit;
  if (crit < 0) {
    caml_fatal_error("invalid critical section nesting");
  } else if (crit == 0) {
    acknowledge_all_pending_interrupts();
  }
  return Val_unit;
}

#define Chunk_size 0x400

CAMLprim value caml_ml_domain_yield(value unused)
{
  struct interruptor* s = &domain_self->interruptor;
  int found_work = 1;
  intnat left;

  if (Caml_state->critical_section_nesting == 0) {
    caml_failwith("Domain.Sync.wait must be called from within a critical section");
  }

  caml_plat_lock(&s->lock);
  while (!Caml_state->pending_interrupts) {
    if (handle_incoming(s) == 0 && !found_work) {
      CAML_EV_BEGIN(EV_DOMAIN_IDLE_WAIT);
      caml_plat_wait(&s->cond);
      CAML_EV_END(EV_DOMAIN_IDLE_WAIT);
    } else {
      caml_plat_unlock(&s->lock);
      left = caml_opportunistic_major_collection_slice(Chunk_size);
      if (left == Chunk_size)
        found_work = 0;
      caml_plat_lock(&s->lock);
    }
  }
  caml_plat_unlock(&s->lock);

  return Val_unit;
}

static void handle_ml_interrupt(struct domain* d, void* unique_id_p, interrupt* req)
{
  if (d->internals->interruptor.unique_id != *(uintnat*)unique_id_p) {
    caml_acknowledge_interrupt(req);
    return;
  }
  if (d->state->critical_section_nesting > 0) {
    req->next = d->state->pending_interrupts;
    d->state->pending_interrupts = req;
  } else {
    caml_acknowledge_interrupt(req);
  }
}

CAMLprim value caml_ml_domain_interrupt(value domain)
{
  CAMLparam1 (domain);
  uintnat unique_id = (uintnat)Long_val(domain);
  struct interruptor* target =
    &all_domains[unique_id % Max_domains].interruptor;

  CAML_EV_BEGIN(EV_DOMAIN_SEND_INTERRUPT);
  if (!caml_send_interrupt(&domain_self->interruptor, target, &handle_ml_interrupt, &unique_id)) {
    /* the domain might have terminated, but that's fine */
  }
  CAML_EV_END(EV_DOMAIN_SEND_INTERRUPT);

  CAMLreturn (Val_unit);
}

CAMLprim int64_t caml_ml_domain_ticks_unboxed(value unused)
{
  return caml_time_counter() - startup_timestamp;
}

CAMLprim value caml_ml_domain_ticks(value unused)
{
  return caml_copy_int64(caml_ml_domain_ticks_unboxed(unused));
}

CAMLprim value caml_ml_domain_yield_until(value t)
{
  int64_t ts = Int64_val(t) + startup_timestamp;
  struct interruptor* s = &domain_self->interruptor;
  value ret = Val_int(1); /* Domain.Sync.Notify */
  int res;
  intnat left;
  int found_work = 1;

  if (Caml_state->critical_section_nesting == 0){
    caml_failwith("Domain.Sync.wait_until must be called from within a critical section");
  }

  caml_plat_lock(&s->lock);

  while (!Caml_state->pending_interrupts) {
    if (ts < caml_time_counter ()) {
      ret = Val_int(0); /* Domain.Sync.Timeout */
      break;
    } else if (handle_incoming(s) == 0 && !found_work) {
      CAML_EV_BEGIN(EV_DOMAIN_IDLE_WAIT);
      res = caml_plat_timedwait(&s->cond, ts);
      CAML_EV_END(EV_DOMAIN_IDLE_WAIT);
      if (res) {
        ret = Val_int(0); /* Domain.Sync.Timeout */
        break;
      }
    } else {
      caml_plat_unlock(&s->lock);
      left = caml_opportunistic_major_collection_slice(Chunk_size);
      if (left == Chunk_size)
        found_work = 0;
      caml_plat_lock(&s->lock);
    }
  }

  caml_plat_unlock(&s->lock);

  return ret;
}

CAMLprim value caml_ml_domain_cpu_relax(value t)
{
  struct interruptor* self = &domain_self->interruptor;
  handle_incoming_otherwise_relax (self);
  return Val_unit;
}

CAMLprim value caml_domain_dls_set(value t)
{
  CAMLnoalloc;
  caml_modify_generational_global_root(&Caml_state->dls_root, t);
  return Val_unit;
}

CAMLprim value caml_domain_dls_get(value unused)
{
  CAMLnoalloc;
  return Caml_state->dls_root;
}
