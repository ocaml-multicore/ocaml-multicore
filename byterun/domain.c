#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include "caml/domain.h"
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

/* Since we support both heavyweight OS threads and lightweight
   userspace threads, the word "thread" is ambiguous. This file deals
   with OS-level threads, called "domains".
*/


struct dom_internal {
  /* readonly fields, initialised and never modified */
  atomic_uintnat* interrupt_word_address;
  struct domain state;

  /* fields accessed by the domain itself and the domain requesting an RPC */
  atomic_uintnat rpc_request;
  domain_rpc_handler rpc_handler;
  void* rpc_data;
  atomic_uintnat* rpc_completion_signal;


  caml_plat_mutex roots_lock;

  /* fields protected by all_domains_lock */
  int running;

  /* readonly */
  uintnat tls_area;
  uintnat tls_area_end;
  uintnat minor_heap_area;
  uintnat minor_heap_area_end;
};
typedef struct dom_internal dom_internal;


/* possible values of domain->rpc_request */
enum { RPC_IDLE = 0,
       RPC_REQUEST_INITIALISING = 1,
       RPC_REQUEST_SENT = 2 };

#define Max_domains (1 << Minor_heap_sel_bits)


static caml_plat_mutex all_domains_lock;
static struct dom_internal all_domains[Max_domains];
static uintnat minor_heaps_base;
static __thread dom_internal* domain_self;

CAMLexport __thread struct caml_domain_state* caml_domain_state;

static __thread char domains_locked[Max_domains];

/* If you lock a domain, you are responsible for handling
   its incoming RPC requests */
static int try_lock_domain(dom_internal* target) {
  Assert(domains_locked[target->state.id] == 0);
  if (caml_plat_try_lock(&target->roots_lock)) {
    domains_locked[target->state.id] = 1;
    return 1;
  } else {
    return 0;
  }
}

static int domain_is_locked(dom_internal* target) {
  return domains_locked[target->state.id];
}

static void unlock_domain(dom_internal* target) {
  Assert(domains_locked[target->state.id] == 1);
  domains_locked[target->state.id] = 0;
  caml_plat_unlock(&target->roots_lock);
}


asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t page_size = caml_mem_round_up_pages(1);
  asize_t bs, max;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = caml_mem_round_up_pages(Bsize_wsize (wsize));

  Assert(page_size * 2 < (1 << Minor_heap_align_bits));
  max = (1 << Minor_heap_align_bits) - page_size * 2;

  if (bs > max) bs = max;

  return bs;
}

void caml_reallocate_minor_heap(asize_t size)
{
  Assert(caml_domain_state->young_ptr == caml_domain_state->young_end);

  /* free old minor heap.
     instead of unmapping the heap, we decommit it, so there's
     no race whereby other code could attempt to reuse the memory. */
  caml_mem_decommit((void*)domain_self->minor_heap_area,
                    domain_self->minor_heap_area_end - domain_self->minor_heap_area);

  size = caml_norm_minor_heap_size(size);

  caml_mem_commit((void*)domain_self->minor_heap_area, size);

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)domain_self->minor_heap_area;
    for (; p < (uintnat*)(domain_self->minor_heap_area + size); p++)
      *p = Debug_uninit_align;
  }
#endif

  caml_minor_heap_size = size;
  caml_domain_state->young_start = (char*)domain_self->minor_heap_area;
  caml_domain_state->young_end = (char*)(domain_self->minor_heap_area + size);
  caml_domain_state->young_ptr = caml_domain_state->young_end;
}

/* must be run on the domain's thread */
static void create_domain(uintnat initial_minor_heap_size, int is_main) {
  int i;
  dom_internal* d = 0;
  Assert (domain_self == 0);

  caml_plat_lock(&all_domains_lock);

  for (i = 0; i < Max_domains; i++) {
    if (!all_domains[i].running) {
      d = &all_domains[i];
      break;
    }
  }

  if (d) {
    d->running = 1;
    d->state.is_main = is_main;
    d->state.vm_inited = 0;
    d->state.internals = d;
    /* FIXME: shutdown RPC? */
    atomic_store_rel(&d->rpc_request, RPC_IDLE);

    domain_self = d;
    caml_domain_state = (void*)(d->tls_area);
    caml_plat_lock(&d->roots_lock);

    if (!d->interrupt_word_address) {
      caml_mem_commit((void*)d->tls_area, (d->tls_area_end - d->tls_area));
      atomic_uintnat* young_limit = (atomic_uintnat*)&caml_domain_state->young_limit;
      d->interrupt_word_address = young_limit;
      atomic_store_rel(young_limit, d->minor_heap_area);
    }
    caml_domain_state->young_start = caml_domain_state->young_end =
      caml_domain_state->young_ptr = 0;

    d->state.shared_heap = caml_init_shared_heap();
    caml_init_major_gc();
    caml_reallocate_minor_heap(initial_minor_heap_size);

    caml_init_main_stack();

    d->state.remembered_set = &caml_remembered_set;
    d->state.local_roots = &caml_local_roots;
#ifdef NATIVE_CODE
    /* FIXME */
#else
    d->state.current_stack = &caml_current_stack;
#endif
    d->state.state = caml_domain_state;
    d->state.mark_stack = &caml_mark_stack;
    d->state.mark_stack_count = &caml_mark_stack_count;
    d->state.vm_inited = 1;
  }
  caml_plat_unlock(&all_domains_lock);
}


void caml_init_domains(uintnat minor_size) {
  int i;
  uintnat size;
  void* heaps_base;

  /* sanity check configuration */
  if (caml_mem_round_up_pages(1 << Minor_heap_align_bits) != (1 << Minor_heap_align_bits))
    caml_fatal_error("Minor_heap_align_bits misconfigured for this platform");

  /* reserve memory space for minor heaps */
  size = (uintnat)1 << (Minor_heap_sel_bits + Minor_heap_align_bits);

  /* To ensure Is_foreign gives no false positives, we reserve twice
     the address space needed and only use the first half */
  heaps_base = caml_mem_map(size*2, size*2, 1 /* reserve_only */);
  if (!heaps_base) caml_raise_out_of_memory();

  minor_heaps_base = (uintnat) heaps_base;

  caml_plat_mutex_init(&all_domains_lock);

  for (i = 0; i < Max_domains; i++) {
    struct dom_internal* dom = &all_domains[i];
    caml_plat_mutex_init(&dom->roots_lock);
    dom->running = 0;
    dom->state.id = i;

    dom->tls_area = minor_heaps_base +
      (uintnat)(1 << Minor_heap_align_bits) * (uintnat)i;
    dom->tls_area_end =
      caml_mem_round_up_pages(dom->tls_area +
                              sizeof(struct caml_domain_state));
    dom->minor_heap_area = /* skip guard page */
      caml_mem_round_up_pages(dom->tls_area_end + 1);
    dom->minor_heap_area_end =
      dom->minor_heap_area + (1 << Minor_heap_align_bits);
  }


  create_domain(minor_size, 1);
  if (!domain_self) caml_fatal_error("Failed to create main domain");

  caml_init_global_roots();
  caml_init_signal_handling();
}


static void domain_terminate() {
  caml_gc_log("Domain terminating");
  caml_enter_blocking_section();

  /* FIXME: proper domain termination and reuse */
  /* interrupt_word_address must not go away */
  pause();
}

struct domain_startup_params {
  caml_plat_event ev;
  value callback;
  dom_internal* newdom;
};

static void* domain_thread_func(void* v) {
  CAMLparam0();
  CAMLlocal1(callback);
  struct domain_startup_params* p = v;

  callback = p->callback;
  create_domain(caml_startup_params.minor_heap_init, 0);
  p->newdom = domain_self;
  caml_plat_event_trigger(&p->ev);

  if (domain_self) {
    caml_gc_log("Domain starting");
    caml_callback(callback, Val_unit);
    domain_terminate();
  } else {
    caml_gc_log("Failed to create domain");
  }
  CAMLreturnT(void*, 0);
}


CAMLprim value caml_domain_spawn(value callback)
{
  CAMLparam1 (callback);
  struct domain_startup_params p;
  pthread_t th;
  int err;

  caml_plat_event_init(&p.ev);
  p.callback = caml_promote(&domain_self->state, callback);

  err = pthread_create(&th, 0, domain_thread_func, (void*)&p);
  if (err) {
    caml_failwith("failed to create domain thread");
  }

  caml_enter_blocking_section();
  caml_plat_event_wait(&p.ev);
  caml_leave_blocking_section();

  if (p.newdom) {
    /* successfully created a domain */
    pthread_detach(th);
  } else {
    /* failed */
    void* r;
    pthread_join(th, &r);
    caml_failwith("failed to allocate domain");
  }

  CAMLreturn (Val_unit);
}

struct domain* caml_domain_self()
{
  return domain_self ? &domain_self->state : 0;
}

struct domain* caml_random_domain()
{
  dom_internal* d;
  int i, r = rand(), len = 0;

  caml_plat_lock(&all_domains_lock);
  for (i = 0; i < Max_domains; i++)
    if (all_domains[i].running) len++;
  r %= len;
  for (i = 0; r > 0; i++)
    if (all_domains[i].running) r--;
  d = &all_domains[i];
  Assert( 0 <= i && i < Max_domains && d->running);
  caml_plat_unlock(&all_domains_lock);

  return &d->state;
}

struct domain* caml_owner_of_young_block(value v) {
  Assert(Is_minor(v));
  int heap_id = ((uintnat)v - minor_heaps_base) /
    (1 << Minor_heap_align_bits);
  return &all_domains[heap_id].state;
}

struct domain* caml_domain_of_id(int id)
{
  return &all_domains[id].state;
}

CAMLprim value caml_ml_domain_id(value unit)
{
  return Val_int(domain_self->state.id);
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1);
static __thread int volatile caml_force_major_slice = 0;

static atomic_uintnat stw_requested;


static void interrupt_domain(dom_internal* d) {
  atomic_store_rel(d->interrupt_word_address, INTERRUPT_MAGIC);
}

static void request_stw() {
  int i;
  atomic_store_rel(&stw_requested, 1);
  /* interrupt all running domains */
  caml_plat_lock(&all_domains_lock);
  for (i = 0; i < Max_domains; i++) {
    if (all_domains[i].running && all_domains[i].interrupt_word_address)
      interrupt_domain(&all_domains[i]);
  }
  caml_plat_unlock(&all_domains_lock);
}


void caml_trigger_stw_gc() {
  request_stw();
}

void caml_interrupt_self() {
  interrupt_domain(domain_self);
}

/* Arrange for a garbage collection to be performed on the current domain
   as soon as possible */
void caml_urge_major_slice (void)
{
  caml_force_major_slice = 1;
  caml_interrupt_self();
}


static void stw_phase(void);
static int check_rpc(void);

void caml_handle_gc_interrupt() {
  atomic_uintnat* young_limit = domain_self->interrupt_word_address;
  if (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
    /* interrupt */
    while (atomic_load_acq(young_limit) == INTERRUPT_MAGIC) {
      atomic_cas(young_limit, INTERRUPT_MAGIC, domain_self->minor_heap_area);
    }
    check_rpc();
    if (atomic_load_acq(&stw_requested)) {
      stw_phase();
    }
  }

  if (((uintnat)caml_domain_state->young_ptr - Bhsize_wosize(Max_young_wosize) <
       domain_self->minor_heap_area) ||
      caml_force_major_slice) {
    /* out of minor heap or collection forced */
    caml_force_major_slice = 0;
    caml_minor_collection();
  }
}

CAMLexport void caml_leave_blocking_section() {
  caml_plat_lock(&domain_self->roots_lock);
  caml_restore_stack_gc();
  caml_process_pending_signals();
}

CAMLexport void caml_enter_blocking_section() {
  caml_process_pending_signals();
  caml_save_stack_gc();
  caml_plat_unlock(&domain_self->roots_lock);
}


static atomic_uintnat heaps_marked;
static atomic_uintnat domain_accounted_for[Max_domains];

static void stw_phase () {
  int i;
  int my_heaps = 0;
  char inactive_domains_locked[Max_domains] = {0};

  /* First, make sure all domains are accounted for. */
  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    SPIN_WAIT {
      if (atomic_load_acq(&domain_accounted_for[i]))
        break;
      /* not accounted for yet */
      int mine = (d == domain_self) || domain_is_locked(d);
      if (!mine && try_lock_domain(d)) {
        /* mine now! */
        inactive_domains_locked[i] = 1;
        mine = 1;
      }
      if (mine) {
        /* accounted for by current thread */
        atomic_store_rel(&domain_accounted_for[i], 1);
        my_heaps++;
      } else {
        /* locked by some other thread,
           but not yet accounted for, need to wait */
        check_rpc();
      }
    }
  }

  caml_gc_log("Contributing %d heaps to GC", my_heaps);

  /* Finish GC on the domains we've locked */

  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if (!d->state.shared_heap)
      continue; /* skip non-running domains */

    if (d == domain_self) {
      /* finish GC */
      while (caml_sweep(d->state.shared_heap, 10) <= 0);
      caml_empty_minor_heap();
      caml_finish_marking();
    } else if (domain_is_locked(d)) {
      /* GC some inactive domain that we locked */
      caml_gc_log("GCing inactive domain [%02d]", d->state.id);
      while (caml_sweep(d->state.shared_heap, 10) <= 0);
      caml_do_sampled_roots(&caml_darken, &d->state);
      caml_empty_mark_stack();
    }
  }

  /* Wait until all threads finish GC */

  if (atomic_fetch_add(&heaps_marked, (uintnat)my_heaps)
      + (uintnat)my_heaps == Max_domains) {
    /* we marked the last heap, so all other threads are waiting */
    //caml_cleanup_deleted_roots();

    caml_cycle_heap_stw();
    caml_gc_log("GC cycle completed (heap cycled)");

    /* reset for next GC */
    for (i = 0; i < Max_domains; i++) {
      atomic_store_rel(&domain_accounted_for[i], 0);
    }
    atomic_store_rel(&stw_requested, 0);

    /* allow other threads to proceed */
    atomic_store_rel(&heaps_marked, 0);
  } else {
    /* we didn't mark the last heap, so wait */
    SPIN_WAIT {
      if (atomic_load_acq(&heaps_marked) == 0)
        break;
      Assert(atomic_load_acq(&heaps_marked) <= Max_domains);
      check_rpc();
    }
    caml_gc_log("GC cycle completed");
  }

  /* Finally, start the next sweeping cycle and
     unlock any inactive domains we locked for GC */

  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if ((d == domain_self || domain_is_locked(d)) && d->state.shared_heap)
      caml_cycle_heap(d->state.shared_heap);

    if (inactive_domains_locked[i])
      unlock_domain(d);
  }
}


static int handle_rpc(dom_internal* target)
{
  int r = 0;
  if (atomic_load_acq(&target->rpc_request) != RPC_IDLE) {

    /* wait until we know what the request is */
    SPIN_WAIT {
      if (atomic_load_acq(&target->rpc_request) != RPC_REQUEST_INITIALISING) break;
    }

    Assert(atomic_load_acq(&target->rpc_request) == RPC_REQUEST_SENT);

    {
      domain_rpc_handler rpc_handler = target->rpc_handler;
      void* rpc_data = target->rpc_data;
      atomic_uintnat* rpc_completion_signal = target->rpc_completion_signal;

      /* we have a copy of the request, it is now safe for other domains to overwrite it */
      atomic_store_rel(&target->rpc_request, RPC_IDLE);
      /* handle the request */
      rpc_handler(&target->state, rpc_data);
      /* signal completion */
      atomic_store_rel(rpc_completion_signal, 1);
    }
    r = 1;
  }
  return r;
}

/* Handle incoming RPC requests for the current domain,
   and any domains we have temporarily taken over.
   Returns nonzero if any RPC requests were handled. */
static int check_rpc()
{
  int i, res = 0;
  res |= handle_rpc(domain_self);
  for (i = 0; i < Max_domains; i++) {
    dom_internal* d = &all_domains[i];
    if (domain_is_locked(d))
      res |= handle_rpc(d);
  }
  return res;
}

static int attempt_rpc_takeover(dom_internal* target) {
  int res;
  if (try_lock_domain(target)) {
    /* process pending RPC for this domain */
    res = check_rpc();
    unlock_domain(target);
  } else {
    res = check_rpc();
  }
  return res;
}


/* may be called from a non-safepoint. must not GC.
   message will be processed during an RPC safepoint in the target
   domain (or while holding the target domain's roots_lock).
   target domain must not be current domain */
CAMLexport void caml_domain_rpc(struct domain* domain,
                                domain_rpc_handler handler, void* data)
{
  atomic_uintnat completed = ATOMIC_UINTNAT_INIT(0);
  dom_internal* target = domain->internals;

  if (target == domain_self || domain_is_locked(target)) {
    /* well that was easy */
    handler(&target->state, data);
    return;
  }

  /* Wait until we can send an RPC to the target.
     Need to keep handling incoming RPCs while waiting.
     The target may have deactivated, so try taking it over */
  while (!(atomic_load_acq(&target->rpc_request) == RPC_IDLE &&
           atomic_cas(&target->rpc_request, RPC_IDLE, RPC_REQUEST_INITIALISING))) {
    /* exit the SPIN_WAIT loop when anything happens,
       so that backoff time does not increase */
    SPIN_WAIT {
      if (atomic_load_acq(&target->rpc_request) == RPC_IDLE) break;
      if (attempt_rpc_takeover(target)) break;
    }
  }

  /* Initialise and send the request */
  target->rpc_handler = handler;
  target->rpc_data = data;
  target->rpc_completion_signal = &completed;
  atomic_store_rel(&target->rpc_request, RPC_REQUEST_SENT);
  interrupt_domain(target);

  /* Wait for a response */
  while (!atomic_load_acq(&completed)) {
    /* exit the SPIN_WAIT loop when anything happens,
       so that backoff time does not increase */
    SPIN_WAIT {
      if (atomic_load_acq(&completed)) break;
      if (attempt_rpc_takeover(target)) break;
    }
  }
}
