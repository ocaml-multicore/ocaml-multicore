#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#include "mlvalues.h"
#include "domain_state.h"
#include "memory.h"

struct domain {
  int id;
  int is_main;
  int vm_inited;

  struct dom_internal* internals;
  struct caml_heap_state* shared_heap;
  struct caml_remembered_set* remembered_set;
  struct caml__roots_block** local_roots;
  struct caml_domain_state* state;
  value** mark_stack;
  int* mark_stack_count;
};

#define Caml_check_gc_interrupt() \
  ((uintnat)caml_domain_state->young_ptr < caml_domain_state->young_limit)

asize_t caml_norm_minor_heap_size (intnat);
void caml_reallocate_minor_heap(asize_t);

void caml_handle_gc_interrupt(void);

void caml_trigger_stw_gc(void);

void caml_urge_major_slice (void);

void caml_interrupt_self(void);


CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

void caml_init_domains(uintnat minor_heap_size);


struct domain* caml_domain_self();

typedef void (*domain_rpc_handler)(struct domain*, void*);

struct domain* caml_random_domain();

struct domain* caml_owner_of_young_block(value);

struct domain* caml_domain_of_id(int);

void caml_domain_rpc(struct domain*,
                     domain_rpc_handler, void*);

#endif /* CAML_DOMAIN_H */
