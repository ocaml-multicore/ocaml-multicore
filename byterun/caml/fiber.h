#ifndef CAML_FIBER_H
#define CAML_FIBER_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "roots.h"
#include "stack.h"


#define Stack_dirty_domain(stk) (fiber_link(stk)->dirty_domain)
#define Stack_handle_value(stk) (fiber_link(stk)->handle_val)
#define Stack_handle_exception(stk) (fiber_link(stk)->handle_exn)
#define Stack_handle_effect(stk) (fiber_link(stk)->handle_eff)
#define Stack_parent(stk) (fiber_link(stk)->parent)
#define Stack_sp(stk) (fiber_link(stk)->sp)
#define Stack_high(stk) ((value*)fiber_link(stk))

/* States for Stack_dirty_domain field */
/* A clean fiber does not have pointers into any minor heaps */
#define FIBER_CLEAN ((struct domain*)0)
/* A clean fiber is being scanned by a GC thread */
#define FIBER_SCANNING ((struct domain*)1)

#ifdef NATIVE_CODE

/* Stack layout for native code. Stack grows downwards.
 *
 * +------------------------+ <--- Stack_high
 * | caml_fiber_exn_handler |
 * +------------------------+
 * |    debugger_slot       |
 * +------------------------+
 * | caml_fiber_val_handler |
 * +------------------------+
 * |      caml_handler      |
 * +------------------------+
 * |                        |
 * .      OCaml frames      . <--- sp
 * |                        |
 * +------------------------+ <--- Stack_threshold
 * |                        |
 * .      Slop space        .
 * |                        |
 * +------------------------+ <--- Stack_base
 * |        parent          |
 * +------------------------+
 * |  handle effect closure |
 * +------------------------+
 * |   handle exn closure   |
 * +------------------------+
 * |   handle val closure   |
 * +------------------------+
 * |      dirty domain      |
 * +------------------------+
 * | Offset of sp from high |
 * +------------------------+ <--- stack object
 * |      HEADER WORD       |
 * +------------------------+
 */
#endif


value caml_find_performer(value stack);

/* The table of global identifiers */
extern caml_root caml_global_data;

#define Trap_pc(tp) ((tp)[0])
#define Trap_link(tp) ((tp)[1])

void caml_init_stack(value stack);
value caml_alloc_main_stack (uintnat init_size);
void* caml_init_main_stack(void);
void caml_scan_dirty_stack_domain(scanning_action f, void*, value stack,
                                  struct domain* domain);
void caml_scan_stack(scanning_action, void*, value stack);
void caml_save_stack_gc();
void caml_restore_stack_gc();
void caml_restore_stack();
void caml_clean_stack(value stack);
void caml_clean_stack_domain(value stack, struct domain* domain);
void caml_realloc_stack (asize_t required_size, value* save, int nsave);
void caml_change_max_stack_size (uintnat new_max_size);
void caml_maybe_expand_stack();
int  caml_on_current_stack(value*);
int  caml_running_main_fiber();
#ifdef NATIVE_CODE
int caml_switch_stack(value stk);
#else
value caml_switch_stack(value stk);
#endif
value caml_fiber_death();
void caml_darken_stack(value stack);
value caml_reverse_fiber_stack(value stack);

#ifdef NATIVE_CODE
void caml_get_stack_sp_pc (value stack, char** sp /* out */, uintnat* pc /* out */);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_FIBER_H */
