/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* To walk the memory roots for garbage collection */

#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/fiber.h"
#include "caml/shared_heap.h"

#ifdef NATIVE_CODE
/* Communication with [caml_start_program] and [caml_call_gc]. */

/* FIXME: there should be one of these per domain */
intnat caml_globals_inited = 0;
static intnat caml_globals_scanned = 0;

#endif

CAMLexport __thread struct caml__roots_block *caml_local_roots = NULL;

CAMLexport void caml_do_local_roots (scanning_action f, struct domain* domain)
{
  struct caml__roots_block *lr;
  int i, j;
  value* sp;

#ifdef NATIVE_CODE
  struct caml_domain_state* st = domain->state;
  caml_scan_stack_roots(f, st->bottom_of_stack,
                        st->last_return_address, st->gc_regs);
#else
  f(*(domain->current_stack), domain->current_stack);
#endif
  for (lr = *(domain->local_roots); lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          f (*sp, sp);
        }
      }
    }
  }
}

void caml_do_sampled_roots(scanning_action f, struct domain* domain)
{
  /* look for roots on the minor heap */
  value* p = (value*)(domain->state->young_ptr);
  value* end = (value*)(domain->state->young_end);
  while (p < end) {
    value v = Val_hp(p);
    Assert (Is_block(v) && Wosize_val(v) <= Max_young_wosize);
    if (Tag_val(v) == Stack_tag) {
      caml_scan_stack(f, v);
    } else if (Tag_val(v) < No_scan_tag) {
      int i;
      value* fields = Op_val(v);
      for (i = 0; i < Wosize_val(v); i++) {
        if (Is_block(fields[i]) && !Is_minor(fields[i])) f(fields[i], &fields[i]);
      }
    }
    p += Whsize_wosize(Wosize_val(v));
  }
  Assert(p == end);

  /* look for gray values in the mark stack */
  value* mark_stack = *domain->mark_stack;
  value* mark_stack_end = *domain->mark_stack + *domain->mark_stack_count;
  for (p = mark_stack; p < mark_stack_end; p++) {
    value v = *p;
    Assert (Is_block(v));
    f(v, p);
    if (Tag_val(v) == Stack_tag) {
      caml_scan_stack(f, v);
    } else if (Tag_val(v) < No_scan_tag) {
      int i;
      value* fields = Op_val(v);
      Assert(Tag_val(v) != Infix_tag); /* Infix_tag can't appear on mark stack */
      for (i = 0; i < Wosize_val(v); i++) {
        if (Is_block(fields[i]) && !Is_minor(fields[i])) f(fields[i], &fields[i]);
      }
    }
  }
  /* these values need no longer be grayed by the target domain */
  *domain->mark_stack_count = 0;

  /* treat the remembered sets as roots */
  struct caml_ref_entry* r;
  for (r = domain->remembered_set->ref.base; r < domain->remembered_set->ref.ptr; r++) {
    f(r->obj, 0);
  }
  for (r = domain->remembered_set->fiber_ref.base; r < domain->remembered_set->fiber_ref.ptr; r++) {
    f(r->obj, 0);
    caml_scan_stack(f, r->obj);
  }


  /* look for local C and stack roots */
  caml_do_local_roots(f, domain);
}
