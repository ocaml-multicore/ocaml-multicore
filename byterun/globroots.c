/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Registration of global memory roots */

#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "globroots.h"
#include "callback.h"
#include "platform.h"
#include "alloc.h"
#ifdef NATIVE_CODE
#include "stack.h"
#endif

/* A caml_root is in fact a value. We don't expose that fact outside
   of this file so that C code doesn't attempt to directly modify it.
   The value points to a block on the shared heap with the following
   fields:

    0: the actual root, as set by caml_modify_root
    1: an integer flag stating whether this root has been deleted
    2: the next root in roots_all

   The roots are not scanned during minor GC. Instead, since the root
   blocks are all on the shared heap, pointers from roots to a minor
   heap will be detected using the normal inter-generational pointer
   mechanism. */

static caml_plat_mutex roots_mutex;
static value roots_all = Val_unit;


void caml_init_global_roots()
{
  caml_plat_mutex_init(&roots_mutex);
}

CAMLexport caml_root caml_create_root(value init)
{
  CAMLparam1(init);
  value v = caml_alloc_shr(3, 0);
  caml_initialize_field(v, 0, init);
  caml_initialize_field(v, 1, Val_int(1));

  caml_plat_lock(&roots_mutex);
  caml_initialize_field(v, 2, roots_all);
  roots_all = v;
  caml_plat_unlock(&roots_mutex);

  CAMLreturnT(caml_root, (caml_root)v);
}

CAMLexport void caml_delete_root(caml_root root)
{
  value v = (value)root;
  Assert(root);
  /* the root will be removed from roots_all and freed at the next GC */
  caml_modify_field(v, 0, Val_unit);
  caml_modify_field(v, 1, Val_int(0));
}

CAMLexport value caml_read_root(caml_root root)
{
  value v = (value)root;
  Assert(root);
  Assert(Hd_val(root));
  Assert(Field(v,1) == Val_int(0) || Field(v,1) == Val_int(1));
  return Field(v, 0);
}

CAMLexport void caml_modify_root(caml_root root, value newv)
{
  value v = (value)root;
  Assert(root);
  caml_modify_field(v, 0, newv);
}

static void scan_global_roots(scanning_action f)
{
  value r, newr;
  caml_plat_lock(&roots_mutex);
  r = roots_all;
  caml_plat_unlock(&roots_mutex);

  Assert(!Is_minor(r));
  newr = r;
  f(newr, &newr);
  Assert(r == newr); /* GC should not move r, it is not young */
}

void caml_cleanup_deleted_roots()
{
  value r, prev;
  int first = 1;
  caml_plat_lock(&roots_mutex);

  r = roots_all;
  while (Is_block(r)) {
    value next = Field(r, 2);
    if (Field(r, 1) == Val_int(0)) {
      /* root was deleted, remove from list */
      if (first) {
        roots_all = next;
      } else {
        caml_modify_field(prev, 2, next);
      }
    }

    prev = r;
    first = 0;
    r = next;
  }

  caml_plat_unlock(&roots_mutex);
}

#ifdef NATIVE_CODE

/* Linked-list of natdynlink'd globals */

typedef struct link {
  void *data;
  struct link *next;
} link;

static link *cons(void *data, link *tl) {
  link *lnk = caml_stat_alloc(sizeof(link));
  lnk->data = data;
  lnk->next = tl;
  return lnk;
}

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)


/* protected by roots_mutex */
static link * caml_dyn_globals = NULL;

void caml_register_dyn_global(void *v) {
  caml_plat_lock(&roots_mutex);
  caml_dyn_globals = cons((void*) v,caml_dyn_globals);
  caml_plat_unlock(&roots_mutex);
}

static void scan_native_globals(scanning_action f)
{
  int i, j;
  static link* dyn_globals;
  value glob;
  link* lnk;

  caml_plat_lock(&roots_mutex);
  dyn_globals = caml_dyn_globals;
  caml_plat_unlock(&roots_mutex);

  /* The global roots */
  for (i = 0; caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++)
      f (Op_val(glob)[j], &Op_val(glob)[j]);
  }

  /* Dynamic (natdynlink) global roots */
  iter_list(dyn_globals, lnk) {
    glob = (value) lnk->data;
    for (j = 0; j < Wosize_val(glob); j++){
      f (Op_val(glob)[j], &Op_val(glob)[j]);
    }
  }
}

#endif

void caml_scan_global_roots(scanning_action f) {
  scan_global_roots(f);
#ifdef NATIVE_CODE
  scan_native_globals(f);
#endif
}
