/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <string.h>

#include "config.h"
#include "fail.h"
#include "finalise.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "weak.h"
#include "domain.h"
#include "shared_heap.h"
#include "addrmap.h"
#include "fiber.h"
#include "eventlog.h"

asize_t __thread caml_minor_heap_size;

CAMLexport __thread struct caml_remembered_set caml_remembered_set;

#ifdef DEBUG
static __thread unsigned long minor_gc_counter = 0;
#endif

void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv)
{
  tbl->size = sz;
  tbl->reserve = rsv;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = (value**) caml_stat_alloc ((tbl->size + tbl->reserve)
                                         * sizeof (value*));
  tbl->ptr = tbl->base;
  tbl->threshold = tbl->base + tbl->size;
  tbl->limit = tbl->threshold;
  tbl->end = tbl->base + tbl->size + tbl->reserve;
}

static void reset_table (struct caml_ref_table *tbl)
{
  tbl->size = 0;
  tbl->reserve = 0;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = tbl->ptr = tbl->threshold = tbl->limit = tbl->end = NULL;
}

static void clear_table (struct caml_ref_table *tbl)
{
    tbl->ptr = tbl->base;
    tbl->limit = tbl->threshold;
}

/* size in bytes */
void caml_set_minor_heap_size (asize_t size)
{
  if (caml_domain_state->young_ptr != caml_domain_state->young_end) caml_minor_collection ();

  caml_reallocate_minor_heap(size);

  reset_table (&caml_remembered_set.major_ref);
  reset_table (&caml_remembered_set.minor_ref);
}

//*****************************************************************************

static __thread value oldify_todo_list = 0;
static __thread uintnat stat_live_bytes = 0;

/* Promotion input and output variables. */
static __thread struct domain* promote_domain = 0;
static __thread value oldest_promoted = 0;

static value alloc_shared(mlsize_t wosize, tag_t tag)
{
  void* mem = caml_shared_try_alloc(caml_domain_self()->shared_heap, wosize, tag, 0 /* not promotion */);
  caml_allocated_words += Whsize_wosize(wosize);
  if (mem == NULL) {
    caml_fatal_error("allocation failure during minor GC");
  }
  return Val_hp(mem);
}

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */

static void oldify_one (value v, value *p, int promote_stack)
{
  value result;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;
  struct caml_domain_state* domain_state =
    promote_domain ? promote_domain->state : caml_domain_state;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  Assert (domain_state->young_start <= domain_state->young_ptr &&
          domain_state->young_ptr <= domain_state->young_end);

 tail_call:
  if (Is_block (v) && young_ptr <= Hp_val(v) && Hp_val(v) < young_end) {
    hd = Hd_val (v);
    stat_live_bytes += Bhsize_hd(hd);
    if (hd == 0){         /* If already forwarded */
      *p = Op_val(v)[0];  /*  then forward pointer is first field. */
    } else {
      if (((value)Hp_val(v)) > oldest_promoted) {
        oldest_promoted = (value)Hp_val(v);
      }
      tag = Tag_hd (hd);
      if (tag < Infix_tag){
        value field0;

        if (tag == Stack_tag && !promote_stack) {
          /* Stacks are not promoted unless explicitly requested. */
          Ref_table_add(&promote_domain->remembered_set->major_ref, p);
        } else {
          sz = Wosize_hd (hd);
          result = alloc_shared (sz, tag);
          // caml_gc_log ("promoting object %p (referred from %p) tag=%d size=%lu to %p", (value*)v, p, tag, sz, (value*)result);
          *p = result;
          if (tag == Stack_tag) {
            memcpy((void*)result, (void*)v, sizeof(value) * sz);
            Hd_val (v) = 0;
            Op_val(v)[0] = result;
            Op_val(v)[1] = oldify_todo_list;
            oldify_todo_list = v;
          } else {
            field0 = Op_val(v)[0];
            Hd_val (v) = 0;            /* Set forward flag */
            Op_val(v)[0] = result;     /*  and forward pointer. */
            if (sz > 1){
              Op_val (result)[0] = field0;
              Op_val (result)[1] = oldify_todo_list;    /* Add this block */
              oldify_todo_list = v;                    /*  to the "to do" list. */
            }else{
              Assert (sz == 1);
              p = Op_val(result);
              v = field0;
              goto tail_call;
            }
          }
        }
      } else if (tag >= No_scan_tag) {
        sz = Wosize_hd (hd);
        result = alloc_shared(sz, tag);
        for (i = 0; i < sz; i++) Op_val (result)[i] = Op_val(v)[i];
        Hd_val (v) = 0;            /* Set forward flag */
        Op_val (v)[0] = result;    /*  and forward pointer. */
        // caml_gc_log ("promoting object %p (referred from %p) tag=%d size=%lu to %p", (value*)v, p, tag, sz, (value*)result);
        *p = result;
      } else if (tag == Infix_tag) {
        mlsize_t offset = Infix_offset_hd (hd);
        oldify_one (v - offset, p, promote_stack);   /* Cannot recurse deeper than 1. */
        *p += offset;
      } else {
        Assert (tag == Forward_tag);

        value f = Forward_val (v);
        tag_t ft = 0;

        if (Is_block (f)) {
          ft = Tag_val (Hd_val (f) == 0 ? Op_val (f)[0] : f);
        }

        if (ft == Forward_tag || ft == Lazy_tag || ft == Double_tag) {
          /* Do not short-circuit the pointer.  Copy as a normal block. */
          Assert (Wosize_hd (hd) == 1);
          result = alloc_shared (1, Forward_tag);
          // caml_gc_log ("promoting object %p (referred from %p) tag=%d size=%lu to %p",
          //             (value*)v, p, tag, (value)1, (value*)result);
          *p = result;
          Hd_val (v) = 0;             /* Set (GC) forward flag */
          Op_val (v)[0] = result;      /*  and forward pointer. */
          p = Op_val (result);
          v = f;
          goto tail_call;
        } else {
          v = f;                        /* Follow the forwarding */
          goto tail_call;               /*  then oldify. */
        }
      }
    }
  } else {
    /* XXX KC: Does this operation race with other domain's oldifying operation */
    *p = v;
  }
}

static void caml_oldify_one (value v, value* p) {
  oldify_one (v, p, 1);
}

/* Finish the work that was put off by [oldify_one].
   Note that [oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
static void oldify_mopup (int promote_stack)
{
  value v, new_v, f;
  mlsize_t i;
  struct caml_domain_state* domain_state =
    promote_domain ? promote_domain->state : caml_domain_state;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;

  while (oldify_todo_list != 0){
    v = oldify_todo_list;                 /* Get the head. */
    Assert (Hd_val (v) == 0);             /* It must be forwarded. */
    new_v = Op_val (v)[0];                /* Follow forward pointer. */
    if (Tag_val(new_v) == Stack_tag) {
      oldify_todo_list = Op_val (v)[1];   /* Remove from list (stack) */
      //caml_gc_log ("oldify_mopup: caml_scan_stack start old=%p new=%p",
      //             (value*)v, (value*)new_v);
      caml_scan_stack(caml_oldify_one, new_v);
      //caml_gc_log ("oldify_mopup: caml_scan_stack end old=%p new=%p",
      //             (value*)v, (value*)new_v);
    } else {
      oldify_todo_list = Op_val (new_v)[1]; /* Remove from list (non-stack) */

      f = Op_val (new_v)[0];
      if (Is_block (f) && young_ptr <= Hp_val(v)
          && Hp_val(v) < young_end) {
        oldify_one (f, Op_val (new_v), promote_stack);
      }
      for (i = 1; i < Wosize_val (new_v); i++){
        f = Op_val (v)[i];
        if (Is_block (f) && young_ptr <= Hp_val(v)
            && Hp_val(v) < young_end) {
          oldify_one (f, Op_val (new_v) + i, promote_stack);
        } else {
          Op_val (new_v)[i] = f;
        }
      }
    }

    Assert (Wosize_val(new_v));
  }
}

static void caml_oldify_mopup (void) {
  oldify_mopup (1);
}

//*****************************************************************************

void forward_pointer (value v, value *p) {
  header_t hd;
  mlsize_t offset;
  value fwd;
  struct caml_domain_state* domain_state =
    promote_domain ? promote_domain->state : caml_domain_state;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;

  if (Is_block (v) && young_ptr <= Hp_val(v) && Hp_val(v) < young_end) {
    hd = Hd_val(v);
    if (hd == 0) {
      // caml_gc_log ("forward_pointer: p=%p old=%p new=%p", p, (value*)v, (value*)Op_val(v)[0]);
      *p = Op_val(v)[0];
      Assert (Is_block(*p) && !Is_minor(*p));
    } else if (Tag_hd(hd) == Infix_tag) {
      offset = Infix_offset_hd(hd);
      fwd = 0;
      forward_pointer (v - offset, &fwd);
      if (fwd) *p = fwd + offset;
    }
  }
}

//*****************************************************************************

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/

void caml_empty_minor_heap_domain (struct domain* domain)
{
  struct caml_domain_state* domain_state = domain->state;
  unsigned rewritten = 0;
  int saved_stack = 0;
  value young_ptr = (value)domain_state->young_ptr;
  value young_end = (value)domain_state->young_end;
  uintnat minor_allocated_bytes = young_end - young_ptr;
  value **r;

  if (!caml_stack_is_saved()) {
    saved_stack = 1;
    caml_save_stack_gc();
  }

  promote_domain = domain;

  stat_live_bytes = 0;

  if (minor_allocated_bytes != 0) {
    caml_gc_log ("Minor collection of domain %d starting", domain->id);
    caml_do_local_roots(&caml_oldify_one, domain);

    for (r = domain->remembered_set->fiber_ref.base; r < domain->remembered_set->fiber_ref.ptr; r++) {
      caml_scan_dirty_stack_domain (&caml_oldify_one, (value)*r, domain);
    }

    for (r = domain->remembered_set->major_ref.base; r < domain->remembered_set->major_ref.ptr; r++) {
      value x = **r;
      caml_oldify_one (x, &x);
    }

    caml_oldify_mopup ();

    for (r = domain->remembered_set->major_ref.base; r < domain->remembered_set->major_ref.ptr; r++){
      value v = **r;
      if (Is_block (v) &&
          (char*)young_ptr <= Hp_val(v) &&
          Hp_val(v) < (char*)young_end) {
        Assert (Hp_val (v) >= domain_state->young_ptr);
        value vnew;
        header_t hd = Hd_val(v);
        int offset = 0;
        if (Tag_hd(hd) == Infix_tag) {
          offset = Infix_offset_hd(hd);
          v -= offset;
        }
        Assert (Hd_val(v) == 0);
        vnew = Op_val(v)[0] + offset;
        Assert (Is_block(vnew) && !Is_minor(vnew));
        Assert (Hd_val(vnew));
        if (Tag_hd(hd) == Infix_tag) { Assert(Tag_val(vnew) == Infix_tag); }
        if (__sync_bool_compare_and_swap (*r,v,vnew)) ++rewritten;
        caml_darken(vnew,0);
      }
      //XXX KC: Could I move the darkening into the conditional above as
      //caml_darken(vnew,0)?
      //caml_darken (**r,*r);
    }

    clear_table (&(domain->remembered_set->major_ref));
    clear_table (&(domain->remembered_set->minor_ref));

    domain_state->young_ptr = domain_state->young_end;
    caml_stat_minor_words += Wsize_bsize (minor_allocated_bytes);

    caml_gc_log ("Minor collection of domain %d completed: %u of %u kb live, %u pointers rewritten",
                 domain->id, (unsigned)stat_live_bytes/1024, (unsigned)minor_allocated_bytes/1024, rewritten);
  }
  else {
    caml_gc_log ("Minor collection of domain %d: skipping", domain->id);
  }

  for (r = domain->remembered_set->fiber_ref.base; r < domain->remembered_set->fiber_ref.ptr; r++) {
    caml_scan_dirty_stack_domain (&caml_darken, (value)*r, domain);
    caml_clean_stack_domain ((value)*r, domain);
  }
  clear_table (&(domain->remembered_set->fiber_ref));

  if (saved_stack) {
    caml_restore_stack_gc();
  }

  promote_domain = 0;


#ifdef DEBUG
  {
    value *p;
    for (p = (value *) domain_state->young_start;
         p < (value *) domain_state->young_end; ++p){
      *p = Debug_free_minor;
    }
    ++ minor_gc_counter;
  }
#endif
}

CAMLexport value caml_promote(struct domain* domain, value root)
{
  CAMLparam1_domain(domain, root);
  caml_empty_minor_heap_domain (domain);
  CAMLreturn_domain(domain, root);
}

void caml_empty_minor_heap ()
{
  caml_empty_minor_heap_domain (caml_domain_self());
}

/* Do a minor collection and a slice of major collection, call finalisation
   functions, etc.
   Leave the minor heap empty.
*/
CAMLexport void caml_minor_collection (void)
{
  /* !! intnat prev_alloc_words = caml_allocated_words; */

  caml_log_event(EVENT_GC_START);

  caml_empty_minor_heap ();

  /* !! caml_stat_promoted_words += caml_allocated_words - prev_alloc_words; */
  ++ caml_stat_minor_collections;
  caml_major_collection_slice (0);

  /* !! caml_final_do_calls (); */

  caml_empty_minor_heap ();
  caml_log_event(EVENT_GC_END);
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  caml_handle_gc_interrupt();
  CAMLreturn (extra_root);
}

void caml_realloc_ref_table (struct caml_ref_table *tbl)
{                                           Assert (tbl->ptr == tbl->limit);
                                            Assert (tbl->limit <= tbl->end);
                                      Assert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    caml_alloc_table (tbl, caml_minor_heap_size / sizeof (value) / 8, 256);
  }else if (tbl->limit == tbl->threshold){
    caml_gc_log ("ref_table threshold crossed");
    tbl->limit = tbl->end;
    caml_urge_major_slice ();
  }else{ /* This will almost never happen with the bytecode interpreter. */
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * sizeof (value*);
    caml_gc_log ("Growing ref_table to %"
                 ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
                     (intnat) sz/1024);
    tbl->base = (value**) caml_stat_resize ((char *) tbl->base, sz);
    if (tbl->base == NULL){
      caml_fatal_error ("Fatal error: ref_table overflow\n");
    }
    tbl->end = tbl->base + tbl->size + tbl->reserve;
    tbl->threshold = tbl->base + tbl->size;
    tbl->ptr = tbl->base + cur_ptr;
    tbl->limit = tbl->end;
  }
}
