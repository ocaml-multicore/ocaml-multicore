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
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/weak.h"
#include "caml/domain.h"
#include "caml/shared_heap.h"
#include "caml/addrmap.h"
#include "caml/fiber.h"
#include "caml/eventlog.h"

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
  struct caml_domain_state* domain_state = CAML_DOMAIN_STATE;
  if (domain_state->young_ptr != domain_state->young_end) caml_minor_collection ();

  caml_reallocate_minor_heap(size);

  reset_table (&domain_state->remembered_set->major_ref);
  reset_table (&domain_state->remembered_set->minor_ref);
}

//*****************************************************************************

struct oldify_state {
  value todo_list;
  uintnat live_bytes;
  int should_promote_stacks;
  struct domain* promote_domain;
  value oldest_promoted;
};

static value alloc_shared(mlsize_t wosize, tag_t tag)
{
  void* mem = caml_shared_try_alloc(caml_domain_self()->shared_heap, wosize, tag, 0 /* not promotion */);
  CAML_DOMAIN_STATE->allocated_words += Whsize_wosize(wosize);
  if (mem == NULL) {
    caml_fatal_error("allocation failure during minor GC");
  }
  return Val_hp(mem);
}

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */

static void oldify_one (void* st_v, value v, value *p)
{
  struct oldify_state* st = st_v;
  value result;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;
  struct caml_domain_state* domain_state =
    st->promote_domain ? st->promote_domain->state : CAML_DOMAIN_STATE;
  struct caml_remembered_set *remembered_set = domain_state->remembered_set;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;
  Assert (domain_state->young_start <= domain_state->young_ptr &&
          domain_state->young_ptr <= domain_state->young_end);

 tail_call:
  if (Is_block (v) && young_ptr <= Hp_val(v) && Hp_val(v) < young_end) {
    hd = Hd_val (v);

    if (hd == 0){         /* If already forwarded */
      *p = Op_val(v)[0];  /*  then forward pointer is first field. */
    } else {
      if (((value)Hp_val(v)) > st->oldest_promoted) {
        st->oldest_promoted = (value)Hp_val(v);
      }
      tag = Tag_hd (hd);
      if (tag < Infix_tag){
        value field0;

        if (tag == Stack_tag && !st->should_promote_stacks) {
          /* Stacks are not promoted unless explicitly requested. */
          Ref_table_add(&remembered_set->major_ref, p);
        } else {
          sz = Wosize_hd (hd);
          st->live_bytes += Bhsize_hd(hd);
          result = alloc_shared (sz, tag);
          // caml_gc_log ("promoting object %p (referred from %p) tag=%d size=%lu to %p", (value*)v, p, tag, sz, (value*)result);
          *p = result;
          if (tag == Stack_tag) {
            memcpy((void*)result, (void*)v, sizeof(value) * sz);
            Hd_val (v) = 0;
            Op_val(v)[0] = result;
            Op_val(v)[1] = st->todo_list;
            st->todo_list = v;
          } else {
            field0 = Op_val(v)[0];
            Hd_val (v) = 0;            /* Set forward flag */
            Op_val(v)[0] = result;     /*  and forward pointer. */
            if (sz > 1){
              Op_val (result)[0] = field0;
              Op_val (result)[1] = st->todo_list;    /* Add this block */
              st->todo_list = v;                     /*  to the "to do" list. */
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
        st->live_bytes += Bhsize_hd(hd);
        result = alloc_shared(sz, tag);
        for (i = 0; i < sz; i++) Op_val (result)[i] = Op_val(v)[i];
        Hd_val (v) = 0;            /* Set forward flag */
        Op_val (v)[0] = result;    /*  and forward pointer. */
        // caml_gc_log ("promoting object %p (referred from %p) tag=%d size=%lu to %p", (value*)v, p, tag, sz, (value*)result);
        *p = result;
      } else if (tag == Infix_tag) {
        mlsize_t offset = Infix_offset_hd (hd);
        oldify_one (st, v - offset, p);   /* Cannot recurse deeper than 1. */
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
          st->live_bytes += Bhsize_hd(hd);
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
    *p = v;
  }
}

/* Finish the work that was put off by [oldify_one].
   Note that [oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
static void oldify_mopup (struct oldify_state* st)
{
  value v, new_v, f;
  mlsize_t i;
  struct caml_domain_state* domain_state =
    st->promote_domain ? st->promote_domain->state : CAML_DOMAIN_STATE;
  char* young_ptr = domain_state->young_ptr;
  char* young_end = domain_state->young_end;

  while (st->todo_list != 0){
    v = st->todo_list;                 /* Get the head. */
    Assert (Hd_val (v) == 0);             /* It must be forwarded. */
    new_v = Op_val (v)[0];                /* Follow forward pointer. */
    if (Tag_val(new_v) == Stack_tag) {
      st->todo_list = Op_val (v)[1];   /* Remove from list (stack) */
      //caml_gc_log ("oldify_mopup: caml_scan_stack start old=%p new=%p",
      //             (value*)v, (value*)new_v);
      caml_scan_stack(oldify_one, st, new_v);
      //caml_gc_log ("oldify_mopup: caml_scan_stack end old=%p new=%p",
      //             (value*)v, (value*)new_v);
    } else {
      st->todo_list = Op_val (new_v)[1]; /* Remove from list (non-stack) */

      f = Op_val (new_v)[0];
      if (Is_block (f) && young_ptr <= Hp_val(v)
          && Hp_val(v) < young_end) {
        oldify_one (st, f, Op_val (new_v));
      }
      for (i = 1; i < Wosize_val (new_v); i++){
        f = Op_val (v)[i];
        if (Is_block (f) && young_ptr <= Hp_val(v)
            && Hp_val(v) < young_end) {
          oldify_one (st, f, Op_val (new_v) + i);
        } else {
          Op_val (new_v)[i] = f;
        }
      }
    }

    Assert (Wosize_val(new_v));
  }
}

//*****************************************************************************

void forward_pointer (void* state, value v, value *p) {
  header_t hd;
  mlsize_t offset;
  value fwd;
  struct domain* promote_domain = state;
  struct caml_domain_state* domain_state =
    promote_domain ? promote_domain->state : CAML_DOMAIN_STATE;
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
      forward_pointer (state, v - offset, &fwd);
      if (fwd) *p = fwd + offset;
    }
  }
}

void caml_empty_minor_heap_domain (struct domain* domain);

CAMLexport value caml_promote(struct domain* domain, value root)
{
  value **r;
  value iter, f;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;
  int saved_stack = 0;
  struct caml_domain_state *domain_state = domain->state;
  struct caml_remembered_set *remembered_set = domain_state->remembered_set;
  value young_ptr = (value)domain_state->young_ptr;
  value young_end = (value)domain_state->young_end;
  float percent_to_scan;
  uintnat prev_alloc_words = domain_state->allocated_words;
  struct oldify_state st = {0};

  /* Integers are already shared */
  if (Is_long(root))
    return root;

  tag = Tag_val(root);
   /* Non-stack objects which are in the major heap are already shared. */
  if (tag != Stack_tag && !Is_minor(root))
    return root;

  if (!caml_stack_is_saved()) {
    saved_stack = 1;
    caml_save_stack_gc();
  }

  st.oldest_promoted = (value)domain_state->young_start;
  st.promote_domain = domain;

  if (tag != Stack_tag) {
    Assert(caml_owner_of_young_block(root) == domain);

    /* For non-stack objects, don't promote referenced stacks. They are
     * promoted only when explicitly requested. */
    oldify_one (&st, root, &root);
  } else {
    /* The object is a stack */

    if (Is_minor(root)) {
      /* While we do not in general promote stacks that we find, we
         certainly want to promote the root if it happens to be a
         stack */
      st.should_promote_stacks = 1;      
      oldify_one (&st, root, &root);
      st.should_promote_stacks = 0;
    } else {
      /* Though the stack is in the major heap, it can contain objects in the
       * minor heap. They must be promoted. */
      caml_scan_dirty_stack_domain(&oldify_one, &st, root, domain);
    }
  }

  oldify_mopup (&st);

  // caml_gc_log ("caml_promote: new root=0x%lx oldest_promoted=0x%lx",
  //            root, oldest_promoted);

  Assert (!Is_minor(root));
  /* XXX KC: We might checking for rpc's just before a stw_phase of a major
   * collection? Is this necessary? */
  caml_darken(0, root, 0);

  if (tag == Stack_tag) {
    /* Since we've promoted the objects on the stack, the stack is now clean. */
    caml_clean_stack_domain(root, domain);
  }

  percent_to_scan = st.oldest_promoted <= young_ptr ? 0.0 :
    (((float)(st.oldest_promoted - young_ptr)) * 100.0 /
     (young_end - (value)domain_state->young_start));

  if (percent_to_scan > Percent_to_promote_with_GC) {
    caml_gc_log("caml_promote: forcing minor GC. %%_minor_to_scan=%f", percent_to_scan);
    if (saved_stack) caml_restore_stack_gc();
    // ???
    caml_empty_minor_heap_domain (domain);
  } else {
    /* Scan local roots */
    caml_do_local_roots (forward_pointer, st.promote_domain, domain);

    /* Scan current stack */
    caml_scan_stack (forward_pointer, st.promote_domain, domain_state->current_stack);

    /* Scan major to young pointers. */
    for (r = remembered_set->major_ref.base; r < remembered_set->major_ref.ptr; r++) {
      value old_p = **r;
      if (Is_block(old_p) && young_ptr <= old_p && old_p < young_end) {
        value new_p = old_p;
        forward_pointer (st.promote_domain, new_p, &new_p);
        if (old_p != new_p)
          __sync_bool_compare_and_swap (*r,old_p,new_p);
        //caml_gc_log ("forward: old_p=%p new_p=%p **r=%p",(value*)old_p, (value*)new_p,(value*)**r);
      }
    }

    /* Scan young to young pointers */
    for (r = remembered_set->minor_ref.base; r < remembered_set->minor_ref.ptr; r++) {
      forward_pointer (st.promote_domain, **r, *r);
    }

    /* Scan newer objects */
    iter = young_ptr;
    Assert(st.oldest_promoted < young_end);
    while (iter <= st.oldest_promoted) {
      hd = Hd_hp(iter);
      iter = Val_hp(iter);
      if (hd == 0) {
        /* Fowarded object. */
        mlsize_t wsz = Wosize_val(Op_val(iter)[0]);
        Assert (wsz <= Max_young_wosize);
        sz = Bsize_wsize(wsz);
      } else {
        tag = Tag_hd (hd);
        Assert (tag != Infix_tag);
        sz = Bosize_hd (hd);
        Assert (Wosize_hd(hd) <= Max_young_wosize);
        //caml_gc_log ("Scan: iter=%p sz=%lu tag=%u", (value*)iter, Wsize_bsize(sz), tag);
        if (tag < No_scan_tag && tag != Stack_tag) { /* Stacks will be scanned lazily, so skip. */
          for (i = 0; i < Wsize_bsize(sz); i++) {
            f = Op_val(iter)[i];
            if (Is_block(f)) {
              forward_pointer (st.promote_domain, f,((value*)iter) + i);
            }
          }
        }
      }
      iter += sz;
    }

    if (saved_stack)
      caml_restore_stack_gc();

    domain_state->promoted_in_current_cycle = 1;
  }
  domain_state->stat_promoted_words += domain_state->allocated_words - prev_alloc_words;
  return root;
}

//*****************************************************************************

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/

void caml_empty_minor_heap_domain (struct domain* domain)
{
  struct caml_domain_state* domain_state = domain->state;
  struct caml_remembered_set *remembered_set = domain_state->remembered_set;
  unsigned rewritten = 0;
  int saved_stack = 0;
  value young_ptr = (value)domain_state->young_ptr;
  value young_end = (value)domain_state->young_end;
  uintnat minor_allocated_bytes = young_end - young_ptr;
  struct oldify_state st = {0};
  value **r;

  if (!caml_stack_is_saved()) {
    saved_stack = 1;
    caml_save_stack_gc();
  }

  st.promote_domain = domain;
  st.should_promote_stacks = 1;

  if (minor_allocated_bytes != 0) {
    uintnat prev_alloc_words = domain_state->allocated_words;
    caml_gc_log ("Minor collection of domain %d starting", domain->id);
    caml_do_local_roots(&oldify_one, &st, domain);

    for (r = remembered_set->fiber_ref.base; r < remembered_set->fiber_ref.ptr; r++) {
      caml_scan_dirty_stack_domain (&oldify_one, &st, (value)*r, domain);
    }

    for (r = remembered_set->major_ref.base; r < remembered_set->major_ref.ptr; r++) { value x = **r;
      oldify_one (&st, x, &x);
    }

    oldify_mopup (&st);

    for (r = remembered_set->major_ref.base; r < remembered_set->major_ref.ptr; r++) {
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
        caml_darken(0, vnew,0);
      }
    }

    clear_table (&remembered_set->major_ref);
    clear_table (&remembered_set->minor_ref);
    domain_state->young_ptr = domain_state->young_end;
    domain_state->stat_minor_words += Wsize_bsize (minor_allocated_bytes);
    domain_state->stat_minor_collections++;
    domain_state->stat_promoted_words += domain_state->allocated_words - prev_alloc_words;

    caml_gc_log ("Minor collection of domain %d completed: %2.0f%% of %u KB live, %u pointers rewritten",
                 domain->id,
                 100.0 * (double)st.live_bytes / (double)minor_allocated_bytes,
                 (unsigned)(minor_allocated_bytes + 512)/1024, rewritten);
  }
  else {
    caml_gc_log ("Minor collection of domain %d: skipping", domain->id);
  }

  for (r = remembered_set->fiber_ref.base; r < remembered_set->fiber_ref.ptr; r++) {
    caml_scan_dirty_stack_domain (&caml_darken, 0, (value)*r, domain);
    caml_clean_stack_domain ((value)*r, domain);
  }
  clear_table (&remembered_set->fiber_ref);

  if (saved_stack) {
    caml_restore_stack_gc();
  }

  domain_state->promoted_in_current_cycle = 0;

#ifdef DEBUG
  {
    value *p;
    for (p = (value *) domain_state->young_start;
         p < (value *) domain_state->young_end; ++p){
      *p = Debug_free_minor;
    }
  }
#endif
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
  caml_log_event(EVENT_GC_START);

  caml_empty_minor_heap ();

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
    caml_alloc_table (tbl, CAML_DOMAIN_STATE->minor_heap_size / sizeof (value) / 8, 256);
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
