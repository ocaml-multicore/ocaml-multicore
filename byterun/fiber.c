#define CAML_INTERNALS

#include <string.h>
#include <unistd.h>
#include "caml/misc.h"
#include "caml/fiber.h"
#include "caml/gc_ctrl.h"
#include "caml/instruct.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/platform.h"
#include "caml/fix_code.h"
#include "caml/minor_gc.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/memory.h"
#include "caml/startup_aux.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "frame_descriptors.h"
#endif

static __thread int stack_is_saved = 0;

static void dirty_stack(value stack)
{
  uintnat status;
  /* There is no write barrier (caml_modify) on writes to the stack,
     so a just-run fiber's stack may contain untracked shared->young
     pointers or pointers to unmarked objects. We add the stack to a
     ref table so that the GC can find these pointers. */
  if (!Is_minor(stack)) {
    Assert(Stack_dirty_domain(stack) == FIBER_CLEAN ||
           Stack_dirty_domain(stack) == FIBER_SCANNING ||
           Stack_dirty_domain(stack) == caml_domain_self());
    if (Stack_dirty_domain(stack) != caml_domain_self ()) {
      SPIN_WAIT {
        status = atomic_load_acq((atomic_uintnat*)&Stack_dirty_domain(stack));
        if(status == (uintnat)FIBER_SCANNING) continue;
        if(__sync_bool_compare_and_swap(&Stack_dirty_domain(stack),
                                        FIBER_CLEAN, caml_domain_self())) {
          caml_darken(0, stack, 0);
          /* XXX KC: Optimize. Since we cannot distinguish Grey from
           * Black, we might scan the stacks multiple times. */
          caml_scan_stack(&caml_darken, 0, stack);
          break;
        }
      }
      Ref_table_add(&Caml_state->remembered_set->fiber_ref, (value*)stack);
    }
  }
}

#ifdef NATIVE_CODE

static value save_stack ()
{
  caml_domain_state* domain_state = Caml_state;
  Assert (Hd_val(domain_state->current_stack) &&
          (Is_minor(domain_state->current_stack) || !is_garbage(domain_state->current_stack)));
  value old_stack = domain_state->current_stack;
  return old_stack;
}

static void load_stack (value stack) {
  caml_domain_state* domain_state = Caml_state;
  domain_state->stack_threshold = Stack_base(stack)
    + caml_params->profile_slop_wsz + Stack_threshold / sizeof(value);
  domain_state->stack_high = Stack_high(stack);
  domain_state->current_stack = stack;
  dirty_stack(stack);
  /* XXX KC: Optimize this. Record minor cycle count. */
  if (domain_state->promoted_in_current_cycle)
    caml_scan_stack (forward_pointer, 0, stack);
}

extern void caml_fiber_exn_handler (value) Noreturn;
extern void caml_fiber_val_handler (value) Noreturn;

//FIXME #define INIT_FIBER_USED (3 + sizeof(struct caml_context) / sizeof(value))

value caml_alloc_stack (value hval, value hexn, value heff)
{
  CAMLparam3(hval, hexn, heff);
  CAMLlocal1(stack);
  char* sp;
  struct fiber_context *ctxt;
  struct fiber_link *link;

  stack = caml_alloc(caml_fiber_wsz + caml_params->profile_slop_wsz, Stack_tag);
  link = fiber_link(stack);
  link->dirty_domain = FIBER_CLEAN;
  link->handle_val = hval;
  link->handle_exn = hexn;
  link->handle_eff = heff;
  link->parent = Val_unit;
//FIXME  link->parent_sp = 0;
  link->exn.handler_pc = (uintnat)caml_fiber_exn_handler;
  link->exn.next_offset = 0;
  
  /* push a fiber_context below the fiber_link */
  ctxt = ((struct caml_context*)link) - 1;
  ctxt->exception_handler_offset = (char*)link - (char*)(&link->exn);
  ctxt->pc = (uintnat)caml_fiber_val_handler;
  ctxt->gc_regs = 0;

  Stack_sp(stack) = (value*)ctxt;
  caml_gc_log ("Allocate stack=0x%lx of %lu words", stack, caml_fiber_wsz);

  CAMLreturn (stack);
}

void caml_scan_stack(scanning_action f, void* fdata, value stack)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int n, ofs;
  unsigned short * p;
  value *root;
  struct fiber_context* context;
  frame_descr** frame_descriptors;
  struct fiber_link* fiblink;
  int frame_descriptors_mask;

  f(fdata, caml_frame_descriptor_table, &caml_frame_descriptor_table);
  f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
  f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
  f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));
  f(fdata, Stack_parent(stack), &Stack_parent(stack));

  fiblink = fiber_link(stack);
  context = (struct fiber_context*)fiblink->sp;
  
  Assert(caml_frame_descriptor_table != Val_unit);
  frame_descriptors = Data_abstract_val(caml_frame_descriptor_table);
  frame_descriptors_mask = Wosize_val(caml_frame_descriptor_table) - 1;

next_chunk:
  sp = (char*)(context + 1);
  if (sp == (char*)fiblink) return;
  regs = context->gc_regs;
  retaddr = context->pc;

  while(1) {
    /* Find the descriptor corresponding to the return address */
    h = Hash_retaddr(retaddr, frame_descriptors_mask);
    while(1) {
      d = frame_descriptors[h];
      if (d->retaddr == retaddr) break;
      h = (h+1) & frame_descriptors_mask;
    }
    if (d->frame_size != 0xFFFF) {
      /* Scan the roots in this frame */
      for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
        ofs = *p;
        if (ofs & 1) {
          root = regs + (ofs >> 1);
        } else {
          root = (value *)(sp + ofs);
        }
        f (fdata, *root, root);
      }
      /* Move to next frame */
      sp += (d->frame_size & 0xFFFC);
      retaddr = Saved_return_address(sp);
      /* XXX KC: disabled already scanned optimization. */
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous stack
       * chunk. */
      context = (struct fiber_context*)sp;
      goto next_chunk;
    }
  }
}

void caml_maybe_expand_stack ()
{
  uintnat stack_available;

  Assert(Tag_val(Caml_state->current_stack) == Stack_tag);

  stack_available = Wosize_val(Caml_state->current_stack)
    - (Stack_high(Caml_state->current_stack) - Stack_sp(Caml_state->current_stack))
    - Stack_ctx_words;
  if (stack_available < caml_params->profile_slop_wsz
                      + 2 * Stack_threshold / sizeof(value))
    caml_realloc_stack (0, 0, 0);
}

void caml_update_gc_regs_slot (value* gc_regs)
{
  struct caml_context *ctxt;
  ctxt = (struct caml_context*) Stack_sp(Caml_state->current_stack);
  ctxt->gc_regs = gc_regs;
}

/* Returns 1 if the target stack is a fresh stack to which control is switching
 * to. */
int caml_switch_stack(value stk)
{
  abort();
  /*
  save_stack();
  load_stack(stk);
  if (Stack_sp(stk) == Stack_high(stk) - INIT_FIBER_USED)
    return 1;
  return 0;
  */
}

#else /* End NATIVE_CODE, begin BYTE_CODE */

caml_root caml_global_data;

static value save_stack ()
{
  caml_domain_state* domain_state = Caml_state;
  Assert (Hd_val(domain_state->current_stack) &&
          (Is_minor(domain_state->current_stack) || !is_garbage(domain_state->current_stack)));
  value old_stack = domain_state->current_stack;
  Stack_sp(old_stack) = domain_state->extern_sp;
  Assert(domain_state->stack_threshold ==
         Stack_base(old_stack) + caml_params->profile_slop_wsz + Stack_threshold / sizeof(value));
  Assert(domain_state->stack_high == Stack_high(old_stack));
  return old_stack;
}

static void load_stack(value new_stack)
{
  Assert (Hd_val(new_stack) &&
          (Is_minor(new_stack) || !is_garbage(new_stack)));
  Assert(Tag_val(new_stack) == Stack_tag);
  caml_domain_state* domain_state = Caml_state;
  Assert(Stack_dirty_domain(new_stack) == FIBER_CLEAN
         || Stack_dirty_domain(new_stack) == FIBER_SCANNING
         || Stack_dirty_domain(new_stack) == caml_domain_self());
  domain_state->stack_threshold =
    Stack_base(new_stack) + caml_params->profile_slop_wsz + Stack_threshold / sizeof(value);
  domain_state->stack_high = Stack_high(new_stack);
  domain_state->extern_sp = Stack_sp(new_stack);
  domain_state->current_stack = new_stack;
  dirty_stack(new_stack);
  /* XXX KC: Optimize this. Record minor cycle count. */
  if (domain_state->promoted_in_current_cycle)
    caml_scan_stack (forward_pointer, 0, new_stack);
}

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  CAMLparam3(hval, hexn, heff);
  CAMLlocal1(stack);
  value* sp;
  struct fiber_context *ctxt;
  struct fiber_link *link;

  stack = caml_alloc(caml_fiber_wsz, Stack_tag);
  link = fiber_link(stack);
  link->dirty_domain = FIBER_CLEAN;
  link->handle_val = hval;
  link->handle_exn = hexn;
  link->handle_eff = heff;
  link->parent = Val_unit;
  // FIXME ?
  link->exn.handler_pc = Val_long(1);
  link->exn.next_offset = Val_long(1);

  Stack_sp(stack) = Stack_high(stack);

#ifdef FIXME
  // ?
  sp -= 1;
  sp[0] = Val_long(1); /* trapsp ?? */

  Stack_sp(stack) = sp;
  Stack_dirty_domain(stack) = FIBER_CLEAN;
  Stack_handle_value(stack) = hval;
  Stack_handle_exception(stack) = hexn;
  Stack_handle_effect(stack) = heff;
  Stack_parent(stack) = Val_unit;
#endif

  CAMLreturn (stack);
}

/*
  Find the stack that performed an effect, skipping over several stacks that
  reperformed the effect if necessary.

  Reverses the parent pointers to point
  performer -> delegator instead of
  delegator -> performer.
*/
value caml_find_performer(value stack)
{
  caml_domain_state* domain_state = Caml_state;
  value parent = domain_state->current_stack;
  Assert (Hd_val(parent) && (Is_minor(parent) || !is_garbage(parent)));
  do {
    value delegator = Stack_parent(stack);
    Stack_parent(stack) = parent;
    parent = stack;
    stack = delegator;
  } while (stack != Val_unit);
  return parent;
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->extern_sp - req < Stack_base(Caml_state->current_stack))
    caml_realloc_stack(req, 0, 0);
  return Val_unit;
}

void caml_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = Caml_state->stack_high - Caml_state->extern_sp
                 + caml_params->profile_slop_wsz
                 + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != caml_max_stack_size){
    caml_gc_log ("Changing stack limit to %luk bytes",
                     new_max_size * sizeof (value) / 1024);
  }
  caml_max_stack_size = new_max_size;
}

/*
  Root scanning.

  Used by the GC to find roots on the stacks of running or runnable fibers.
*/

void caml_scan_stack(scanning_action f, void* fdata, value stack)
{
  value *low, *high, *sp;
  Assert(Is_block(stack) && Tag_val(stack) == Stack_tag);

  f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
  f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
  f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));
  f(fdata, Stack_parent(stack), &Stack_parent(stack));

  high = Stack_high(stack);
  low = Stack_sp(stack);
  for (sp = low; sp < high; sp++) {
    f(fdata, *sp, sp);
  }
}

value caml_switch_stack(value stk)
{
  value s = save_stack();
  load_stack(stk);
  return s;
}

#endif /* end BYTE_CODE */

void caml_save_stack_gc()
{
  Assert(stack_is_saved >= 0);
  save_stack();
  ++stack_is_saved;
}

int caml_stack_is_saved ()
{
  return stack_is_saved;
}

void caml_restore_stack_gc()
{
  caml_domain_state* domain_state = Caml_state;
  Assert(stack_is_saved > 0);
  Assert(Tag_val(domain_state->current_stack) == Stack_tag);
  --stack_is_saved;
  if (stack_is_saved == 0)
    load_stack(domain_state->current_stack);
}

void caml_clean_stack_domain(value stack, struct domain* domain)
{
  Assert(Tag_val(stack) == Stack_tag);
  if (Stack_dirty_domain(stack) == domain) {
    atomic_store_rel((atomic_uintnat*)&Stack_dirty_domain(stack),
                     (uintnat)FIBER_CLEAN);
  }
}

void caml_scan_dirty_stack_domain(scanning_action f, void* fdata, value stack,
                                  struct domain* domain)
{
  Assert (Tag_val(stack) == Stack_tag);
  if (Stack_dirty_domain(stack) == domain) {
    caml_scan_stack(f, fdata, stack);
  }
}

void caml_darken_stack(value stack)
{
  Assert(Tag_val(stack) == Stack_tag);
  if (Stack_dirty_domain(stack) == FIBER_CLEAN &&
      __sync_bool_compare_and_swap (&Stack_dirty_domain(stack),
                                    FIBER_CLEAN, FIBER_SCANNING)) {
    caml_darken(0, stack, 0);
    /* XXX KC: Optimize. Since we cannot distinguish Grey from Black,
     * we might scan the stacks multiple times. */
    caml_scan_stack(&caml_darken, 0, stack);
    atomic_store_rel((atomic_uintnat*)&Stack_dirty_domain(stack),
                    (uintnat)FIBER_CLEAN);
  }
}

void caml_clean_stack(value stack)
{
  Assert(Tag_val(stack) == Stack_tag);
  if (Stack_dirty_domain(stack) == caml_domain_self()) {
    Stack_dirty_domain(stack) = FIBER_CLEAN;
  }
}

/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

int caml_on_current_stack(value* p)
{
  return Stack_base(Caml_state->current_stack) <= p && p < Caml_state->stack_high;
}

void caml_realloc_stack(asize_t required_space, value* saved_vals, int nsaved)
{
  CAMLparamN(saved_vals, nsaved);
  CAMLlocal2(old_stack, new_stack);
  asize_t size;
  int stack_used;

  old_stack = save_stack();

  stack_used = Stack_high(old_stack) - Stack_sp(old_stack);
  size = Stack_high(old_stack) - Stack_base(old_stack);
  do {
    if (size >= caml_max_stack_size) caml_raise_stack_overflow();
    size *= 2;
  } while (size < stack_used + required_space);
  if (size > 4096 / sizeof(value)) {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
                 (uintnat) size * sizeof(value) / 1024);
  } else {
    caml_gc_log ("Growing stack to %"
                 ARCH_INTNAT_PRINTF_FORMAT "u bytes",
                 (uintnat) size * sizeof(value));
  }

  new_stack = caml_alloc(Stack_ctx_words + size, Stack_tag);
  caml_move_stack(old_stack, new_stack);

  Stack_dirty_domain(new_stack) = FIBER_CLEAN;
  if (Stack_dirty_domain(old_stack) != FIBER_CLEAN) {
    Assert (Stack_dirty_domain(old_stack) == caml_domain_self());
    dirty_stack(new_stack);
  }

  load_stack(new_stack);

  /* Reset old stack */
  Stack_sp(old_stack) = Stack_high(old_stack);

  CAMLreturn0;
}

void caml_init_stack (value stack)
{
  Stack_sp(stack) = Stack_high(stack);
  Stack_dirty_domain(stack) = FIBER_CLEAN;
  Stack_handle_value(stack) = Val_long(0);
  Stack_handle_exception(stack) = Val_long(0);
  Stack_handle_effect(stack) = Val_long(0);
  Stack_parent(stack) = Val_unit;
}

value caml_alloc_main_stack (uintnat init_size)
{
  CAMLparam0();
  CAMLlocal1(stack);

  /* Create a stack for the main program.
     The GC is not initialised yet, so we use caml_alloc_shr
     which cannot trigger it */
  stack = caml_alloc_shr(init_size, Stack_tag);
  caml_init_stack (stack);

  CAMLreturn(stack);
}

void* caml_init_main_stack ()
{
  value stack;
  stack = caml_alloc_main_stack (caml_params->profile_slop_wsz +
                            Stack_size/sizeof(value));
  load_stack(stack);
  return Stack_sp(stack);
}

CAMLprim value caml_clone_continuation (value cont)
{
  abort();
#if FIXME
  CAMLparam1(cont);
  CAMLlocal4(new_cont, prev_target, source, target);
  intnat bvar_stat;
  int stack_used;

  bvar_stat = caml_bvar_status(cont);
  if (bvar_stat & BVAR_EMPTY)
    caml_invalid_argument ("Obj.clone: continuation already taken");

  prev_target = Val_unit;
  caml_read_field(cont, 0, &source);

  do {
    Assert (Is_block (source) && Tag_val(source) == Stack_tag);

    /* Ensure that the stack remains 16-byte aligned. Note: Stack_high
     * always returns 16-byte aligned down address. */
    stack_used = Stack_high(source) - Stack_sp(source);
    target = caml_alloc (Wosize_val(source), Stack_tag);
    memcpy((void*)target, (void*)source, sizeof(value) * Stack_ctx_words);
    Stack_sp(target) = Stack_high(target) - stack_used;
    memcpy(Stack_sp(target), Stack_sp(source), stack_used * sizeof(value));
    Stack_dirty_domain(target) = FIBER_CLEAN;

    if (prev_target == Val_unit) {
      new_cont = caml_bvar_create (target);
    } else {
      Stack_parent(prev_target) = target;
    }

    prev_target = target;
    source = Stack_parent(source);
  } while (source != Val_unit);

  CAMLreturn(new_cont);
#endif
}

void caml_restore_stack()
{
  Assert(Tag_val(Caml_state->current_stack) == Stack_tag);
  load_stack(Caml_state->current_stack);
}

value caml_reverse_fiber_stack (value stack)
{
  Assert(Tag_val(stack) == Stack_tag);
  value next;
  value prev = Val_unit;

  while (stack != Val_unit) {
    next = Stack_parent(stack);
    Stack_parent(stack) = prev;
    prev = stack;
    stack = next;
  }

  return prev;
}

#ifdef DEBUG
value* stack_sp(value stk) {
  return Stack_sp(stk);
}

struct domain* stack_dirty_domain(value stk) {
  return Stack_dirty_domain(stk);
}

value stack_parent(value stk) {
  return Stack_parent(stk);
}

value stack_high(value stk) {
  return (value)Stack_high(stk);
}
#endif
