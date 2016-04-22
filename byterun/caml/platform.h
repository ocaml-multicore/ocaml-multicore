#ifndef CAML_PLAT_THREADS_H
#define CAML_PLAT_THREADS_H
/* Platform-specific concurrency and memory primitives */

#include <pthread.h>
#include "mlvalues.h"
#include "memory.h"

/*
FIXME: This file should use C11 atomics if they are available.

#if __STDC_VERSION__ >= 201112L
... stuff ...
#endif
*/

#if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif

#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || defined(__GNUC__)
#define INLINE static inline
#else
#define INLINE static
#endif

#if defined(__GNUC__)
#define compiler_barrier() __asm__ __volatile__ ("" ::: "memory");
#else
#error "no compiler barrier defined for this compiler"
#endif

#if defined(__GNUC__)
#define compiler_expect_1(c) __builtin_expect((c), 1)
#else
#define compiler_expect_1(c) c
#endif


/* Loads and stores with acquire and release semantics respectively */

#if defined(__x86_64__) || defined(__i386__)

INLINE void cpu_relax() {
  asm volatile("pause" ::: "memory");
}

#define ATOMIC_UINTNAT_INIT(x) { (x) }

/* On x86, all loads are acquire and all stores are release. So, only
   compiler barriers are necessary in the following. */

INLINE uintnat atomic_load_acq(atomic_uintnat* p) {
  uintnat v = p->val;
  compiler_barrier();
  return v;
}

INLINE void atomic_store_rel(atomic_uintnat* p, uintnat v) {
  compiler_barrier();
  p->val = v;
}

#else
#error "unsupported platform (i.e. not x86)"
#endif


/* Spin-wait loops */

#define Max_spins 1000

unsigned caml_plat_spin_wait(unsigned spins,
                             const char* file, int line,
                             const char* function);

#define GENSYM_3(name, l) name##l
#define GENSYM_2(name, l) GENSYM_3(name, l)
#define GENSYM(name) GENSYM_2(name, __LINE__)


#define SPIN_WAIT                                                       \
  unsigned GENSYM(caml__spins) = 0;                                     \
  for (; 1; cpu_relax(),                                                \
         GENSYM(caml__spins) =                                          \
           compiler_expect_1(GENSYM(caml__spins) < Max_spins) ?         \
         GENSYM(caml__spins) + 1 :                                      \
         caml_plat_spin_wait(GENSYM(caml__spins),                       \
                             __FILE__, __LINE__, __func__))


INLINE uintnat atomic_load_wait_nonzero(atomic_uintnat* p) {
  SPIN_WAIT {
    uintnat v = atomic_load_acq(p);
    if (v) return v;
  }
}




/* Atomic read-modify-write instructions, with full fences */

#if defined(__GNUC__)

/* atomically: old = *p; *p += v; return old; */
INLINE uintnat atomic_fetch_add(atomic_uintnat* p, uintnat v) {
  return __sync_fetch_and_add(&p->val, v);
}

/* atomically: if (*p == vold) { *p = vnew; return 1; } else { return 0; }
   may spuriously return 0 even when *p == vold */
INLINE int atomic_cas(atomic_uintnat* p, uintnat vold, uintnat vnew) {
  return __sync_bool_compare_and_swap(&p->val, vold, vnew);
}

/* atomically: if (*p == vold) { *p = vnew; } */
INLINE void atomic_cas_strong(atomic_uintnat* p, uintnat vold, uintnat vnew) {
  __sync_val_compare_and_swap(&p->val, vold, vnew);
}

#else
#error "unsupported platform"
#endif





typedef pthread_mutex_t caml_plat_mutex;
#define caml_plat_mutex_init(m) pthread_mutex_init(m,0)
#define caml_plat_lock pthread_mutex_lock
#define caml_plat_try_lock(l) (pthread_mutex_trylock(l) == 0)
#define caml_plat_unlock pthread_mutex_unlock
#define caml_plat_mutex_free pthread_mutex_destroy

struct caml__mutex_unwind {
  caml_plat_mutex* mutex;
  struct caml__mutex_unwind* next;
};

#define With_mutex(mutex)                               \
  Assert(caml_local_roots);                             \
  caml_plat_mutex* caml__mutex = (mutex);                    \
  int caml__mutex_go = 1;                               \
  struct caml__mutex_unwind caml__locked_mutex =        \
    { caml__mutex, caml_local_roots->mutexes };         \
  caml_local_roots->mutexes = &caml__locked_mutex;      \
  for (caml_enter_blocking_section(),                   \
         caml_plat_lock(caml__mutex),                  \
         caml_leave_blocking_section();                 \
       caml__mutex_go;                                  \
       caml_plat_unlock(caml__mutex),                  \
         caml__mutex_go = 0,                            \
         caml_local_roots->mutexes =                    \
         caml_local_roots->mutexes->next)


/* One-shot events.
   Once created, can be waited for (once) or triggered (once).
   Events cannot be reused, and are free after being both
   waited for and triggered */

typedef struct {
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int waited, triggered;
} caml_plat_event;

void caml_plat_event_init(caml_plat_event*);
void caml_plat_event_wait(caml_plat_event*);
void caml_plat_event_trigger(caml_plat_event*);


/* Better implementations of shared_stack can use CAS (+ABA protection) or LL/SC */

typedef struct shared_stack_node {
  struct shared_stack_node* next;
} shared_stack_node;

typedef struct shared_stack {
  caml_plat_mutex lock;
  struct shared_stack_node first;
} shared_stack;

#define SHARED_STACK_INIT { PTHREAD_MUTEX_INITIALIZER, { 0 } }

INLINE void shared_stack_init(shared_stack* stk) {
  stk->first.next = 0;
  caml_plat_mutex_init(&stk->lock);
}

INLINE void shared_stack_push(shared_stack* stk, shared_stack_node* node) {
  caml_plat_lock(&stk->lock);
  node->next = stk->first.next;
  stk->first.next = node;
  caml_plat_unlock(&stk->lock);
}

INLINE void* shared_stack_pop(shared_stack* stk) {
  caml_plat_lock(&stk->lock);
  shared_stack_node* n = stk->first.next;
  if (n) {
    stk->first.next = n->next;
  }
  caml_plat_unlock(&stk->lock);
  return n;
}


/* Memory management primitives (mmap) */

uintnat caml_mem_round_up_pages(uintnat size);
void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only);
void caml_mem_commit(void* mem, uintnat size);
void caml_mem_decommit(void* mem, uintnat size);
void caml_mem_unmap(void* mem, uintnat size);

#endif /* CAML_PLATFORM_H */
