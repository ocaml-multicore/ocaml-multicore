#ifndef CAML_LOCKFREE_queue_H
#define CAML_LOCKFREE_queue_H

#ifdef CAML_INTERNALS

#include "camlatomic.h"
#include "platform.h"

struct lockfree_queue {
    atomic_intptr_t head;
};

void lockfree_queue_init(struct lockfree_queue* queue);
void lockfree_queue_push(struct lockfree_queue* queue, void* queue_item);
void* lockfree_queue_pop(struct lockfree_queue* queue);

#endif /* CAML_INTERNALS */
#endif /* CAML_SHARED_HEAP_H */
