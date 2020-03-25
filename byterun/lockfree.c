#define CAML_INTERNALS

#include "caml/domain.h"
#include "caml/lockfree.h"
#include <stdint.h>

/* Simple Treiber queue for now (a LIFO queue to match existing linked list semantics) */

struct queue_item {
  atomic_intptr_t next;
};

void lockfree_queue_init(struct lockfree_queue* queue) {
    queue->head = 0;
}

void lockfree_queue_push(struct lockfree_queue* queue, void* queue_item) {
    struct queue_item* new_item = (struct queue_item*)queue_item;

    if( caml_domain_alone() ) 
    {
        new_item->next = atomic_load_explicit(&queue->head, memory_order_relaxed);
        atomic_store_explicit(&queue->head, (intptr_t)new_item, memory_order_relaxed);
    }
    else 
    {
        intptr_t current_head;
        do {
            current_head = atomic_load_explicit(&queue->head, memory_order_acquire);

            atomic_store_explicit(&new_item->next, (intptr_t)current_head, memory_order_release);
        } while(!atomic_compare_exchange_strong(&queue->head, (intptr_t*)&current_head, (intptr_t)new_item));
    }
}

void* lockfree_queue_pop(struct lockfree_queue* queue) {
    if( caml_domain_alone() )
    {
        struct queue_item* current_head = (struct queue_item*)atomic_load_explicit(&queue->head, memory_order_relaxed);

        if( current_head == NULL ) {
            return 0;
        }

        atomic_store_explicit(&queue->head, (intptr_t)current_head->next, memory_order_relaxed);

        return current_head;
    }
    else
    {
        intptr_t current_head;
        intptr_t new_head;

        do {
            current_head = atomic_load_explicit(&queue->head, memory_order_acquire);

            if( current_head == 0 ) {
                return 0;
            }
            
            new_head = atomic_load_explicit(&(((struct queue_item*)current_head)->next), memory_order_acquire);
        } while(!atomic_compare_exchange_strong(&queue->head, (intptr_t*)&current_head, (intptr_t)new_head));

        return (void*)current_head;
    }
}