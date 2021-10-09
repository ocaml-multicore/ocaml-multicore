#define CAML_NAME_SPACE

#include "caml/alloc.h"
#include "caml/eventring.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#include <assert.h>

struct counters {
    int minor_started;
    int major_started;
    int compact_started;
    int minors;
    int majors;
    int compacts;
};

void start_eventring() {
    caml_eventring_start();
}

void ev_begin(void* callback_data, uint64_t timestamp, ev_runtime_phase phase) {
    struct counters* tmp_counters = (struct counters*)callback_data;
    switch( phase ) {
        case EV_MINOR:
            tmp_counters->minor_started = 1;
            break;
        case EV_MAJOR:
            tmp_counters->major_started = 1;
            break;
        case EV_COMPACT_MAIN:
            tmp_counters->compact_started = 1;
            break;
    }
}

void ev_end(void* callback_data, uint64_t timestamp, ev_runtime_phase phase) {
    struct counters* tmp_counters = (struct counters*)callback_data;
    switch( phase ) {
        case EV_MINOR:
            assert(tmp_counters->minor_started);
            tmp_counters->minor_started = 0;
            tmp_counters->minors++;
            break;
        case EV_MAJOR:
            assert(tmp_counters->major_started);
            tmp_counters->major_started = 0;
            tmp_counters->majors++;
            break;
        case EV_COMPACT_MAIN:
            assert(tmp_counters->compact_started);
            tmp_counters->compact_started = 0;
            tmp_counters->compacts++;
            break;
    }
}

value get_event_counts(void) {
    CAMLparam0();
    CAMLlocal1(counts_tuple);
    counts_tuple = caml_alloc_small(3, 0);

    struct caml_eventring_cursor* cursor = caml_eventring_create_cursor(NULL, -1);

    if( !cursor ) {
        caml_failwith("invalid or non-existent cursor");
    }

    struct caml_eventring_callbacks callbacks = { 0 };
    struct counters tmp_counters = { 0 };

    callbacks.ev_runtime_begin = ev_begin;
    callbacks.ev_runtime_end = ev_end;

    int read_events = caml_eventring_read_poll(cursor, &callbacks, &tmp_counters, 0);

    Field(counts_tuple, 0) = Val_long(tmp_counters.minors);
    Field(counts_tuple, 1) = Val_long(tmp_counters.majors);
    Field(counts_tuple, 2) = Val_long(tmp_counters.compacts);

    caml_eventring_free_cursor(cursor);

    CAMLreturn(counts_tuple);
}