/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                                                                        */
/*   Copyright 2021 Opsian Ltd                                            */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_EVENTRING_H
#define CAML_EVENTRING_H

#include "eventlog.h"
#include "mlvalues.h"
#include <stdint.h>

#ifdef CAML_INSTR
#define CAML_EV_ALLOC(s) caml_ev_alloc(s)
#define CAML_EV_ALLOC_FLUSH() caml_ev_alloc_flush()
#else
#define CAML_EV_ALLOC(s)      /**/
#define CAML_EV_ALLOC_FLUSH() /**/
#endif

#define CAML_EV_FLUSH() caml_ev_flush()

/* external C-API for reading from the eventring */
struct caml_eventring_cursor;

typedef enum {
  E_SUCCESS = 0,
  E_CURSOR_NOT_OPEN = -1,
  E_CORRUPT_STREAM = -2,
  E_ALLOC_FAIL = -3,
  E_PATH_FAILURE = -4,
  E_STAT_FAILURE = -5
} eventring_error;

/* Starts eventring. Needs to be called before [caml_eventring_create_cursor] */
extern value caml_eventring_start();
extern value caml_eventring_pause();
extern value caml_eventring_resume();

/* [eventring_path] is a path to a directory containing eventrings. [pid] is the
    process id (or equivalent) of the startup OCaml process. This function will
    return a cursor which can we be used with caml_eventring_read_poll to read
    events from the eventrings. */
CAMLextern eventring_error
caml_eventring_create_cursor(const char *eventring_path, int pid,
                             struct caml_eventring_cursor **cursor_res);

/* Set the runtime_begin event callback on the cursor */
extern void caml_eventring_set_runtime_begin(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_phase phase));

/* Set the runtime_end event callback on the cursor */
extern void caml_eventring_set_runtime_end(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_phase phase));

/* Set the runtime_counter event callback on the cursor */
extern void caml_eventring_set_runtime_counter(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, uint64_t timestamp,
             ev_runtime_counter counter, uint64_t val));

/* Set the alloc event callback on the cursor */
extern void
caml_eventring_set_runtime_alloc(struct caml_eventring_cursor *cursor,
                                 int (*f)(int domain_id, void *callback_data,
                                          uint64_t timestamp, uint64_t *sz));

/* Set the lifecycle event callback on the cursor */
extern void caml_eventring_set_lifecycle(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, int64_t timestamp,
              ev_lifecycle lifecycle, int64_t data));

/* Set the lost events callback on the cursor */
extern void caml_eventring_set_lost_events(
    struct caml_eventring_cursor *cursor,
    int (*f)(int domain_id, void *callback_data, int lost_words));

/* frees a cursor obtained from caml_eventring_creator_cursor */
CAMLextern void
caml_eventring_free_cursor(struct caml_eventring_cursor *cursor);

/* polls the eventring pointed to by [cursor] and calls the appropriate callback
    for each new event up to at most [max_events] times.

    Returns the number of events consumed in [events_consumed], if set.

    0 for [max_events] indicates no limit to the number of callbacks. */
CAMLextern eventring_error caml_eventring_read_poll(
    struct caml_eventring_cursor *cursor,
    void *callback_data,
    uint max_events, uint *events_consumed);

/* OCaml API for reading from the eventring */
extern value caml_eventring_create_wrapped_cursor(value path_pid);
extern value caml_eventring_free_wrapped_cursor(value wrapped_cursor);
extern value caml_eventring_read_poll_wrapped(value wrapped_cursor,
                                              value callbacks,
                                              value max_events_option);

#ifdef CAML_INTERNALS

/* Functions for putting runtime data on to the eventring */

void caml_eventring_init();
void caml_eventring_destroy();
void caml_ev_begin(ev_runtime_phase phase);
void caml_ev_end(ev_runtime_phase phase);
void caml_ev_counter(ev_runtime_counter counter, uint64_t val);
void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data);
void caml_ev_alloc(uint64_t sz);
void caml_ev_alloc_flush();
void caml_ev_flush();

#endif /* CAML_INTERNALS */

#endif /*CAML_EVENTRING_H*/
