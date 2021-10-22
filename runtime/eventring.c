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

#define CAML_INTERNALS

#include "caml/eventring.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"

#include <fcntl.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <process.h>
#include <wtypes.h>
#elif defined(HAS_UNISTD)
#include <unistd.h>
#endif

#ifdef HAS_MACH_ABSOLUTE_TIME
#include <mach/mach_time.h>
#elif HAS_POSIX_MONOTONIC_CLOCK
#include <time.h>
#endif

#define RING_FILE_NAME_LEN 4096
#define DEFAULT_RING_BUFFER_SIZE (1 << 20)
#define MAX_MSG_LENGTH (1 << 10)

typedef enum { EV_RUNTIME, EV_USER } ev_category;

struct ring_buffer {
  atomic_uint_fast64_t ring_head;
  atomic_uint_fast64_t ring_tail;
  uint64_t data_offset; /* Offset from this struct to ring buffer's data */
};

struct metadata_header {
  uint64_t version;
  uint64_t max_domains;
  uint64_t
      ring_with_header_size_bytes; /* Ring data plus header size in bytes */
  uint64_t ring_size_elements;     /* Ring size in 64-bit elements */
  uint64_t first_ring_offset; /* Offset from this struct to first ring buffer */
};

/* event header fields (for runtime events):
| -- length (10 bits) -- | runtime or user event (1 bit) | event type (4 bits) |
event id (13 bits)
*/

#define RING_ITEM_LENGTH(header) (((header) >> 54) & ((1UL << 10) - 1))
#define RING_ITEM_IS_RUNTIME(header) !((header) | (1UL << 53))
#define RING_ITEM_IS_USER(header) ((header) | (1UL << 53))
#define RING_ITEM_TYPE(header) (((header) >> 49) & ((1UL << 4) - 1))
#define RING_ITEM_ID(header) (((header) >> 36) & ((1UL << 13) - 1))

static char *eventring_path;
static struct metadata_header *current_metadata = NULL;
static char *current_ring_buffer_loc = NULL;
static int current_ring_total_size;

static atomic_uintnat eventring_enabled = 0;
static atomic_uintnat eventring_paused = 0;

static int64_t time_counter(void) {
#ifdef _WIN32
  static double clock_freq = 0;
  static LARGE_INTEGER now;

  if (clock_freq == 0) {
    LARGE_INTEGER f;
    if (!QueryPerformanceFrequency(&f))
      return 0;
    clock_freq = (1000000000.0 / f.QuadPart);
  };

  if (!QueryPerformanceCounter(&now))
    return 0;
  return (int64_t)(now.QuadPart * clock_freq);

#elif defined(HAS_MACH_ABSOLUTE_TIME)
  static mach_timebase_info_data_t time_base = {0};
  uint64_t now;

  if (time_base.denom == 0) {
    if (mach_timebase_info(&time_base) != KERN_SUCCESS)
      return 0;

    if (time_base.denom == 0)
      return 0;
  }

  now = mach_absolute_time();
  return (int64_t)((now * time_base.numer) / time_base.denom);

#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return (int64_t)t.tv_sec * (int64_t)1000000000 + (int64_t)t.tv_nsec;

#endif
}

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset);

void caml_eventring_init() {
  eventring_path = caml_secure_getenv(T("OCAML_EVENTRING_PATH"));

  if (caml_secure_getenv(T("OCAML_EVENTRING_ENABLED"))) {
    caml_eventring_start();
  }
}

static void teardown_eventring(caml_domain_state *domain_state, void *data,
                               int num_participating,
                               caml_domain_state **participanting_domains) {
  caml_global_barrier();
  if (participanting_domains[0] == domain_state) {
    munmap(current_metadata, current_ring_total_size);
    unlink(current_ring_buffer_loc);

    current_metadata = NULL;

    atomic_store_rel(&eventring_enabled, 0);
  }
  caml_global_barrier();
}

void caml_eventring_destroy() {
  if (atomic_load_acq(&eventring_enabled)) {
    write_to_ring(EV_RUNTIME, EV_LIFECYCLE, EV_RING_STOP, 0, NULL, 0);

    caml_try_run_on_all_domains(&teardown_eventring, NULL, NULL);
  }
}

static void
create_and_start_ring_buffers(caml_domain_state *domain_state, void *data,
                              int num_participating,
                              caml_domain_state **participanting_domains) {
  /* Everyone must be stopped for starting and stopping eventring */
  caml_global_barrier();

  /* Only do this on one domain */
  if (participanting_domains[0] == domain_state) {
    /* Don't initialise eventring twice */
    if( !atomic_load_acq(&eventring_enabled) ) {
      int ring_fd, ret;
      long int pid;

      current_ring_buffer_loc = caml_stat_alloc(RING_FILE_NAME_LEN);

      pid = getpid();

      if (eventring_path) {
        snprintf_os(current_ring_buffer_loc, RING_FILE_NAME_LEN,
                    T("%s/%ld.eventring"), eventring_path, pid);
      } else {
        snprintf_os(current_ring_buffer_loc, RING_FILE_NAME_LEN,
                    T("%ld.eventring"), pid);
      }

      current_ring_total_size =
          Max_domains * (DEFAULT_RING_BUFFER_SIZE * sizeof(uint64_t) +
                        sizeof(struct ring_buffer)) +
          sizeof(struct metadata_header);

      ring_fd =
          open(current_ring_buffer_loc, O_RDWR | O_CREAT, (S_IRUSR | S_IWUSR));
      caml_stat_free(current_ring_buffer_loc);

      if (ring_fd < 0) {
        caml_fatal_error("Couldn't open ring buffer loc: %s",
                        current_ring_buffer_loc);
      }

      ret = ftruncate(ring_fd, current_ring_total_size);

      if (ret < 0) {
        caml_fatal_error("Can't resize ring buffer");
      }

      current_metadata = mmap(NULL, current_ring_total_size,
                              PROT_READ | PROT_WRITE, MAP_SHARED, ring_fd, 0);

      current_metadata->version = 1;
      current_metadata->max_domains = Max_domains;
      current_metadata->ring_with_header_size_bytes =
          DEFAULT_RING_BUFFER_SIZE * sizeof(uint64_t) +
          sizeof(struct ring_buffer);
      current_metadata->ring_size_elements = DEFAULT_RING_BUFFER_SIZE;
      current_metadata->first_ring_offset = sizeof(struct metadata_header);

      for (int domain_num = 0; domain_num < Max_domains; domain_num++) {
        struct ring_buffer *ring_buffer =
            (struct ring_buffer
                *)((char *)current_metadata +
                    current_metadata->first_ring_offset +
                    domain_num * current_metadata->ring_with_header_size_bytes);

        ring_buffer->ring_head = 0;
        ring_buffer->ring_tail = 0;
        ring_buffer->data_offset = sizeof(struct ring_buffer);
      }

      close(ring_fd);

      atomic_store_rel(&eventring_enabled, 1);
      atomic_store_rel(&eventring_paused, 0);

      caml_ev_lifecycle(EV_RING_START, pid);
    }
  }
  caml_global_barrier();
}

CAMLprim value caml_eventring_start() {
  if( !atomic_load_acq(&eventring_enabled) ) {
    caml_try_run_on_all_domains(&create_and_start_ring_buffers, NULL, NULL);
  }

  return Val_unit;
}

/* TODO: Should this be a STW? */
CAMLprim value caml_eventring_pause() {
  if (atomic_load_acq(&eventring_enabled) &&
      !atomic_load_acq(&eventring_paused)) {
    caml_ev_lifecycle(EV_RING_PAUSE, 0);
    atomic_store_rel(&eventring_paused, 1);
  }

  return Val_unit;
}

/* TODO: Should this be a STW? */
CAMLprim value caml_eventring_resume() {
  if (atomic_load_acq(&eventring_enabled) &&
      atomic_load_acq(&eventring_paused)) {
    caml_ev_lifecycle(EV_RING_RESUME, 0);
    atomic_store_rel(&eventring_paused, 0);
  }

  return Val_unit;
}

struct ring_buffer *get_ring_buffer_by_domain_id(int domain_id) {
  return (
      struct ring_buffer *)((char *)current_metadata +
                            current_metadata->first_ring_offset +
                            domain_id *
                                current_metadata->ring_with_header_size_bytes);
}

static void write_to_ring(ev_category category, ev_message_type type,
                          int event_id, int event_length, uint64_t *content,
                          int word_offset) {
  /* account for header and timestamp */
  uint64_t length_with_header_ts = event_length + 2;

  struct ring_buffer *domain_ring_buffer =
      get_ring_buffer_by_domain_id(Caml_state->id);

  uint64_t *ring_ptr = (uint64_t *)((char *)domain_ring_buffer +
                                    domain_ring_buffer->data_offset);

  uint64_t ring_head = atomic_load_explicit(&domain_ring_buffer->ring_head,
                                            memory_order_acquire);
  uint64_t ring_tail = atomic_load_explicit(&domain_ring_buffer->ring_tail,
                                            memory_order_acquire);

  uint64_t ring_mask = current_metadata->ring_size_elements - 1;
  uint64_t ring_tail_offset = ring_tail & ring_mask;
  uint64_t ring_distance_to_end =
      current_metadata->ring_size_elements - ring_tail_offset;
  uint64_t padding_required = 0;

  uint64_t timestamp = time_counter();

  /* length must be less than 2^10 */
  CAMLassert(event_length < MAX_MSG_LENGTH);
  /* Runtime event with type EV_INTERNAL and id 0 is reserved for padding */
  CAMLassert(!(category == EV_RUNTIME && type == EV_INTERNAL && event_id == 0));

  // Work out if padding is required
  if (ring_distance_to_end < length_with_header_ts) {
    padding_required = ring_distance_to_end;
  }

  // First we check if a write would take us over the head
  while ((ring_tail + length_with_header_ts + padding_required) - ring_head >=
         DEFAULT_RING_BUFFER_SIZE) {
    // The write would over-write some old bit of data. Need to advance the
    // head.
    uint64_t head_header = ring_ptr[ring_head & ring_mask];

    ring_head += RING_ITEM_LENGTH(head_header);

    atomic_store_explicit(&domain_ring_buffer->ring_head, ring_head,
                          memory_order_release); // advance the ring head
  }

  if (padding_required > 0) {
    ring_ptr[ring_tail_offset] =
        (ring_distance_to_end
         << 54); // Padding header with size ring_distance_to_end
                 // Readers will skip the message and go straight
                 // to the beginning of the ring.

    ring_tail += ring_distance_to_end;

    atomic_store_explicit(&domain_ring_buffer->ring_tail, ring_tail,
                          memory_order_release);

    ring_tail_offset = 0;
  }

  // Write header
  // TODO: Describe this properly with reference to the header structure
  ring_ptr[ring_tail_offset++] = (((uint64_t)length_with_header_ts) << 54) |
                                 ((category == EV_RUNTIME) ? 0 : (1ULL << 53)) |
                                 ((uint64_t)type) << 49 |
                                 ((uint64_t)event_id) << 36;
  ring_ptr[ring_tail_offset++] = timestamp;
  if (content != NULL) {
    memcpy(&ring_ptr[ring_tail_offset], content + word_offset,
           event_length * sizeof(uint64_t));
  }
  atomic_store_explicit(&domain_ring_buffer->ring_tail,
                        ring_tail + length_with_header_ts,
                        memory_order_release);
}

/* Functions for putting runtime data on to the eventring */

void caml_ev_begin(ev_runtime_phase phase) {
  if (atomic_load_acq(&eventring_enabled) &&
      !atomic_load_acq(&eventring_paused)) {
    write_to_ring(EV_RUNTIME, EV_BEGIN, phase, 0, NULL, 0);
  }
}

void caml_ev_end(ev_runtime_phase phase) {
  if (atomic_load_acq(&eventring_enabled) &&
      !atomic_load_acq(&eventring_paused)) {
    write_to_ring(EV_RUNTIME, EV_EXIT, phase, 0, NULL, 0);
  }
}

void caml_ev_counter(ev_runtime_counter counter, uint64_t val) {
  if (atomic_load_acq(&eventring_enabled) &&
      !atomic_load_acq(&eventring_paused)) {
    uint64_t buf[1];
    buf[0] = val;

    write_to_ring(EV_RUNTIME, EV_COUNTER, counter, 1, buf, 0);
  }
}

void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data) {
  if (atomic_load_acq(&eventring_enabled) &&
      !atomic_load_acq(&eventring_paused)) {
    write_to_ring(EV_RUNTIME, EV_LIFECYCLE, lifecycle, 1, (uint64_t *)&data, 0);
  }
}

#define NUM_ALLOC_BUCKETS 20
static uint64_t alloc_buckets[NUM_ALLOC_BUCKETS] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

/* This function records allocations in caml_alloc_shr_aux in given bucket sizes
   These buckets are meant to be flushed explicitly by the caller through the
   caml_ev_alloc_flush function. Until then the buckets are just updated until
   flushed.
*/
void caml_ev_alloc(uint64_t sz) {
  if (!atomic_load_acq(&eventring_enabled))
    return;
  if (atomic_load_acq(&eventring_paused))
    return;

  if (sz < (NUM_ALLOC_BUCKETS / 2)) {
    ++alloc_buckets[sz];
  } else if (sz < (NUM_ALLOC_BUCKETS * 10 / 2)) {
    ++alloc_buckets[sz / (NUM_ALLOC_BUCKETS / 2) + (NUM_ALLOC_BUCKETS / 2 - 1)];
  } else {
    ++alloc_buckets[NUM_ALLOC_BUCKETS - 1];
  }
}

/*  Note that this function does not trigger an actual disk flush, it just
    pushes events in the event buffer.
*/
void caml_ev_alloc_flush() {
  int i;

  if (!atomic_load_acq(&eventring_enabled))
    return;
  if (atomic_load_acq(&eventring_paused))
    return;

  write_to_ring(EV_RUNTIME, EV_ALLOC, 0, NUM_ALLOC_BUCKETS, alloc_buckets, 0);

  for (i = 1; i < NUM_ALLOC_BUCKETS; i++) {
    alloc_buckets[i] = 0;
  }
}

void caml_ev_flush() {
  // This is a no-op for eventring
}

struct caml_eventring_cursor {
  int cursor_open; /* has this cursor been opened? */
  struct metadata_header *metadata; /* pointer to the ring metadata */
  uint64_t *current_positions; /* positions in the rings for each domain */
  size_t ring_file_size_bytes; /* the size of the eventring file in bytes */
  int next_read_domain; /* the last domain we read from */
};

/* C-API for reading from an eventring */

/* [eventring_path] is a path to a directory containing eventrings. [pid] is the
    process id (or equivalent) of the startup OCaml process. This function will
    return a cursor which can we be used with [caml_eventring_read_poll] to read
    events from the eventrings. */
struct caml_eventring_cursor *
caml_eventring_create_cursor(const char *eventring_path, int pid) {
  int ring_fd, ret;
  struct stat tmp_stat;
  // TODO: Move from caml_stat_alloc_noexc to malloc
  struct caml_eventring_cursor *cursor =
      caml_stat_alloc_noexc(sizeof(struct caml_eventring_cursor));
  char *eventring_loc;

  if (cursor == NULL) {
    // TODO: Log here? Or just pass a pointer to an eventring_cursor pointer
    return NULL;
  }

  eventring_loc = caml_stat_alloc_noexc(RING_FILE_NAME_LEN);

  if (eventring_loc == NULL) {
    // TODO: Log here?
    return NULL;
  }

  if (pid < 0) {
    pid = getpid();
  }

  if (eventring_path) {
    ret = snprintf_os(eventring_loc, RING_FILE_NAME_LEN, T("%s/%d.eventring"),
                      eventring_path, pid);
  } else {
    ret =
        snprintf_os(eventring_loc, RING_FILE_NAME_LEN, T("%d.eventring"), pid);
  }

  if (ret < 0) {
    // TODO: Need an error here
    caml_stat_free(cursor);
    caml_stat_free(eventring_loc);
    return 0;
  }

  ring_fd = open(eventring_loc, O_RDONLY, 0);
  ret = fstat(ring_fd, &tmp_stat);

  if (ret < 0) {
    // TODO: Need error here
    caml_stat_free(cursor);
    caml_stat_free(eventring_loc);
    return 0;
  }

  cursor->ring_file_size_bytes = tmp_stat.st_size;
  cursor->metadata = mmap(NULL, cursor->ring_file_size_bytes, PROT_READ,
                          MAP_SHARED, ring_fd, 0);
  cursor->current_positions =
      caml_stat_alloc(cursor->metadata->max_domains * sizeof(uint64_t));
  for (int j = 0; j < cursor->metadata->max_domains; j++) {
    cursor->current_positions[j] = 0;
  }
  cursor->cursor_open = 1;
  cursor->next_read_domain = 0;

  return cursor;
}

/* frees a cursor obtained from caml_eventring_reader_create */
void caml_eventring_free_cursor(struct caml_eventring_cursor *cursor) {
  if (cursor->cursor_open) {
    cursor->cursor_open = 0;
    munmap(cursor->metadata, cursor->ring_file_size_bytes);
    caml_stat_free(cursor->current_positions);
    caml_stat_free(cursor);
  }
}

/* polls the eventring pointed to by [cursor] and calls the appropriate callback
    provided in [callbacks] for each new event up to at most [max_events] times.
    Returns the number of events consumed.

    0 for [max_events] indicates no limit to the number of callbacks. */
uint caml_eventring_read_poll(struct caml_eventring_cursor *cursor,
                              struct caml_eventring_callbacks *callbacks,
                              void *callback_data, uint max_events) {
  int events_consumed = 0;
  int start_domain = cursor->next_read_domain;
  uint64_t ring_head, ring_tail;
  char *raw_header_ptr;

  if (!cursor->cursor_open) {
    /* TODO: Should probably log something here as well */
    return 0;
  }

  raw_header_ptr =
      (char *)cursor->metadata + cursor->metadata->first_ring_offset;

  /* this loop looks a bit odd because we're iterating from the last domain
     that we read from on the last read_poll call and then looping around. */
  for (int i = 0; i < cursor->metadata->max_domains;
       i++) {
    int domain_num = (start_domain + i) % cursor->metadata->max_domains;

    struct ring_buffer *ring_buffer_header =
        (struct ring_buffer *)raw_header_ptr;
    uint64_t *ring_ptr = (uint64_t *)((char *)ring_buffer_header +
                                      ring_buffer_header->data_offset);

    do {
      uint64_t buf[MAX_MSG_LENGTH];
      uint64_t ring_mask, header, msg_length;
      ring_head = atomic_load_explicit(&ring_buffer_header->ring_head,
                                       memory_order_acquire);
      ring_tail = atomic_load_explicit(&ring_buffer_header->ring_tail,
                                       memory_order_acquire);

      if (ring_head > cursor->current_positions[domain_num]) {
        if (callbacks->ev_lost_events) {
          callbacks->ev_lost_events(domain_num,
              callback_data, ring_head - cursor->current_positions[domain_num]);
        }
        cursor->current_positions[domain_num] = ring_head;
      }

      if( cursor->current_positions[domain_num] >= ring_tail ) {
        break;
      }

      ring_mask = current_metadata->ring_size_elements - 1;
      header = ring_ptr[cursor->current_positions[domain_num] & ring_mask];
      msg_length = RING_ITEM_LENGTH(header);

      if (msg_length > MAX_MSG_LENGTH) {
        // TODO: fatal error here, the stream is corrupt or our position is
        // wrong.
      }

      memcpy(buf,
             ring_ptr + (cursor->current_positions[domain_num] & ring_mask),
             msg_length * sizeof(uint64_t));

      ring_head = atomic_load_explicit(&ring_buffer_header->ring_head,
                                       memory_order_acquire);

      /* Check the message we've read hasn't been overwritten by the writer */
      if (ring_head > cursor->current_positions[domain_num]) {
        /* It potentially has, retry for the next one after we've notified
             the callbacks about lost messages. */
        int lost_words = ring_head - cursor->current_positions[domain_num];
        cursor->current_positions[domain_num] = ring_head;

        if (callbacks->ev_lost_events) {
          callbacks->ev_lost_events(domain_num, callback_data, lost_words);
        }
        break;
      }

      switch (RING_ITEM_TYPE(header)) {
      case EV_BEGIN:
        if (callbacks->ev_runtime_begin) {
          callbacks->ev_runtime_begin(domain_num, callback_data, buf[1],
                                      RING_ITEM_ID(header));
        }
        break;
      case EV_EXIT:
        if (callbacks->ev_runtime_end) {
          callbacks->ev_runtime_end(domain_num, callback_data, buf[1],
                                    RING_ITEM_ID(header));
        }
        break;
      case EV_COUNTER:
        if (callbacks->ev_runtime_counter) {
          callbacks->ev_runtime_counter(domain_num, callback_data, buf[1],
                                        RING_ITEM_ID(header), buf[2]);
        }
        break;
      case EV_ALLOC:
        if (callbacks->ev_alloc) {
          callbacks->ev_alloc(domain_num, callback_data, buf[1], &buf[2]);
        }
        break;
      case EV_LIFECYCLE:
        if (callbacks->ev_lifecycle) {
          callbacks->ev_lifecycle(domain_num, callback_data, buf[1],
                                  RING_ITEM_ID(header), buf[2]);
        }
      }

      if (RING_ITEM_TYPE(header) != EV_INTERNAL) {
        events_consumed++;
      }

      cursor->current_positions[domain_num] += msg_length;

    /* There is an unfairness here. Under heavy load situations we might only
    end up reading [max_events] from a single domain's ring. */
    // TODO: Leave the next domain number to read from in the cursor
    } while (cursor->current_positions[domain_num] < ring_tail &&
             (max_events == 0 || events_consumed < max_events));

    cursor->next_read_domain = (domain_num+1) % cursor->metadata->max_domains;

    raw_header_ptr += cursor->metadata->ring_with_header_size_bytes;
  }

  return events_consumed;
}

#define Cursor_val(v) (*((struct caml_eventring_cursor **)Data_custom_val(v)))

static void finalise_cursor(value v) {
  struct caml_eventring_cursor *cursor = Cursor_val(v);

  if (cursor != NULL) {
    caml_eventring_free_cursor(cursor);

    Cursor_val(v) = NULL;
  }
}

static struct custom_operations cursor_operations = {
    "eventring.cursor",         finalise_cursor,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_eventring_create_wrapped_cursor(value path_pid_option) {
  CAMLparam0();
  CAMLlocal1(wrapper);
  struct caml_eventring_cursor *cursor;
  int pid;
  const char *path;

  wrapper = caml_alloc_custom(&cursor_operations,
                              sizeof(struct caml_eventring_cursor *), 0, 1);

  if (Is_some(path_pid_option)) {
    path = String_val(Field(path_pid_option, 0));
    pid = Int_val(Field(path_pid_option, 1));
  } else {
    path = NULL;
    pid = -1;
  }

  cursor = caml_eventring_create_cursor(path, pid);

  if (cursor == NULL) {
    caml_failwith("Could not obtain cursor");
  }

  Cursor_val(wrapper) = cursor;

  CAMLreturn(wrapper);
}

CAMLprim value caml_eventring_free_wrapped_cursor(value wrapped_cursor) {
  CAMLparam1(wrapped_cursor);

  struct caml_eventring_cursor *cursor = Cursor_val(wrapped_cursor);

  if (cursor != NULL) {
    caml_eventring_free_cursor(cursor);
    Cursor_val(wrapped_cursor) = NULL;
  }

  CAMLreturn(Val_unit);
};

static void ml_runtime_begin(int domain_id, void *callback_data,
                            uint64_t timestamp, ev_runtime_phase phase) {
  CAMLparam0();
  CAMLlocal4(tmp_callback, ts_val, msg_type, callbacks_root);

  callbacks_root = *((value*)callback_data);

  tmp_callback = Field(callbacks_root, 0); /* ev_runtime_begin */
  if (Is_some(tmp_callback)) {
    ts_val = caml_copy_int64(timestamp);
    msg_type = Val_long(phase);

    caml_callback3(Some_val(tmp_callback), Val_long(domain_id), ts_val, msg_type);
  }

  CAMLreturn0;
}

static void ml_runtime_end(int domain_id, void *callback_data,
                          uint64_t timestamp, ev_runtime_phase phase) {
  CAMLparam0();
  CAMLlocal4(tmp_callback, ts_val, msg_type, callbacks_root);

  callbacks_root = *((value*)callback_data);

  tmp_callback = Field(callbacks_root, 1); /* ev_runtime_end */
  if (Is_some(tmp_callback)) {
    ts_val = caml_copy_int64(timestamp);
    msg_type = Val_long(phase);

    caml_callback3(Some_val(tmp_callback), Val_long(domain_id), ts_val, msg_type);
  }

  CAMLreturn0;
}

static void ml_runtime_counter(int domain_id, void *callback_data,
                              uint64_t timestamp, ev_runtime_counter counter,
                              uint64_t val) {
  CAMLparam0();
  CAMLlocal2(tmp_callback, callbacks_root);
  CAMLlocalN(params, 4);

  callbacks_root = *((value*)callback_data);

  tmp_callback = Field(callbacks_root, 2); /* ev_runtime_counter */
  if (Is_some(tmp_callback)) {
    params[0] = Val_long(domain_id);
    params[1] = caml_copy_int64(timestamp);
    params[2] = Val_long(counter);
    params[3] = Val_long(val);

    caml_callbackN(Some_val(tmp_callback), 4, params);
  }

  CAMLreturn0;
}

static void ml_alloc(int domain_id, void *callback_data, uint64_t timestamp,
                    uint64_t *sz) {
  CAMLparam0();
  CAMLlocal4(tmp_callback, ts_val, misc_val, callbacks_root);

  callbacks_root = *((value*)callback_data);

  tmp_callback = Field(callbacks_root, 3); /* ev_alloc */
  if (Is_some(tmp_callback)) {
    int i;

    ts_val = caml_copy_int64(timestamp);
    misc_val = caml_alloc(NUM_ALLOC_BUCKETS, 0);

    for (i = 0; i < NUM_ALLOC_BUCKETS; i++) {
      Store_field(misc_val, i, Val_long(sz[i]));
    }

    caml_callback3(Some_val(tmp_callback), Val_long(domain_id), ts_val, misc_val);
  }

  CAMLreturn0;
}

static void ml_lifecycle(int domain_id, void *callback_data, int64_t timestamp,
                         ev_lifecycle lifecycle, int64_t data) {
  CAMLparam0();
  CAMLlocal2(tmp_callback, callbacks_root);
  CAMLlocalN(params, 4);

  callbacks_root = *((value*)callback_data);

  tmp_callback = Field(callbacks_root, 4); /* ev_lifecycle */
  if (Is_some(tmp_callback)) {
    params[0] = Val_long(domain_id);
    params[1] = caml_copy_int64(timestamp);
    params[2] = Val_long(lifecycle);
    if (data != 0) {
      params[3] = caml_alloc(1, 0);
      Store_field(params[3], 0, Val_long(data));
    } else {
      params[3] = Val_none;
    }

    caml_callbackN(Some_val(tmp_callback), 4, params);
  }

  CAMLreturn0;
}

static void ml_lost_events(int domain_id, void *callback_data, int lost_words){
  CAMLparam0();
  CAMLlocal2(tmp_callback, callbacks_root);

  callbacks_root = *((value*)callback_data);

  tmp_callback = Field(callbacks_root, 5); /* lost_events */

  if (Is_some(tmp_callback)) {
    caml_callback2(Some_val(tmp_callback), Val_long(domain_id), Val_long(lost_words));
  }

  CAMLreturn0;
}

static struct caml_eventring_callbacks local_callbacks = {
  ev_runtime_begin : ml_runtime_begin,
  ev_runtime_end : ml_runtime_end,
  ev_runtime_counter : ml_runtime_counter,
  ev_alloc : ml_alloc,
  ev_lifecycle : ml_lifecycle,
  ev_lost_events : ml_lost_events
};

CAMLprim value caml_eventring_read_poll_wrapped(value wrapped_cursor,
                                                value callbacks_val,
                                                value max_events_val) {
  CAMLparam2(wrapped_cursor, callbacks_val);
  CAMLlocal5(tmp_callback, ts_val, msg_type, counter_val, misc_val);

  int events_consumed = 0;
  int max_events = Is_some(max_events_val) ? Some_val(max_events_val) : 0;
  struct caml_eventring_cursor *cursor = Cursor_val(wrapped_cursor);

  if (cursor == NULL) {
    caml_failwith("Invalid or closed cursor");
  }

  if (!cursor->cursor_open) {
    caml_failwith("Eventring cursor is not open");
  }

  events_consumed =
      caml_eventring_read_poll(cursor, &local_callbacks, &callbacks_val, max_events);

  CAMLreturn(Int_val(events_consumed));
};