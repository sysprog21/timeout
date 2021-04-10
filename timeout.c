/*
 * timeout.c - Tickless hierarchical timing wheel.
 * Copyright (c) 2021 National Cheng Kung University, Taiwan
 * Copyright (c) 2013, 2014  William Ahern
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to permit
 * persons to whom the Software is furnished to do so, subject to the
 * following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
 * NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <limits.h> /* CHAR_BIT */

#include <stddef.h> /* NULL */
#include <stdio.h>  /* FILE fprintf(3) */
#include <stdlib.h> /* malloc(3) free(3) */

#include <inttypes.h> /* UINT64_C uint64_t */

#include <string.h> /* memset(3) */

#include <errno.h> /* errno */

#include <sys/queue.h> /* TAILQ(3) */

#include "timeout.h"

#ifdef TIMEOUT_DISABLE_RELATIVE_ACCESS
#define TO_SET_TIMEOUTS(to, T) ((void) 0)
#else
#define TO_SET_TIMEOUTS(to, T) ((to)->timeouts = (T))
#endif

/* ancillary routines */

#define abstime_t timeout_t /* for documentation purposes */
#define reltime_t timeout_t /* "" */

#if !defined countof
#define countof(a) (sizeof(a) / sizeof *(a))
#endif

#if !defined endof
#define endof(a) (&(a)[countof(a)])
#endif

#if !defined MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#if !defined MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#if !defined TAILQ_CONCAT
#define TAILQ_CONCAT(head1, head2, field)                           \
    do {                                                            \
        if (!TAILQ_EMPTY(head2)) {                                  \
            *(head1)->tqh_last = (head2)->tqh_first;                \
            (head2)->tqh_first->field.tqe_prev = (head1)->tqh_last; \
            (head1)->tqh_last = (head2)->tqh_last;                  \
            TAILQ_INIT((head2));                                    \
        }                                                           \
    } while (0)
#endif

#if !defined TAILQ_FOREACH_SAFE
#define TAILQ_FOREACH_SAFE(var, head, field, tvar) \
    for ((var) = TAILQ_FIRST(head);                \
         (var) && ((tvar) = TAILQ_NEXT(var, field), 1); (var) = (tvar))
#endif


/* bit manipulation routines
 *
 * The macros and routines below implement wheel parameterization. The
 * inputs are:
 *
 *   WHEEL_BIT - The number of value bits mapped in each wheel. The
 *               lowest-order WHEEL_BIT bits index the lowest-order (highest
 *               resolution) wheel, the next group of WHEEL_BIT bits the
 *               higher wheel, etc.
 *
 *   WHEEL_NUM - The number of wheels. WHEEL_BIT * WHEEL_NUM = the number of
 *               value bits used by all the wheels. For the default of 6 and
 *               4, only the low 24 bits are processed. Any timeout value
 *               larger than this will cycle through again.
 *
 * The implementation uses bit fields to remember which slot in each wheel
 * is populated, and to generate masks of expiring slots according to the
 * current update interval (i.e. the "tickless" aspect). The slots to
 * process in a wheel are (populated-set & interval-mask).
 *
 * WHEEL_BIT cannot be larger than 6 bits because 2^6 -> 64 is the largest
 * number of slots which can be tracked in a uint64_t integer bit field.
 * WHEEL_BIT cannot be smaller than 3 bits because of our rotr and rotl
 * routines, which only operate on all the value bits in an integer, and
 * there's no integer smaller than uint8_t.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#if !defined WHEEL_BIT
#define WHEEL_BIT 6
#endif

#if !defined WHEEL_NUM
#define WHEEL_NUM 4
#endif

#define WHEEL_LEN (1U << WHEEL_BIT)
#define WHEEL_MAX (WHEEL_LEN - 1)
#define WHEEL_MASK (WHEEL_LEN - 1)
#define TIMEOUT_MAX ((TIMEOUT_C(1) << (WHEEL_BIT * WHEEL_NUM)) - 1)

#include "bitops.h"

#if WHEEL_BIT == 6
#define ctz(n) ctz64(n)
#define clz(n) clz64(n)
#define fls(n) ((int) (64 - clz64(n)))
#else
#define ctz(n) ctz32(n)
#define clz(n) clz32(n)
#define fls(n) ((int) (32 - clz32(n)))
#endif

#if WHEEL_BIT == 6
#define WHEEL_C(n) UINT64_C(n)
typedef uint64_t wheel_t;
#define rotr(v, c) rotr64((v), (c))
#define rotl(v, c) rotl64((v), (c))

#elif WHEEL_BIT == 5
#define WHEEL_C(n) UINT32_C(n)
typedef uint32_t wheel_t;
#define rotr(v, c) rotr32((v), (c))
#define rotl(v, c) rotl32((v), (c))

#elif WHEEL_BIT == 4
#define WHEEL_C(n) UINT16_C(n)
typedef uint16_t wheel_t;
#define rotr(v, c) rotr16((v), (c))
#define rotl(v, c) rotl16((v), (c))

#elif WHEEL_BIT == 3
#define WHEEL_C(n) UINT8_C(n)
typedef uint8_t wheel_t;
#define rotr(v, c) rotr8((v), (c))
#define rotl(v, c) rotl8((v), (c))

#else
#error invalid WHEEL_BIT value
#endif

/* timer routines */

TAILQ_HEAD(timeout_list, timeout);

struct timeouts {
    struct timeout_list wheel[WHEEL_NUM][WHEEL_LEN], expired;

    wheel_t pending[WHEEL_NUM];

    timeout_t curtime;
    timeout_t hertz;
}; /* struct timeouts */


static struct timeouts *timeouts_init(struct timeouts *T, timeout_t hz)
{
    unsigned i, j;

    for (i = 0; i < countof(T->wheel); i++) {
        for (j = 0; j < countof(T->wheel[i]); j++) {
            TAILQ_INIT(&T->wheel[i][j]);
        }
    }

    TAILQ_INIT(&T->expired);

    for (i = 0; i < countof(T->pending); i++) {
        T->pending[i] = 0;
    }

    T->curtime = 0;
    T->hertz = (hz) ? hz : TIMEOUT_mHZ;

    return T;
} /* timeouts_init() */


struct timeouts *timeouts_open(timeout_t hz, int *error)
{
    struct timeouts *T;

    if ((T = malloc(sizeof *T)))
        return timeouts_init(T, hz);

    *error = errno;

    return NULL;
} /* timeouts_open() */


static void timeouts_reset(struct timeouts *T)
{
    struct timeout_list reset;
    struct timeout *to;
    unsigned i, j;

    TAILQ_INIT(&reset);

    for (i = 0; i < countof(T->wheel); i++) {
        for (j = 0; j < countof(T->wheel[i]); j++) {
            TAILQ_CONCAT(&reset, &T->wheel[i][j], tqe);
        }
    }

    TAILQ_CONCAT(&reset, &T->expired, tqe);

    TAILQ_FOREACH (to, &reset, tqe) {
        to->pending = NULL;
        TO_SET_TIMEOUTS(to, NULL);
    }
} /* timeouts_reset() */


void timeouts_close(struct timeouts *T)
{
    /*
     * NOTE: Delete installed timeouts so timeout_pending() and
     * timeout_expired() worked as expected.
     */
    timeouts_reset(T);

    free(T);
} /* timeouts_close() */


timeout_t timeouts_hz(struct timeouts *T)
{
    return T->hertz;
} /* timeouts_hz() */


void timeouts_del(struct timeouts *T, struct timeout *to)
{
    if (to->pending) {
        TAILQ_REMOVE(to->pending, to, tqe);

        if (to->pending != &T->expired && TAILQ_EMPTY(to->pending)) {
            ptrdiff_t index = to->pending - &T->wheel[0][0];
            int wheel = index / WHEEL_LEN;
            int slot = index % WHEEL_LEN;

            T->pending[wheel] &= ~(WHEEL_C(1) << slot);
        }

        to->pending = NULL;
        TO_SET_TIMEOUTS(to, NULL);
    }
} /* timeouts_del() */

static inline int timeout_wheel(timeout_t curtime, timeout_t expires)
{
    /* must be called with expires > curtime, so fls input is nonzero */
    return (fls(MIN(curtime ^ expires, TIMEOUT_MAX)) - 1) / WHEEL_BIT;
}

static inline int timeout_slot(int wheel, timeout_t expires)
{
    return WHEEL_MASK & ((expires >> (wheel * WHEEL_BIT)));
} /* timeout_slot() */

static void timeouts_sched_nopending(struct timeouts *T, struct timeout *to);
static void timeouts_sched_future_nopending(struct timeouts *T,
                                            struct timeout *to);

static void timeouts_sched(struct timeouts *T,
                           struct timeout *to,
                           timeout_t expires)
{
    timeouts_del(T, to);

    to->expires = expires;

    TO_SET_TIMEOUTS(to, T);

    timeouts_sched_nopending(T, to);
}

/* As 'timeouts_sched_future_nopending', but do not require that to->expires
 * is in the future.
 */
static void timeouts_sched_nopending(struct timeouts *T, struct timeout *to)
{
    if (to->expires > T->curtime) {
        timeouts_sched_future_nopending(T, to);
    } else {
        to->pending = &T->expired;
        TAILQ_INSERT_TAIL(to->pending, to, tqe);
    }
}

/* Insert 'to' into 'T'.  Requres that to->expires is set, and set some time
 * in the future. Requires that to is not pending or expired. Requires that
 * TO_SET_TIMEOUTS has been set.
 */
static void timeouts_sched_future_nopending(struct timeouts *T,
                                            struct timeout *to)
{
    int wheel, slot;

    wheel = timeout_wheel(T->curtime, to->expires);
    slot = timeout_slot(wheel, to->expires);

    to->pending = &T->wheel[wheel][slot];
    TAILQ_INSERT_TAIL(to->pending, to, tqe);

    T->pending[wheel] |= WHEEL_C(1) << slot;
}

#ifndef TIMEOUT_DISABLE_INTERVALS
static void timeouts_readd(struct timeouts *T, struct timeout *to)
{
    to->expires += to->interval;

    if (to->expires <= T->curtime) {
        /* If we've missed the next firing of this timeout, reschedule
         * it to occur at the next multiple of its interval after
         * the last time that it fired.
         */
        timeout_t n = T->curtime - to->expires;
        timeout_t r = n % to->interval;
        to->expires = T->curtime + (to->interval - r);
    }

    timeouts_sched_future_nopending(T, to);
} /* timeouts_readd() */
#endif


void timeouts_add(struct timeouts *T, struct timeout *to, timeout_t timeout)
{
#ifndef TIMEOUT_DISABLE_INTERVALS
    if (to->flags & TIMEOUT_INT)
        to->interval = MAX(1, timeout);
#endif

    if (to->flags & TIMEOUT_ABS)
        timeouts_sched(T, to, timeout);
    else
        timeouts_sched(T, to, T->curtime + timeout);
} /* timeouts_add() */


void timeouts_update(struct timeouts *T, abstime_t curtime)
{
    timeout_t elapsed = curtime - T->curtime;
    struct timeout_list todo;
    int wheel;

    TAILQ_INIT(&todo);

    /*
     * There's no avoiding looping over every wheel. It's best to keep
     * WHEEL_NUM smallish.
     */
    for (wheel = 0; wheel < WHEEL_NUM; wheel++) {
        wheel_t pending;

        /*
         * Calculate the slots expiring in this wheel
         *
         * Assume that current slot is empty, it's timer either expired or
         * moved to the lower wheel. If the elapsed time is greater or equal
         * to the maximum period of the wheel, mark every position as
         * expiring.
         *
         * Otherwise, to determine the expired slots fill in all the
         * bits between the last slot processed and the current
         * slot (exclusive, we don't need to check it again), inclusive of
         * the last slot. We'll bitwise-AND this with our pending set below.
         */
        if ((elapsed >> (wheel * WHEEL_BIT)) >= WHEEL_MAX) {
            pending = (wheel_t) ~WHEEL_C(0);
        } else {
            wheel_t _elapsed;
            int oslot, nslot;

            oslot = WHEEL_MASK & (T->curtime >> (wheel * WHEEL_BIT));
            nslot = WHEEL_MASK & (curtime >> (wheel * WHEEL_BIT));
            _elapsed = WHEEL_MASK & (WHEEL_LEN + nslot - oslot);
            pending = rotl(((WHEEL_C(1) << _elapsed) - 1), oslot + 1);
        }

        while (pending & T->pending[wheel]) {
            /* ctz input cannot be zero: loop condition. */
            int slot = ctz(pending & T->pending[wheel]);
            TAILQ_CONCAT(&todo, &T->wheel[wheel][slot], tqe);
            T->pending[wheel] &= ~(UINT64_C(1) << slot);
        }

        if (!(0x1 & pending))
            break; /* break if we didn't wrap around end of wheel */
    }

    T->curtime = curtime;

    while (!TAILQ_EMPTY(&todo)) {
        struct timeout *to = TAILQ_FIRST(&todo);

        TAILQ_REMOVE(&todo, to, tqe);
        to->pending = NULL;

        timeouts_sched_nopending(T, to);
    }

    return;
} /* timeouts_update() */


void timeouts_step(struct timeouts *T, reltime_t elapsed)
{
    timeouts_update(T, T->curtime + elapsed);
} /* timeouts_step() */


bool timeouts_pending(struct timeouts *T)
{
    wheel_t pending = 0;
    int wheel;

    for (wheel = 0; wheel < WHEEL_NUM; wheel++) {
        pending |= T->pending[wheel];
    }

    return !!pending;
} /* timeouts_pending() */


bool timeouts_expired(struct timeouts *T)
{
    return !TAILQ_EMPTY(&T->expired);
} /* timeouts_expired() */


/*
 * Calculate the interval before needing to process any timeouts pending on
 * any wheel.
 *
 * (This is separated from the public API routine so we can evaluate our
 * wheel invariant assertions irrespective of the expired queue.)
 *
 * This might return a timeout value sooner than any installed timeout if
 * only higher-order wheels have timeouts pending. We can only know when to
 * process a wheel, not precisely when a timeout is scheduled. Our timeout
 * accuracy could be off by 2^(N*M)-1 units where N is the wheel number and
 * M is WHEEL_BIT. Only timeouts which have fallen through to wheel 0 can be
 * known exactly.
 *
 * We should never return a timeout larger than the lowest actual timeout.
 */
static timeout_t timeouts_int(struct timeouts *T)
{
    timeout_t timeout = ~TIMEOUT_C(0), _timeout;
    timeout_t relmask;
    int wheel, slot;

    relmask = 0;

    for (wheel = 0; wheel < WHEEL_NUM; wheel++) {
        if (T->pending[wheel]) {
            slot = WHEEL_MASK & (T->curtime >> (wheel * WHEEL_BIT));

            /* ctz input cannot be zero: T->pending[wheel] is
             * nonzero, so rotr() is nonzero. */
            _timeout = (ctz(rotr(T->pending[wheel], slot)) +
                        !!(T->pending[wheel] & (TIMEOUT_C(1) << slot)))
                       << (wheel * WHEEL_BIT);
            /* +1 when a timeout greater than TIMEOUT_MAX and wrap in to current
             * slot of the highest wheel
             */

            _timeout -= relmask & T->curtime;
            /* reduce by how much lower wheels have progressed */

            timeout = MIN(_timeout, timeout);
        }

        relmask <<= WHEEL_BIT;
        relmask |= WHEEL_MASK;
    }

    return timeout;
} /* timeouts_int() */


/*
 * Calculate the interval our caller can wait before needing to process
 * events.
 */
timeout_t timeouts_timeout(struct timeouts *T)
{
    if (!TAILQ_EMPTY(&T->expired))
        return 0;

    return timeouts_int(T);
} /* timeouts_timeout() */


struct timeout *timeouts_get(struct timeouts *T)
{
    if (!TAILQ_EMPTY(&T->expired)) {
        struct timeout *to = TAILQ_FIRST(&T->expired);

        TAILQ_REMOVE(&T->expired, to, tqe);
        to->pending = NULL;
        TO_SET_TIMEOUTS(to, NULL);

#ifndef TIMEOUT_DISABLE_INTERVALS
        if ((to->flags & TIMEOUT_INT) && to->interval > 0)
            timeouts_readd(T, to);
#endif

        return to;
    } else {
        return 0;
    }
} /* timeouts_get() */


/*
 * Use dumb looping to locate the earliest timeout pending on the wheel so
 * our invariant assertions can check the result of our optimized code.
 */
static struct timeout *timeouts_min(struct timeouts *T)
{
    struct timeout *to, *min = NULL;
    unsigned i, j;

    for (i = 0; i < countof(T->wheel); i++) {
        for (j = 0; j < countof(T->wheel[i]); j++) {
            TAILQ_FOREACH (to, &T->wheel[i][j], tqe) {
                if (!min || to->expires < min->expires)
                    min = to;
            }
        }
    }

    return min;
} /* timeouts_min() */


/*
 * Check some basic algorithm invariants. If these invariants fail then
 * something is definitely broken.
 */
#define report(...)                   \
    do {                              \
        if ((fp))                     \
            fprintf(fp, __VA_ARGS__); \
    } while (0)

#define check(expr, ...)         \
    do {                         \
        if (!(expr)) {           \
            report(__VA_ARGS__); \
            return 0;            \
        }                        \
    } while (0)

bool timeouts_check(struct timeouts *T, FILE *fp)
{
    timeout_t timeout;
    struct timeout *to;

    if ((to = timeouts_min(T))) {
        check(to->expires > T->curtime,
              "missed timeout (expires:%" TIMEOUT_PRIu
              " <= curtime:%" TIMEOUT_PRIu ")\n",
              to->expires, T->curtime);

        timeout = timeouts_int(T);
        check(timeout <= to->expires - T->curtime,
              "wrong soft timeout (soft:%" TIMEOUT_PRIu " > hard:%" TIMEOUT_PRIu
              ") (expires:%" TIMEOUT_PRIu "; curtime:%" TIMEOUT_PRIu ")\n",
              timeout, (to->expires - T->curtime), to->expires, T->curtime);

        timeout = timeouts_timeout(T);
        check(timeout <= to->expires - T->curtime,
              "wrong soft timeout (soft:%" TIMEOUT_PRIu " > hard:%" TIMEOUT_PRIu
              ") (expires:%" TIMEOUT_PRIu "; curtime:%" TIMEOUT_PRIu ")\n",
              timeout, (to->expires - T->curtime), to->expires, T->curtime);
    } else {
        timeout = timeouts_timeout(T);

        if (!TAILQ_EMPTY(&T->expired))
            check(timeout == 0,
                  "wrong soft timeout (soft:%" TIMEOUT_PRIu
                  " != hard:%" TIMEOUT_PRIu ")\n",
                  timeout, TIMEOUT_C(0));
        else
            check(timeout == ~TIMEOUT_C(0),
                  "wrong soft timeout (soft:%" TIMEOUT_PRIu
                  " != hard:%" TIMEOUT_PRIu ")\n",
                  timeout, ~TIMEOUT_C(0));
    }

    return 1;
} /* timeouts_check() */


#define ENTER                            \
    do {                                 \
        static const int pc0 = __LINE__; \
        switch (pc0 + it->pc) {          \
        case __LINE__:                   \
            (void) 0

#define SAVE_AND_DO(do_statement) \
    do {                          \
        it->pc = __LINE__ - pc0;  \
        do_statement;             \
    case __LINE__:                \
        (void) 0;                 \
    } while (0)

#define YIELD(rv) SAVE_AND_DO(return (rv))

#define LEAVE           \
    SAVE_AND_DO(break); \
    }                   \
    }                   \
    while (0)

struct timeout *timeouts_next(struct timeouts *T, struct timeouts_it *it)
{
    struct timeout *to;

    ENTER;

    if (it->flags & TIMEOUTS_EXPIRED) {
        if (it->flags & TIMEOUTS_CLEAR) {
            while ((to = timeouts_get(T))) {
                YIELD(to);
            }
        } else {
            TAILQ_FOREACH_SAFE (to, &T->expired, tqe, it->to) {
                YIELD(to);
            }
        }
    }

    if (it->flags & TIMEOUTS_PENDING) {
        for (it->i = 0; it->i < countof(T->wheel); it->i++) {
            for (it->j = 0; it->j < countof(T->wheel[it->i]); it->j++) {
                TAILQ_FOREACH_SAFE (to, &T->wheel[it->i][it->j], tqe, it->to) {
                    YIELD(to);
                }
            }
        }
    }

    LEAVE;

    return NULL;
} /* timeouts_next */

#undef LEAVE
#undef YIELD
#undef SAVE_AND_DO
#undef ENTER


/* timeout routines */

struct timeout *timeout_init(struct timeout *to, int flags)
{
    memset(to, 0, sizeof *to);

    to->flags = flags;

    return to;
} /* timeout_init() */


#ifndef TIMEOUT_DISABLE_RELATIVE_ACCESS
bool timeout_pending(struct timeout *to)
{
    return to->pending && to->pending != &to->timeouts->expired;
} /* timeout_pending() */


bool timeout_expired(struct timeout *to)
{
    return to->pending && to->pending == &to->timeouts->expired;
} /* timeout_expired() */


void timeout_del(struct timeout *to)
{
    timeouts_del(to->timeouts, to);
} /* timeout_del() */
#endif
