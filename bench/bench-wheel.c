#include <stdlib.h>

#include "timeout.c"

#include "bench.h"

static void *init(struct timeout *timeout, size_t count, int verbose)
{
    struct timeouts *T;
    size_t i;
    int error;

    T = timeouts_open(TIMEOUT_mHZ, &error);

    for (i = 0; i < count; i++)
        timeout_init(&timeout[i], 0);

    return T;
}

static void add(void *T, struct timeout *to, timeout_t expires)
{
    timeouts_add(T, to, expires);
}

static void del(void *T, struct timeout *to)
{
    timeouts_del(T, to);
}

static struct timeout *get(void *T)
{
    return timeouts_get(T);
}

static void update(void *T, timeout_t ts)
{
    timeouts_update(T, ts);
}

static void(check)(void *T)
{
    if (!timeouts_check(T, stderr))
        _Exit(1);
}

static int empty(void *T)
{
    return !(timeouts_pending(T) || timeouts_expired(T));
}

static struct timeout *next(void *T, struct timeouts_it *it)
{
    return timeouts_next(T, it);
}

static void destroy(void *T)
{
    timeouts_close(T);
}

const struct benchops benchops = {
    .init = &init,
    .add = &add,
    .del = &del,
    .get = &get,
    .update = &update,
    .check = &check,
    .empty = &empty,
    .next = &next,
    .destroy = &destroy,
};
