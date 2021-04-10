# timeout

Tickless Hierarchical Timing Wheel.

## Description

`timeout` implements hierarchical timing wheels as described in
"[Hashed and Hierarchical Timing Wheels: Data Structures for the Efficient Implementation of a Timer Facility](http://www.cs.columbia.edu/~nahum/w6998/papers/ton97-timing-wheels.pdf)"
by George Varghese and Tony Lauck, ACM 089791-242-X/87/0011/0025, pp. 25-38.
This data structure implements timers in O(1) worst-case time for all
operations including insertion, deletion, and expiration.

`timeout` is tickless. Traditional implementations utilize an external
periodic timer which interrupts at intervals equal to the granularity of the
clock to progress the timing wheel by one tick. `timeout` uses bitmaps and
bitfield manipulation to optimize aperiodic wheel progression, and in
particular reduces calculation of the next minimum update interval to
a small O(1) in the worst case. This makes timing wheels practicable.

Why a timing wheel? The critical feature of timing wheels is O(1)
insertion and deletion. Timeouts rarely expire in network server software;
they are hedges by software for when other expected events fail to occur.
Timeouts are often installed and cancelled repeatedly for even the simplest
of actions. But the typical timeout implementation uses a red-black tree or
priority queue, where insertion and deletion are O(log N) operations.
Timing wheels are considerably more efficient algorithmically, while this
implementation in particular tries to address potential fixed cost and
latency issues, particularly for sparsely populated wheels.

## License

`timeout` is released under the MIT License. Use of this source code is
governed by a MIT-style license that can be found in the LICENSE file.

External source code used for benchmarking:
- `bench/bench-heap.c`: written by Maxim Yegorushki. 3-Clause BSD License.
- `bench/bench-ebtree.c`: written by Willy Tarreau. GNU GPL v2.
