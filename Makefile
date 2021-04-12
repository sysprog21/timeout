CC ?= gcc
CFLAGS = -O2 -g
CFLAGS += -march=native
CFLAGS += -Wall -Wextra -Wno-unused-parameter -Wno-unused-function
# Ensure consistent configurations while testing and benchmarking
CFLAGS += -DWHEEL_BIT=6 -DWHEEL_NUM=4

include mk/common.mk
include mk/autoconf.mk
include mk/lua.mk

OUT ?= out
OBJS := timeout.o
OBJS := $(addprefix $(OUT)/,$(OBJS))
deps := $(OBJS:%.o=%.o.d)
TESTS := $(wildcard tests/test-*.c)
TESTBINS := $(TESTS:%.c=$(OUT)/%)
deps += $(TESTBINS:%=%.o.d)

BENCH_ALGOS = wheel heap llrb ebtree
BENCH_OPS = add del expire
BENCH_DATA := $(addprefix $(OUT)/bench/,$(foreach op,$(BENCH_OPS),$(BENCH_ALGOS:%=%-$(op).dat)))
BENCH_MODS := bench.so $(BENCH_ALGOS:%=bench-%.so)
BENCH_MODS := $(addprefix $(OUT)/bench/,$(BENCH_MODS))
deps += $(BENCH_MODS:%.so=%.o.d)

all: $(OBJS)

CFLAGS := -I. $(CFLAGS)

$(OUT)/%.o: %.c
	$(VECHO) "  CC\t$@\n"
	$(Q)$(CC) -o $@ $(CFLAGS) -c -MMD -MF $@.d $<

SHELL_HACK := $(shell mkdir -p $(OUT) $(OUT)/tests $(OUT)/bench)

$(OUT)/tests/test-%: tests/test-%.c $(OBJS)
	$(VECHO) "  CC+LD\t$@\n"
	$(Q)$(CC) -o $@ $(CFLAGS) -MMD -MF $(@:%=%.o.d) $< $(OBJS) $(LDFLAGS)
	$(VECHO) "Running $@\n"
	$(Q)$@ && $(call pass)

# Add libraries to the Lua environment
LUA_CPATH=$(PWD)/$(OUT)/bench/bench.so
export LUA_CPATH

$(OUT)/bench/%.so: bench/%.c
	$(VECHO) "  CC+LD\t$@\n"
	$(Q)$(CC) -o $@ \
		$(CFLAGS) $(LUA_CFLAGS) -MMD -MF $(@:%.so=%.o.d) $< \
		$(SO_FLAGS) $(LUA_LDFLAGS) $(LDFLAGS)

$(foreach algo,$(BENCH_ALGOS),$(OUT)/bench/bench-$(algo).so): $(OUT)/bench/bench.so

$(OUT)/bench/%-add.dat: $(OUT)/bench/bench-%.so
	$(VECHO) "  GEN\t$@\n"
	$(Q)(cd bench ; $(LUA) bench-add.lua ../$<  > ../$@)
$(OUT)/bench/%-del.dat: $(OUT)/bench/bench-%.so
	$(VECHO) "  GEN\t$@\n"
	$(Q)(cd bench ; $(LUA) bench-del.lua ../$< > ../$@)
$(OUT)/bench/%-expire.dat: $(OUT)/bench/bench-%.so
	$(VECHO) "  GEN\t$@\n"
	$(Q)(cd bench ; $(LUA) bench-expire.lua ../$< > ../$@)

$(OUT)/bench.eps: $(BENCH_DATA)
	$(VECHO) "  PLOT\t$@\n"
	$(Q)(cd $(OUT)/bench; gnuplot ../../bench/bench.plt > ../bench.eps)

$(OUT)/bench.pdf: $(OUT)/bench.eps
	$(VECHO) "  GEN\t$@\n"
	$(Q)ps2pdf $< $@

.PHONY: bench
bench: $(OUT)/bench.pdf

check: $(TESTBINS)

.PHONY: clean
clean:
	-$(RM) $(OBJS) $(deps)
	-$(RM) $(TESTBINS)
	-$(RM) $(OUT)/bench.eps $(OUT)/bench.pdf
	-$(RM) $(OUT)/bench/*.dat
	-$(RM) $(BENCH_MODS)

-include $(deps)
