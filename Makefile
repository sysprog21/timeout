# NOTE: GNU Make 3.81 won't export MAKEFLAGS if .POSIX is specified, but
# Solaris make won't export MAKEFLAGS unless .POSIX is specified.
$(firstword ignore).POSIX:

.DEFAULT_GOAL = all

.SUFFIXES:

all:

#
# USER-MODIFIABLE MACROS
#
top_srcdir = .
top_builddir = .

CFLAGS = -O2 -march=native -g -Wall -Wextra -Wno-unused-parameter -Wno-unused-function
SOFLAGS = $$(auto_soflags)
LIBS = $$(auto_libs)

ALL_CPPFLAGS = -I$(top_srcdir) -DWHEEL_BIT=$(WHEEL_BIT) -DWHEEL_NUM=$(WHEEL_NUM) $(CPPFLAGS)
ALL_CFLAGS = $(CFLAGS)
ALL_SOFLAGS = $(SOFLAGS)
ALL_LDFLAGS = $(LDFLAGS)
ALL_LIBS = $(LIBS)

LUA_API = 5.3
LUA = lua$(LUA_API)
LUA_CPPFLAGS += -I/usr/include/$(LUA)

WHEEL_BIT = 6
WHEEL_NUM = 4

RM = rm -f

# END MACROS

SHRC = \
	top_srcdir="$(top_srcdir)"; \
	top_builddir="$(top_builddir)"; \
	. "$${top_srcdir}/Rules.shrc"

include $(top_srcdir)/lua/Rules.mk
include $(top_srcdir)/bench/Rules.mk

all: test-timeout test-bitops

timeout.o: $(top_srcdir)/timeout.c

timeout.o: $(top_srcdir)/timeout.c $(top_srcdir)/bitops.h $(top_srcdir)/timeout.h
	@$(SHRC); echo_cmd $(CC) $(ALL_CFLAGS) -c -o $@ $${top_srcdir}/$(@F:%.o=%.c) $(ALL_CPPFLAGS)

test-timeout: timeout.o $(top_srcdir)/tests/test-timeout.c
	@$(SHRC); echo_cmd $(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -o $@ timeout.o $(top_srcdir)/tests/test-timeout.c
test-bitops: $(top_srcdir)/tests/test-bitops.c
	@$(SHRC); echo_cmd $(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -o $@ $(top_srcdir)/tests/test-bitops.c

.PHONY: clean clean~

clean:
	$(RM) $(top_builddir)/test-timeout $(top_builddir)/test-bitops $(top_builddir)/*.o
	$(RM) -r $(top_builddir)/*.dSYM

clean~:
	find $(top_builddir) $(top_srcdir) -name "*~" -exec $(RM) -- {} "+"
