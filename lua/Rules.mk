$(LUA_APIS:%=$(top_builddir)/lua/%/timeout.so): $(top_srcdir)/lua/timeout-lua.c $(top_srcdir)/timeout.h $(top_srcdir)/timeout.c
	mkdir -p $(@D)
	@$(SHRC); echo_cmd $(CC) -o $@ $(top_srcdir)/lua/timeout-lua.c -I$(top_srcdir) -DWHEEL_BIT=$(WHEEL_BIT) -DWHEEL_NUM=$(WHEEL_NUM) $(LUA_CPPFLAGS) $(ALL_CPPFLAGS) $(ALL_CFLAGS) $(ALL_SOFLAGS) $(ALL_LDFLAGS) $(ALL_LIBS)

$(LUA_APIS:%=$(top_builddir)/lua/%/timeouts.so):
	cd $(@D) && ln -fs timeout.so timeouts.so

lua-clean:
	$(RM) -r $(top_builddir)/lua/5.?

clean: lua-clean

