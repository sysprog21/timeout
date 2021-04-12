ifeq ($(UNAME_S),Darwin)
LUA = /usr/local/opt/lua@5.3/bin/lua
else
LUA = lua5.3
endif
LUA := $(shell which $(LUA))
ifndef LUA
  LUA = lua
  LUA := $(shell which $(LUA))
  ifndef LUA
    $(warning "no lua-5.3 found. Please check package installation")
  endif
endif

ifeq ($(UNAME_S),Darwin)
LUA_INC_DIR = /usr/local/opt/lua@5.3/include
else
LUA_INC_DIR = /usr/include/lua5.3
endif
LUA_INC_DIR := $(shell dirname -z $(shell find $(LUA_INC_DIR) -name lualib.h 2>/dev/null))
ifndef LUA_INC_DIR
$(warning "no lua-5.3 headers found. Please check package installation")
endif

LUA_CFLAGS = -I"$(LUA_INC_DIR)"

ifeq ($(UNAME_S),Darwin)
LUA_LDFLAGS = -L/usr/local/opt/lua@5.3/lib -llua
endif
