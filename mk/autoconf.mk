ifeq ($(UNAME_S),Darwin)	
    SO_FLAGS := -bundle -undefined dynamic_lookup
endif
ifeq ($(UNAME_S),Linux)
    SO_FLAGS := -fPIC -shared	
    LDFLAGS += -lrt
endif
