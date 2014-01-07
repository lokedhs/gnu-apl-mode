APL_DIST = $(HOME)/src/apl
CC = c++
CXXFLAGS = -Wall -fPIC -g -I$(APL_DIST)/src
LIBS = -lpthread
LIBNAME = libemacs.so
OBJS = emacs.o network.o util.o NetworkConnection.o SiCommand.o SicCommand.o \
	FnCommand.o DefCommand.o GetVarCommand.o
UNAME = $(shell uname)
ifeq ($(UNAME),Darwin)
SHARED_FLAGS = -dynamiclib -Wl,-undefined,dynamic_lookup
else
SHARED_FLAGS = -shared
endif

all:		$(LIBNAME)

$(LIBNAME):	$(OBJS)
		$(CC) $(SHARED_FLAGS) $(CXXFLAGS) $(OBJS) -o $(LIBNAME) $(LIBS)

clean:
		rm -f $(OBJS) $(LIBNAME)
