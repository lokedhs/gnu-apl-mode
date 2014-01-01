APL_DIST = /home/elias/src/apl
CC = c++
CXXFLAGS = -fPIC -g -I$(APL_DIST)/src
LIBS = -lpthread
LIBNAME = libemacs.so
OBJS = emacs.o network.o NetworkConnection.o

all:		$(LIBNAME)

$(LIBNAME):	$(OBJS)
		$(CC) -shared $(CXXFLAGS) $(OBJS) -o $(LIBNAME) $(LIBS)

clean:
		rm -f $(OBJS) $(LIBNAME)
