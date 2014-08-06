#    This file is part of GNU APL, a free implementation of the
#    ISO/IEC Standard 13751, "Programming Language APL, Extended"
#
#    Copyright (C) 2014  Elias Mårtenson
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

APL_DIST = $(HOME)/src/apl
APL_INSTALL_PREFIX = $(HOME)/src/apl/dist
CXX = c++
CXXFLAGS = -Wall -fPIC -g -I$(APL_DIST) -I$(APL_DIST)/src/native
LIBS = -lpthread
OBJS = emacs.o network.o util.o Listener.o TcpListener.o UnixSocketListener.o NetworkConnection.o \
	SiCommand.o SicCommand.o FnCommand.o DefCommand.o GetVarCommand.o \
	VariablesCommand.o FnTagCommand.o VersionCommand.o FollowCommand.o \
	SystemFnCommand.o SystemVariableCommand.o SendCommand.o LockWrapper.o \
	TraceData.o TempFileWrapper.o

UNAME = $(shell uname)
ifeq ($(UNAME),Darwin)
SHARED_FLAGS = -dynamiclib -Wl,-undefined,dynamic_lookup
LIBRARY_EXT = dylib
else
SHARED_FLAGS = -shared
LIBRARY_EXT = so
endif
LIBNAME = libemacs.$(LIBRARY_EXT)

all:		$(LIBNAME)

install:	$(LIBNAME)
		cp $(LIBNAME) $(APL_INSTALL_PREFIX)/lib/apl/`basename $(LIBNAME)`

$(LIBNAME):	$(OBJS)
		$(CC) $(SHARED_FLAGS) $(CXXFLAGS) $(OBJS) -o $(LIBNAME) $(LIBS)

clean:
		rm -f $(OBJS) $(LIBNAME)
