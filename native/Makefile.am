AM_CXXFLAGS = $(CXX_RDYNAMIC)

dist_noinst_DATA =	AUTHORS		\
			LAST_GIT_PULL

pkglib_LTLIBRARIES =	libemacs.la

libemacs_la_SOURCES =						\
		DefCommand.cc		DefCommand.hh		\
		emacs.cc		emacs.hh		\
		FnCommand.cc		FnCommand.hh		\
		FnTagCommand.cc		FnTagCommand.hh		\
		FollowCommand.cc	FollowCommand.hh	\
		GetVarCommand.cc	GetVarCommand.hh	\
		Listener.cc		Listener.hh		\
		LockWrapper.cc		LockWrapper.hh		\
		network.cc		network.hh		\
		NetworkCommand.hh				\
		NetworkConnection.cc	NetworkConnection.hh	\
		SendCommand.cc		SendCommand.hh		\
		SicCommand.cc		SicCommand.hh		\
		SiCommand.cc		SiCommand.hh		\
		SystemFnCommand.cc	SystemFnCommand.hh	\
		SystemVariableCommand.cc SystemVariableCommand.hh \
		TcpListener.cc		TcpListener.hh		\
		TempFileWrapper.cc	TempFileWrapper.hh		\
		TraceData.cc		TraceData.hh		\
		UnixSocketListener.cc	UnixSocketListener.hh	\
		util.cc			util.hh			\
		VariablesCommand.cc	VariablesCommand.hh	\
		VersionCommand.cc	VersionCommand.hh

.PHONY: git-pull
git-pull:
	rm -Rf tmp
	svn export https://github.com/lokedhs/gnu-apl-mode/trunk/native tmp
	mv -f tmp/*cc tmp/*hh .
	rm -Rf tmp
	@echo "Last GIT pull on `date '+%F %R'`" > LAST_GIT_PULL

