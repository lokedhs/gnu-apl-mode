/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright (C) 2014  Elias Mårtenson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef UNIX_SOCKET_LISTENER_HH
#define UNIX_SOCKET_LISTENER_HH

#include "Listener.hh"

class UnixSocketListener : public Listener {
public:
    UnixSocketListener() : server_socket( 0 ), initialised( false ), closing( false ) {};
    virtual ~UnixSocketListener();
    virtual std::string start( void );
    virtual void wait_for_connection( void );
    virtual void close_connection( void );

private:
    int server_socket;
    std::string filename;
    bool initialised;
    bool closing;
    int notification_fd;
};

#endif
