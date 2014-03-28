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

#ifndef TCP_LISTENER_HH
#define TCP_LISTENER_HH

#include "Listener.hh"

class TcpListener : public Listener {
public:
    TcpListener( int port_in ) : port( port_in ), closing( false ) {};
    virtual ~TcpListener() {};
    virtual std::string start( void );
    virtual void wait_for_connection( void );
    virtual void close_connection( void );

private:
    int port;
    int server_socket;
    bool closing;
};

#endif
