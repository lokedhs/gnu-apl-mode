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

#ifndef LISTENER_HH
#define LISTENER_HH

#include <string>

#include "network.hh"

typedef void *ThreadFunction( void * );

class Listener {
public:
    Listener() { register_listener( this ); }
    virtual ~Listener() { unregister_listener( this ); }
    virtual std::string start( void ) = 0;
    virtual void wait_for_connection( void ) = 0;
    virtual void close_connection( void ) = 0;
    static Listener *create_listener( int port );
    virtual void set_thread( pthread_t thread_id_in ) { thread_id = thread_id_in; }
    virtual pthread_t get_thread( void ) { return thread_id; }

protected:
    pthread_t thread_id;
};

class ListenerWrapper {
public:
    ListenerWrapper( Listener *listener_in ) : listener( listener_in ) { }

    virtual ~ListenerWrapper() {
        listener->close_connection();
    }

private:
    Listener *listener;
};

#endif
