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

#include "emacs.hh"
#include "NetworkConnection.hh"
#include "Listener.hh"

#include <memory>
#include <pthread.h>

void *connection_loop( void *arg )
{
    std::auto_ptr<NetworkConnection> connection( (NetworkConnection *)arg );
    try {
        connection->run();
    }
    catch( DisconnectedError &disconnected_error ) {
        // Do nothing here
    }
    catch( ProtocolError &protocol_error ) {
        CERR << "Communication error: " << protocol_error.get_message() << endl;
    }
    catch( ConnectionError &connection_error ) {
        CERR << "Disconnected: " << connection_error.get_message() << endl;
    }
    return NULL;
}

static void *listener_loop( void *arg )
{
    auto_ptr<Listener> listener( (Listener *)arg );

    listener->wait_for_connection();

    return NULL;
}

Token start_listener( int port )
{
    pthread_t thread_id;

    auto_ptr<Listener> listener( Listener::create_listener( port ) );

    string conninfo = listener->start();
    
    int res = pthread_create( &thread_id, NULL, listener_loop, listener.get() );
    if( res != 0 ) {
        Workspace::more_error() = UCS_string( "Unable to start network connection thread" );
        DOMAIN_ERROR;
    }

    listener.release();

    COUT << "Network listener started. Connection information: " << conninfo << endl;

    return Token(TOK_APL_VALUE1, Value::Str0_P);
}
