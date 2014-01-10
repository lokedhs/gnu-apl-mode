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

#include <memory>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <netdb.h>

class AddrWrapper {
public:
    AddrWrapper(struct addrinfo *addr_in) : addr(addr_in) {}
    virtual ~AddrWrapper() { freeaddrinfo( addr ); }

private:
    struct addrinfo *addr;
};

struct ListenerLoopData {
    ListenerLoopData( int server_socket_in ) : server_socket( server_socket_in ) {}
    int server_socket;
};

static void *connection_loop( void *arg )
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
    ListenerLoopData *data = (ListenerLoopData *)arg;
    int server_socket = data->server_socket;
    delete data;

    while( 1 ) {
        struct sockaddr addr;
        socklen_t length;
        int socket = accept( server_socket, &addr, &length );
        if( socket == -1 ) {
            CERR << "Error accepting network connection: " << strerror( errno ) << endl;
            break;
        }
        else {
            NetworkConnection *conn = new NetworkConnection( socket );
            pthread_t thread_id;
            int ret = pthread_create( &thread_id, NULL, connection_loop, conn );
            if( ret != 0 ) {
                CERR << "Error creating thread" << endl;
                delete conn;
            }
        }
    }

    return NULL;
}

Token start_listener( int port )
{
    pthread_t thread_id;
    int server_socket;
    struct addrinfo *addr;
    int ret;

    stringstream serv_name;
    serv_name << port;

    struct addrinfo hints;
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = 0;
    hints.ai_flags = 0;
    hints.ai_addrlen = 0;
    hints.ai_addr = NULL;
    hints.ai_canonname = NULL;
    hints.ai_next = NULL;

    ret = getaddrinfo( "127.0.0.1", serv_name.str().c_str(), &hints, &addr );
    if( ret != 0 ) {
        stringstream errmsg;
        errmsg << "Error looking up listener host: " << gai_strerror( ret );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    AddrWrapper addrWrapper( addr );

    server_socket = socket( AF_INET, SOCK_STREAM, 0 );
    if( server_socket == -1 ) {
        stringstream errmsg;
        errmsg << "Error creating socket: " << strerror( errno );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    int v = 1;
    setsockopt( server_socket, SOL_SOCKET, SO_REUSEADDR, (void *)&v, sizeof( v ) );

    if( bind( server_socket, addr->ai_addr, addr->ai_addrlen ) == -1 ) {
        stringstream errmsg;
        errmsg << "Unable to bind to port " << port << ": " << strerror( errno );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    if( listen( server_socket, 2 ) == -1 ) {
        close( server_socket );
        stringstream errmsg;
        errmsg << "Error calling accept: " << strerror( errno ) << endl;
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    ListenerLoopData *listener_loop_data = new ListenerLoopData( server_socket );
    int res = pthread_create( &thread_id, NULL, listener_loop, listener_loop_data );
    if( res != 0 ) {
        delete listener_loop_data;
        close( server_socket );
        Workspace::more_error() = UCS_string( "Unable to start network connection thread" );
        DOMAIN_ERROR;
    }

    COUT << "Network listener started. Connection information: mode:tcp addr:" << port << endl;

    return Token(TOK_APL_VALUE1, Value::Str0_P);
}
