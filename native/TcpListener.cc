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
#include "network.hh"
#include "NetworkConnection.hh"
#include "TcpListener.hh"

#include <memory>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <netdb.h>

std::string TcpListener::start( void )
{
    struct addrinfo *addr;
    int ret;

    stringstream serv_name;
    serv_name << port;

    struct addrinfo hints = { 0 };
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
    if( setsockopt( server_socket, SOL_SOCKET, SO_REUSEADDR, (void *)&v, sizeof( v ) ) == -1 ) {
        stringstream errmsg;
        errmsg << "Error setting SO_REUSEADDR parameter: " << strerror( errno );
        close( server_socket );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;        
    }

    if( bind( server_socket, addr->ai_addr, addr->ai_addrlen ) == -1 ) {
        stringstream errmsg;
        errmsg << "Unable to bind to port " << port << ": " << strerror( errno );
        close( server_socket );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    if( listen( server_socket, 2 ) == -1 ) {
        stringstream errmsg;
        errmsg << "Error calling accept: " << strerror( errno ) << endl;
        close( server_socket );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    struct sockaddr_in listen_address;
    socklen_t listen_address_len = sizeof( listen_address );
    if( getsockname( server_socket, (struct sockaddr *)&listen_address, &listen_address_len ) == -1 ) {
        stringstream errmsg;
        errmsg << "Error getting port number of socket: " << strerror( errno ) << endl;
        close( server_socket );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    stringstream info_stream;
    info_stream << "mode:tcp addr:" << ntohs( listen_address.sin_port );
    string conninfo = info_stream.str();
    return conninfo;
}

void TcpListener::wait_for_connection( void )
{
    while( true ) {
        struct sockaddr addr;
        socklen_t length;
        int socket = accept( server_socket, &addr, &length );
        if( socket == -1 ) {
            if( !closing ) {
                CERR << "Error accepting network connection: " << strerror( errno ) << endl;
            }
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
}

void TcpListener::close_connection( void )
{
    closing = true;

    if( server_socket != 0 ) {
        close( server_socket );
    }
}
