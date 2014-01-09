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
#include "util.hh"
#include "NetworkConnection.hh"
#include "UserFunction.hh"
#include "Quad_FX.hh"
#include "SiCommand.hh"
#include "SicCommand.hh"
#include "FnCommand.hh"
#include "DefCommand.hh"
#include "GetVarCommand.hh"
#include "VariablesCommand.hh"
#include "RunCommand.hh"

#include <iostream>
#include <sstream>
#include <vector>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>


static void add_command( std::map<std::string, NetworkCommand *> &commands, NetworkCommand *command )
{
    commands.insert( std::pair<std::string, NetworkCommand *>( command->get_name(), command ) );
}

NetworkConnection::NetworkConnection( int socket_in )
    : socket_fd(socket_in), buffer_pos(0), buffer_length(0)
{
    add_command( commands, new SiCommand( "si" ) );
    add_command( commands, new SicCommand( "sic" ) );
    add_command( commands, new FnCommand( "fn" ) );
    add_command( commands, new DefCommand( "def" ) );
    add_command( commands, new GetVarCommand( "getvar" ) );
    add_command( commands, new VariablesCommand( "variables" ) );
}

NetworkConnection::~NetworkConnection()
{
    close( socket_fd );

    for( std::map<std::string, NetworkCommand *>::iterator i = commands.begin() ; i != commands.end() ; i++ ) {
        delete i->second;
    }
}

std::string NetworkConnection::read_line_from_fd()
{
    std::stringstream in;

    bool end = false;
    while( !end ) {
        while( buffer_pos < buffer_length ) {
            char ch = buffer[buffer_pos++];
            if( ch == '\n' ) {
                end = true;
                break;
            }
            in << ch;
        }

        if( !end ) {
            int res = read( socket_fd, (void *)buffer, sizeof( buffer ) );
            if( res == -1 ) {
                throw ConnectionError( "network error" );
            }
            if( res == 0 ) {
                throw DisconnectedError( "Remote disconnected" );
            }
            buffer_pos = 0;
            buffer_length = res;
        }
    }

    std::string result = in.str();
    if( result[result.size() - 1] == '\r' ) {
        return result.substr( 0, result.size() -1 );
    }
    else {
        return result;
    }
}

void NetworkConnection::write_string_to_fd( const std::string &s )
{
    const char *buf = s.c_str();
    int n = strlen( buf );
    int pos = 0;
    while( pos < n ) {
        int res = write( socket_fd, buf + pos, n - pos );
        if( res == -1 ) {
            abort();
        }
        pos += res;
    }
}

std::vector<std::string> NetworkConnection::load_block( void )
{
    std::vector<std::string> result;
    while( 1 ) {
        std::string v = read_line_from_fd();
        if( v == END_TAG ) {
            break;
        }
        result.push_back( v );
    }
    return result;
}

int NetworkConnection::process_command( const std::string &command )
{
    LockWrapper lock;
    std::vector<std::string> elements = split( command, ':' );
    if( elements.size() > 0 ) {
        std::string operation = elements[0];

        if( operation == "quit" ) {
            close( socket_fd );
            throw DisconnectedError( "quit received" );
        }

        std::map<std::string, NetworkCommand *>::iterator command_iterator = commands.find( operation );
        if( command_iterator == commands.end() ) {
            stringstream out;
            out << "unknown command: '" << operation << "'";
            throw ProtocolError( out.str() );
        }
        else {
            command_iterator->second->run_command( *this, elements );
        }
    }
    return 0;
}

void NetworkConnection::run( void )
{
    int end = 0;
    while( !end ) {
        std::string command = read_line_from_fd();
        end = process_command( command );
    }
}
