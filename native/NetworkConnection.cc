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
#include "LockWrapper.hh"
#include "../UserFunction.hh"
#include "../Quad_FX.hh"
#include "SiCommand.hh"
#include "SicCommand.hh"
#include "FnCommand.hh"
#include "DefCommand.hh"
#include "GetVarCommand.hh"
#include "VariablesCommand.hh"
#include "FnTagCommand.hh"
#include "VersionCommand.hh"
#include "FollowCommand.hh"
#include "SystemFnCommand.hh"
#include "SystemVariableCommand.hh"

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
    pthread_mutex_init( &connection_lock, NULL );

    add_command( commands, new SiCommand( "si" ) );
    add_command( commands, new SicCommand( "sic" ) );
    add_command( commands, new FnCommand( "fn" ) );
    add_command( commands, new DefCommand( "def" ) );
    add_command( commands, new GetVarCommand( "getvar" ) );
    add_command( commands, new VariablesCommand( "variables" ) );
    add_command( commands, new FnTagCommand( "functiontag" ) );
    add_command( commands, new VersionCommand( "proto" ) );
    add_command( commands, new FollowCommand( "trace" ) );
    add_command( commands, new SystemFnCommand( "systemcommands" ) );
    add_command( commands, new SystemVariableCommand( "systemvariables" ) );
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
    LockWrapper lock_wrapper( &connection_lock );

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

static char get_char_and_check( const std::string &command, std::string::const_iterator &i )
{
    if( i == command.end() ) {
        throw ConnectionError( "Truncated escaped string" );
    }
    return *i;
}

static std::string unescape( const std::string &command )
{
    stringstream out;
    for( std::string::const_iterator i = command.begin() ; i != command.end() ; i++ ) {
        if( *i == '&' ) {
            char buf[3];
            buf[0] = get_char_and_check( command, ++i );
            buf[1] = get_char_and_check( command, ++i );
            buf[2] = 0;
            if( get_char_and_check( command, ++i ) != ';' ) {
                throw ConnectionError( "Illegal escape sequence" );
            }

            char *endptr;
            long v = strtol( buf, &endptr, 16 );
            if( *endptr != 0 ) {
                throw ConnectionError( "Illegal character code" );
            }

            out << static_cast<unsigned char>( v );
        }
        else {
            out << *i;
        }
    }

    return out.str();
}

int NetworkConnection::process_command( const std::string &command )
{
    ActiveWrapper lock;
    std::vector<std::string> parts = split( command, ':' );
    std::vector<std::string> elements;
    for( std::vector<std::string>::iterator i = parts.begin() ; i != parts.end() ; i++ ) {
        elements.push_back( unescape( *i ) );
    }

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

void NetworkConnection::send_reply( const std::string &str )
{
    std::stringstream out;
    out << str << "\n"
        << END_TAG << "\n";
    write_string_to_fd( out.str() );
}

void NetworkConnection::send_notification( const std::string &str )
{
    std::stringstream out;
    out << NOTIFICATION_START_TAG << "\n"
        << str << "\n"
        << NOTIFICATION_END_TAG << "\n";
    write_string_to_fd( out.str() );
}
