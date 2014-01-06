#include "emacs.hh"
#include "util.hh"
#include "NetworkConnection.hh"
#include "UserFunction.hh"
#include "Quad_FX.hh"
#include "SiCommand.hh"
#include "SicCommand.hh"
#include "FnCommand.hh"
#include "DefCommand.hh"

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


static void add_command( std::map<std::string, Command *> commands, Command *command )
{
    commands.insert( std::pair<std::string, Command *>( command->get_name(), command ) );
}

NetworkConnection::NetworkConnection( int socket_in )
    : socket_fd(socket_in), buffer_pos(0), buffer_length(0)
{
    add_command( commands, new SiCommand( "si" ) );
    add_command( commands, new SicCommand( "sic" ) );
    add_command( commands, new FnCommand( "fn" ) );
    add_command( commands, new DefCommand( "def" ) );
}

NetworkConnection::~NetworkConnection()
{
    for( std::map<std::string, Command *>::iterator i = commands.begin() ; i != commands.end() ; i++ ) {
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
                throw ConnectionError( "disconnected" );
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
/*
        {
            for(int i = 0 ; i < v.size() ; i++) {
                COUT << "  v[" << i << "] = " << (int)(unsigned char)v[i] << endl;
            }
        }
*/
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
            throw ConnectionError( "quit received" );
        }

        std::map<std::string, Command *>::iterator command_iterator = commands.find( operation );
        if( command_iterator != commands.end() ) {
            command_iterator->second->run_command( *this, elements );
        }
        else {
            CERR << "unknown command: '" << operation << "'" << endl;
        }
    }
    else {
        CERR << "empty command" << endl;
    }
    return 0;
}

void NetworkConnection::show_si( void )
{
    std::stringstream out;
    for( const StateIndicator *si = Workspace::SI_top() ; si ; si = si->get_parent() ) {
        out << si->function_name() << "\n";
    }
    out << END_TAG << "\n";

    write_string_to_fd( out.str() );
}

void NetworkConnection::run( void )
{
    int end = 0;
    while( !end ) {
        std::string command = read_line_from_fd();
        end = process_command( command );
    }
}
