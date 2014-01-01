#include "NetworkConnection.hh"
#include "Native_interface.hh"

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

std::string NetworkConnection::read_line_from_fd()
{
    std::stringstream in;
    int end = 0;
    while( !end ) {
        char buf[1024];

        int res = read( socket_fd, (void *)buf, sizeof( buf ) - 1 );
        if( res == -1 ) {
            throw "error";
        }
        if( res == 0 ) {
            throw "disconnected";
        }

        if( buf[res - 1] == '\n' ) {
            if( res >= 2 && buf[res - 2] == '\r' ) {
                buf[res - 2] = 0;
            }
            else {
                buf[res - 1] = 0;
            }
            end = 1;
        }
        else {
            buf[res] = 0;
        }
        in << buf;
    }
    return in.str();
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

static std::vector<std::string> split(const std::string &s, char delim)
{
    std::stringstream ss(s);
    std::string item;
    std::vector<std::string> elems;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

int NetworkConnection::process_command( const std::string &command )
{
    std::vector<std::string> elements = split( command, ':' );
    if( elements.size() > 0 ) {
        std::string operation = elements[0];
        if( operation == "si" ) {
            show_si();
        }
        else {
            CERR << "unknown command: >" << operation << "<" << endl;
        }
    }
    else {
        CERR << "empty command" << endl;
    }
    return 0;
}

void NetworkConnection::show_si( void )
{
    std::cout << "showing si" << std::endl;
    std::stringstream out;
    for( const StateIndicator *si = Workspace::SI_top() ; si ; si = si->get_parent() ) {
        si->print( out );
        out << '\n';
    }

    out << "END\n";

    std::cout << "si:" << out.str() << "end of list" << std::endl;
    write_string_to_fd( out.str() );
}

void NetworkConnection::run( void )
{
    struct sockaddr addr;
    socklen_t length;
    int socket = accept( server_socket, &addr, &length );
    if( socket == -1 ) {
        abort();
    }

    socket_fd = socket;

    int end = 0;
    while( !end ) {
        std::string command = read_line_from_fd();
        end = process_command( command );
    }
}
