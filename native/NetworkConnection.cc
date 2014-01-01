#include "NetworkConnection.hh"

#include <iostream>
#include <sstream>
#include <vector>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <unistd.h>

static std::string read_line_from_fd( int fd )
{
    std::stringstream in;
    int end = 0;
    while( !end ) {
        char buf[1024];

        int res = read( fd, (void *)buf, sizeof( buf ) - 1 );
        if( res == -1 ) {
            throw "error";
        }
        if( res == 0 ) {
            throw "disconnected";
        }

        if( buf[res - 1] == '\n' ) {
            buf[res - 1] = 0;
            end = 1;
        }
        else { 
            buf[res] = 0;
        }
        in << buf;
    }
    return in.str();
}

static std::vector<std::string> split(const std::string &s, char delim) {
    std::stringstream ss(s);
    std::string item;
    std::vector<std::string> elems;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

void process_command( const std::string &command )
{
    std::vector<std::string> elements = split( command, ':' );
    std::cout << "size=" << elements.size() << std::endl;
}

void NetworkConnection::run( void )
{
    struct sockaddr addr;
    socklen_t length;
    int socket = accept( server_socket, &addr, &length );
    if( socket == -1 ) {
        abort();
    }

    int end = 0;
    while( !end ) {
        end = process_command( read_line_from_fd( socket ) );
    }
}
