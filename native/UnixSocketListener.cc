#include "emacs.hh"
#include "network.hh"
#include "UnixSocketListener.hh"
#include "NetworkConnection.hh"

#include <memory>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <netdb.h>

std::string UnixSocketListener::start( void )
{
    server_socket = socket( AF_UNIX, SOCK_STREAM, 0 );
    if( server_socket == -1 ) {
        stringstream errmsg;
        errmsg << "Error creating unix domain socket: " << strerror( errno ) << endl;
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    stringstream name;
    name << "/tmp/gnu_apl_conn_" << getpid();

    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy( addr.sun_path, name.str().c_str(), sizeof( addr.sun_path ) );
    if( bind( server_socket, (struct sockaddr *)&addr, sizeof( addr ) ) == -1 ) {
        close( server_socket );
        stringstream errmsg;
        errmsg << "Error binding unix domain socket: " << strerror( errno ) << endl;
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    if( listen( server_socket, 2 ) == -1 ) {
        close( server_socket );
        stringstream errmsg;
        errmsg << "Error starting listener on unix domain socket: " << strerror( errno ) << endl;
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    stringstream info_stream;
    info_stream << "mode:unix addr:" << name.str();
    string conninfo = info_stream.str();
    return conninfo;
}

void UnixSocketListener::wait_for_connection( void )
{
    while( true ) {
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
}

void UnixSocketListener::close_connection( void )
{
}
