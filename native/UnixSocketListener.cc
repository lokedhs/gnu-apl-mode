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
#include <unistd.h>
#include <sys/stat.h>
#include <poll.h>

UnixSocketListener::~UnixSocketListener()
{
}

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
    filename = name.str();

    unlink( filename.c_str() );

    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy( addr.sun_path, filename.c_str(), sizeof( addr.sun_path ) );
    if( bind( server_socket, (struct sockaddr *)&addr, sizeof( addr ) ) == -1 ) {
        stringstream errmsg;
        errmsg << "Error binding unix domain socket: " << strerror( errno ) << endl;
        close( server_socket );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    initialised = true;

    if( chmod( filename.c_str(), 0600 ) == -1 ) {
        stringstream errmsg;
        errmsg << "Error setting permissions: " << strerror( errno ) << endl;
        close( server_socket );
        Workspace::more_error() = UCS_string( errmsg.str().c_str() );
        DOMAIN_ERROR;
    }

    if( listen( server_socket, 2 ) == -1 ) {
        stringstream errmsg;
        errmsg << "Error starting listener on unix domain socket: " << strerror( errno ) << endl;
        close( server_socket );
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
    int pipe_fd[2];
    if( pipe( pipe_fd ) == -1 ) {
        CERR << "Error creating pipe" << endl;
        return;
    }

    notification_fd = pipe_fd[1];

    while( true ) {
        struct pollfd fds[2];
        fds[0].fd = server_socket;
        fds[0].events = POLLIN | POLLPRI;
        fds[1].fd = pipe_fd[0];
        fds[1].events = POLLIN | POLLPRI;

        int ret = poll( fds, 2, -1 );
        if( ret == -1 ) {
            CERR << "Error while waiting for connection: " << strerror( errno ) << endl;
            break;
        }
        if( ret == 0 ) {
            CERR << "Timed out while waiting for incoming connection" << endl;
            break;
        }

        if( fds[1].revents & (POLLIN | POLLPRI) ) {
            CERR << "Connection interrupted (expected)" << endl;
            break;
        }

        if( fds[0].revents & POLLERR ) {
            CERR << "Error on file handle" << endl;
            break;
        }
        if( fds[0].revents & POLLHUP ) {
            CERR << "Connection was closed" << endl;
            break;
        }

        if( fds[0].revents & (POLLIN | POLLPRI) ) {
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
        else {
            CERR << "Unexpected result from poll on socket" << endl;
        }
    }
}

void UnixSocketListener::close_connection( void )
{
    bool was_closing = closing;
    closing = true;
    if( initialised && !was_closing ) {
        if( server_socket != 0 ) {
            int v = 1;
            if( write( notification_fd, &v, sizeof( v ) ) == -1 ) {
                CERR << "Error writing message to notification file" << endl;
            }
            close( server_socket );
        }

        void *result;
        pthread_join( thread_id, &result );

        if( unlink( filename.c_str() ) == -1 ) {
            CERR << "Error removing socket file name: " << filename << ": " << strerror( errno ) << endl;
        }
    }
}
