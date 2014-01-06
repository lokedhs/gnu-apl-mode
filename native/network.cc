#include "emacs.hh"
#include "NetworkConnection.hh"

#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>

struct ListenerLoopData {
    int server_socket;
};

static void *connection_loop( void *arg )
{
    NetworkConnection *connection = (NetworkConnection *)arg;
    try {
        connection->run();
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

    server_socket = socket( AF_INET, SOCK_STREAM, 0 );
    if( server_socket == -1 ) {
        CERR << "Error creating socket: " << strerror( errno ) << endl;
        DOMAIN_ERROR;
    }

    int v = 1;
    setsockopt( server_socket, SOL_SOCKET, SO_REUSEADDR, (void *)&v, sizeof( v ) );

    struct sockaddr_in addr;
    memset( &addr, 0, sizeof( addr ) );
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons( port );
    if( bind( server_socket, (struct sockaddr *)&addr, sizeof( addr ) ) == -1 ) {
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

    int res = pthread_create( &thread_id, NULL, listener_loop, new NetworkConnection( server_socket ) );
    if( res != 0 ) {
        close( server_socket );
        Workspace::more_error() = UCS_string( "Unable to start network connection thread" );
        DOMAIN_ERROR;
    }

    COUT << "Network listener started. Connection information: mode:tcp addr:" << port << endl;

    return Token(TOK_APL_VALUE1, Value::Str0_P);
}
