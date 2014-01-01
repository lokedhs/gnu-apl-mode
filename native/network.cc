#include "Native_interface.hh"
#include "NetworkConnection.hh"

#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>

static void *listener_loop( void *arg )
{
    NetworkConnection *listener = (NetworkConnection *)arg;
    listener->run();
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

    struct sockaddr_in addr;
    memset( &addr, 0, sizeof( addr ) );
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons( port );
    if( bind( server_socket, (struct sockaddr *)&addr, sizeof( addr ) ) == -1 ) {
        CERR << "Unable to bind to port " << port << ": " << strerror( errno ) << endl;
        DOMAIN_ERROR;
    }

    if( listen( server_socket, 2 ) == -1 ) {
        CERR << "Error calling accept: " << strerror( errno ) << endl;
        DOMAIN_ERROR;
    }

    pthread_create( &thread_id, NULL, listener_loop, new NetworkConnection( server_socket ) );

    return Token(TOK_APL_VALUE1, Value::Str0_P);
}
