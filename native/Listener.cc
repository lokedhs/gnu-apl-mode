#include "Listener.hh"
#include "TcpListener.hh"
#include "UnixSocketListener.hh"

Listener *Listener::create_listener( int port )
{
    Listener *ret;

    if( port >= 0 ) {
        ret = new TcpListener( port );
    }
    else {
        ret = new UnixSocketListener();
    }

    return ret;
}
