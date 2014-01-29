#ifndef UNIX_SOCKET_LISTENER_HH
#define UNIX_SOCKET_LISTENER_HH

#include "Listener.hh"

class UnixSocketListener : public Listener {
public:
    UnixSocketListener() : server_socket( 0 ), initialised( false ), closing( false ) {};
    virtual ~UnixSocketListener();
    virtual std::string start( void );
    virtual void wait_for_connection( void );
    virtual void close_connection( void );

private:
    int server_socket;
    std::string filename;
    bool initialised;
    bool closing;
};

#endif
