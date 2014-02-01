#ifndef TCP_LISTENER_HH
#define TCP_LISTENER_HH

#include "Listener.hh"

class TcpListener : public Listener {
public:
    TcpListener( int port_in ) : port( port_in ), closing( false ) {};
    virtual ~TcpListener() {};
    virtual std::string start( void );
    virtual void wait_for_connection( void );
    virtual void close_connection( void );

private:
    int port;
    int server_socket;
    bool closing;
};

#endif
