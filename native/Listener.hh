#ifndef LISTENER_HH
#define LISTENER_HH

#include <string>

typedef void *ThreadFunction( void * );

class Listener {
public:
    virtual ~Listener() {}
    virtual std::string start( void ) = 0;
    virtual void wait_for_connection( void ) = 0;
    virtual void close_connection( void ) = 0;
    static Listener *create_listener( int port );
};

#endif
