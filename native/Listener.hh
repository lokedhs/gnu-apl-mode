#ifndef LISTENER_HH
#define LISTENER_HH

#include <string>

#include "network.hh"

typedef void *ThreadFunction( void * );

class Listener {
public:
    Listener() { register_listener( this ); }
    virtual ~Listener() { unregister_listener( this ); }
    virtual std::string start( void ) = 0;
    virtual void wait_for_connection( void ) = 0;
    virtual void close_connection( void ) = 0;
    static Listener *create_listener( int port );
    virtual void set_thread( pthread_t thread_id_in ) { thread_id = thread_id_in; }
    virtual pthread_t get_thread( void ) { return thread_id; }

protected:
    pthread_t thread_id;
};

#endif
