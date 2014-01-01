#ifndef NETWORK_CONNECTION_HH
#define NETWORK_CONNECTION_HH

#include <string>

class NetworkConnection {
public:
    NetworkConnection( int server_socket_in ) : server_socket(server_socket_in) {};
    void run( void );

private:
    int server_socket;
    int process_command( const std::string &command );
};

#endif
