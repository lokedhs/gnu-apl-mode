#ifndef NETWORK_CONNECTION_HH
#define NETWORK_CONNECTION_HH

#include <string>

class NetworkConnection {
public:
    NetworkConnection( int server_socket_in ) : server_socket(server_socket_in) {};
    void run( void );

private:
    int server_socket;
    int socket_fd;

    std::string read_line_from_fd( void );
    void write_string_to_fd( const std::string &s );
    int process_command( const std::string &command );
    void show_si( void );
};

#endif
