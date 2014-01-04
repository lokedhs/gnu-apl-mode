#ifndef NETWORK_CONNECTION_HH
#define NETWORK_CONNECTION_HH

#include <string>
#include <vector>

#define END_TAG "APL_NATIVE_END_TAG"

class NetworkConnection {
public:
    NetworkConnection( int socket_in ) : socket_fd(socket_in) {};
    void run( void );

private:
    int socket_fd;

    std::string read_line_from_fd( void );
    void write_string_to_fd( const std::string &s );
    std::vector<std::string> load_block( void );
    int process_command( const std::string &command );
    void show_si( void );
    void clear_si_stack( void );
    void send_function( const std::vector<std::string> &content );
    void show_function( const std::string &name );
};

class ConnectionError {
public:
    ConnectionError( const std::string &message_in ) : message( message_in ) {};
    virtual ~ConnectionError() {};
    std::string get_message( void ) { return message; };

private:
    const std::string message;
};

#endif
