#ifndef COMMAND_HH
#define COMMAND_HH

#include <string>
#include <vector>

class NetworkConnection;

class Command {
public:
    Command( std::string name_in ) : name( name_in ) {};
    virtual ~Command() {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args ) = 0;
    virtual std::string get_name( void ) { return name; };

private:
    std::string name;
};

#endif
