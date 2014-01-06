#ifndef COMMAND_HANDLER_HH
#define COMMAND_HANDLER_HH

#include <vector>

#include "NetworkConnection.hh"

class Command {
public:
    virtual ~Command(std::strig name_in) : name(name_in) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );

private:
    std::string name;

};

#endif
