#ifndef VARIABLES_COMMAND_HH
#define VARIABLES_COMMAND_HH

#include "Command.hh"

class VariablesCommand : public Command {
public:
    VariablesCommand( std::string name_in ) : Command( name_in ) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
};

#endif
