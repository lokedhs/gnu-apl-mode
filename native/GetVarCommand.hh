#ifndef GET_VAR_COMMAND_HH
#define GET_VAR_COMMAND_HH

#include "Command.hh"

class GetVarCommand : public Command {
public:
    GetVarCommand( std::string name_in ) : Command( name_in ) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
};

#endif
