#ifndef FN_COMMAND_HH
#define FN_COMMAND_HH

#include "Command.hh"

class FnCommand : public Command {
public:
    FnCommand( std::string name_in ) : Command( name_in ) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
};

#endif
