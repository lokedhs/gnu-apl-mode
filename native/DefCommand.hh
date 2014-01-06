#ifndef DEF_COMMAND_HH
#define DEF_COMMAND_HH

#include "Command.hh"

class DefCommand : public Command {
public:
    DefCommand( std::string name_in ) : Command( name_in ) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
};

#endif
