#ifndef SI_COMMAND_HH
#define SI_COMMAND_HH

#include "Command.hh"

class SiCommand : public Command {
public:
    SiCommand( std::string name_in ) : Command( name_in ) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
};

#endif
