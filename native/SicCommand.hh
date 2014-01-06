#ifndef SIC_COMMAND_HH
#define SIC_COMMAND_HH

#include "Command.hh"

class SicCommand : public Command {
public:
    SicCommand( std::string name_in ) : Command( name_in ) {};
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
};

#endif
