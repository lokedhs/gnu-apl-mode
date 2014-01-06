#include "NetworkConnection.hh"
#include "SiCommand.hh"
#include "emacs.hh"

void SiCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    std::stringstream out;
    for( const StateIndicator *si = Workspace::SI_top() ; si ; si = si->get_parent() ) {
        out << si->function_name() << "\n";
    }
    out << END_TAG << "\n";

    conn.write_string_to_fd( out.str() );
}
