#include "NetworkConnection.hh"
#include "FnCommand.hh"
#include "emacs.hh"

void FnCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    std::string name = args[1];

    std::stringstream out;

    UCS_string ucs_name = ucs_string_from_string( name );
    NamedObject *obj = Workspace::lookup_existing_name( ucs_name );
    if( obj == NULL ) {
        out << "undefined\n";
    }
    else if( !obj->is_user_defined() ) {
        out << "system function\n";
    }
    else {
        const Function *function = obj->get_function();
        if( function == NULL ) {
            out << "symbol is not a function";
        }
        else if( function->get_exec_properties()[0] != 0 ) {
            out << "function is not executable\n";
        }
        else {
            const UCS_string ucs = function->canonical( false );
            vector<UCS_string> tlines;
            ucs.to_vector( tlines );

            for( vector<UCS_string>::iterator i = tlines.begin() ; i != tlines.end() ; i++ ) {
                out << i->to_string() << "\n";
            }
        }
    }
    out << END_TAG << "\n";

    conn.write_string_to_fd( out.str() );
}
