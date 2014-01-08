#include "NetworkConnection.hh"
#include "VariablesCommand.hh"
#include "emacs.hh"

void VariablesCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    SymbolTable symbol_table = Workspace::get_symbol_table();
    stringstream out;
    symbol_table.list_symbols( out, LIST_VARS, UCS_string( "" ) );
    conn.write_string_to_fd( out.str() );
    conn.write_string_to_fd( "\n" END_TAG "\n" );
}
