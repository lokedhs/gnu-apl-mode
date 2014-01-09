#include "NetworkConnection.hh"
#include "VariablesCommand.hh"
#include "emacs.hh"

void VariablesCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    stringstream out;

    bool all_types = false;
    NameClass cls;
    if( args.size() < 2 ) {
        all_types = true;
    }
    else {
        string typespec = args[1];
        if( typespec == "variable" ) {
            cls = NC_VARIABLE;
        }
        else if( typespec == "function" ) {
            cls = NC_FUNCTION;
        }
        else {
            CERR << "Illegal variable type: " << typespec << endl;
            throw DisconnectedError( "Illegal variable type" );
        }
        all_types = false;
    }

    int num_symbols = Workspace::symbols_allocated();
    Symbol *symbols[num_symbols];
    Workspace::get_all_symbols( symbols, num_symbols );
    for( int i = 0 ; i < num_symbols ; i++ ) {
        Symbol *symbol = symbols[i];
//        if( !symbol->is_erased() && symbol->value_stack.back().name_class == NC_VARIABLE ) {
        if( !symbol->is_erased() && (all_types || symbol->top_of_stack()->name_class == cls) ) {
            out << symbol->get_name() << "\n";
        }
    }

    conn.write_string_to_fd( out.str() );
    conn.write_string_to_fd( END_TAG "\n" );
}
