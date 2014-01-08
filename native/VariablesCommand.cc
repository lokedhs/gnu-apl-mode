#include "NetworkConnection.hh"
#include "VariablesCommand.hh"
#include "emacs.hh"

void VariablesCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    SymbolTable &symbol_table = const_cast<SymbolTable &>( Workspace::get_symbol_table() );
    stringstream out;

#if 0
    bool all_types;
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
    }
#endif

    int num_symbols = symbol_table.symbols_allocated();
    Symbol *symbols[num_symbols];
    symbol_table.get_all_symbols( symbols, num_symbols );
    for( int i = 0 ; i < num_symbols ; i++ ) {
        Symbol *symbol = symbols[i];
//        if( !symbol->is_erased() && symbol->value_stack.back().name_class == NC_VARIABLE ) {
        if( !symbol->is_erased() ) {
            out << symbol->get_name() << "\n";
        }
    }

    conn.write_string_to_fd( out.str() );
    conn.write_string_to_fd( END_TAG "\n" );
}
