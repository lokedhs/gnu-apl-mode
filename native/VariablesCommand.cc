/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright (C) 2014  Elias Mårtenson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "NetworkConnection.hh"
#include "VariablesCommand.hh"
#include "emacs.hh"

#include <sstream>

enum TypeSpec {
    ALL,
    VARIABLE,
    FUNCTION
};

void VariablesCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    stringstream out;

    TypeSpec cls = ALL;
    if( args.size() >= 2 ) {
        string typespec = args[1];
        if( typespec == "variable" ) {
            cls = VARIABLE;
        }
        else if( typespec == "function" ) {
            cls = FUNCTION;
        }
        else {
            CERR << "Illegal variable type: " << typespec << endl;
            throw DisconnectedError( "Illegal variable type" );
        }
    }

    int num_symbols = Workspace::symbols_allocated();
    Symbol **symbols = new Symbol *[num_symbols];
    Workspace::get_all_symbols( symbols, num_symbols );
    for( int i = 0 ; i < num_symbols ; i++ ) {
        Symbol *symbol = symbols[i];
        if( !symbol->is_erased() ) {
            NameClass symbol_nc = symbol->top_of_stack()->name_class;
            if( (cls == ALL && (symbol_nc == NC_VARIABLE || symbol_nc == NC_FUNCTION || symbol_nc == NC_OPERATOR))
                || (cls == VARIABLE && symbol_nc == NC_VARIABLE)
                || (cls == FUNCTION && (symbol_nc == NC_FUNCTION || symbol_nc == NC_OPERATOR)) ) {
                out << symbol->get_name() << "\n";
            }
        }
    }

    conn.send_reply( out.str() );

    delete[] symbols;
}
