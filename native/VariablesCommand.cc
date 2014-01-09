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
        if( !symbol->is_erased() && (all_types || symbol->top_of_stack()->name_class == cls) ) {
            out << symbol->get_name() << "\n";
        }
    }

    conn.write_string_to_fd( out.str() );
    conn.write_string_to_fd( END_TAG "\n" );
}
