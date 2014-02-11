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
#include "TraceVCommand.hh"
#include "TraceSymbol.hh"
#include "emacs.hh"

void TraceVariableCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    if( args.size() != 2 ) {
        throw ConnectionError( "Wrong number of arguments to tracesymbol" );
    }

    SymbolTable &symbol_table = const_cast<SymbolTable &>( Workspace::get_symbol_table() );
    Symbol *old_symbol = symbol_table.lookup_existing_symbol( ucs_string_from_string( args[1] ) );
    
    TraceSymbol *sym = new TraceSymbol( *old_symbol );
}
