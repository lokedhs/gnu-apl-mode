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
#include "FollowCommand.hh"
#include "emacs.hh"

#include "Symbol.hh"

static void symbol_assignment( const Symbol &symbol, Symbol_Event ev )
{
    CERR << "event on " << symbol.get_name() << ": " << ev << endl;
}

static bool parse_boolean( string arg )
{
    if( arg == "on" ) {
        return true;
    }
    else if( arg == "off" ) {
        return false;
    }
    else {
        throw ConnectionError( "unexpected argument to trace" );
    }
}

static void enable_trace( const Symbol *symbol )
{
}

static void disable_trace( const Symbol *symbol )
{
}

void FollowCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    if( args.size() != 3 ) {
        throw ConnectionError( "Wrong number of arguments to trace" );
    }

    SymbolTable &symbol_table = const_cast<SymbolTable &>( Workspace::get_symbol_table() );
    Symbol *symbol = symbol_table.lookup_existing_symbol( ucs_string_from_string( args[1] ) );
    if( symbol == NULL ) {
        conn.send_reply( "undefined" );
        return;
    }
    if( symbol->get_nc() != NC_VARIABLE ) {
        conn.send_reply( "wrong type" );
        return;
    }

    bool enable = parse_boolean( args[2] );
    if( enable ) {
        enable_trace( symbol );
    }
    else {
        disable_trace( symbol );
    }
}
