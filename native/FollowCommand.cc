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

#include "emacs.hh"
#include "NetworkConnection.hh"
#include "FollowCommand.hh"

#include "TraceData.hh"
#include "../Symbol.hh"
#include "LockWrapper.hh"

#include <sstream>
#include <map>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

typedef map<const Symbol *, TraceData *> SymbolTraceMap;

SymbolTraceMap trace_data; 
pthread_mutex_t trace_data_lock = PTHREAD_MUTEX_INITIALIZER;

void symbol_assignment( const Symbol &symbol, Symbol_Event ev )
{
    LockWrapper lock_wrapper( &trace_data_lock );

    SymbolTraceMap::iterator it = trace_data.find( &symbol );
    if( it != trace_data.end() ) {
        TraceData *data = it->second;
        data->send_update( ev );
    }
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

static TraceData *find_trace_data( Symbol *symbol )
{
    TraceData *data;
    SymbolTraceMap::iterator it = trace_data.find( symbol );
    if( it == trace_data.end() ) {
        data = new TraceData( symbol );
        trace_data[symbol] = data;
    }
    else {
        data = it->second;
    }

    return data;
}

static void enable_trace( NetworkConnection &conn, Symbol *symbol, int cr_level = -1 )
{
    LockWrapper lock_wrapper( &trace_data_lock );

    TraceData *data = find_trace_data( symbol );
    data->add_listener( &conn, cr_level );

    stringstream out;
    out << "enabled" << endl;
    Value_P v = symbol->get_value();
    TraceData::display_value_for_trace( out, v, cr_level );
    conn.send_reply( out.str() );
}

static void disable_trace( NetworkConnection &conn, Symbol *symbol )
{
    LockWrapper lock_wrapper( &trace_data_lock );

    TraceData *data = find_trace_data( symbol );
    data->remove_listener( &conn );

    conn.send_reply( "disabled" );
}

void FollowCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    int num_args = args.size();
    if( num_args < 3 || num_args > 4 ) {
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
        int cr_level = -1;
        if( num_args > 3 ) {
            string cr_arg = args[3];
            if( cr_arg != "off" ) {
                long v = strtol( cr_arg.c_str(), NULL, 10 );
                if( v == LONG_MAX && errno == ERANGE ) {
                    throw ConnectionError( "Illegal CR level argument to follow command" );
                }
                cr_level = v;
            }
        }
        enable_trace( conn, symbol, cr_level );
    }
    else {
        disable_trace( conn, symbol );
    }
}
