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

#include "TraceData.hh"
#include "LockWrapper.hh"
#include "FollowCommand.hh"
#include "../Workspace.hh"

#include <sstream>

TraceData::TraceData( Symbol *symbol_in ) : symbol( symbol_in )
{
}

void TraceData::add_listener( NetworkConnection *connection, int cr_level )
{
    Assert( active_listeners.find( connection ) == active_listeners.end() );

    if( active_listeners.empty() ) {
        symbol->set_monitor_callback( symbol_assignment );
    }

    active_listeners.insert( pair<NetworkConnection *, int>( connection, cr_level ) );
}

void TraceData::remove_listener( NetworkConnection *connection )
{
    int n = active_listeners.erase( connection );
    Assert( n == 1 );

    if( active_listeners.empty() ) {
        symbol->set_monitor_callback( NULL );
    }
}

void TraceData::display_value_for_trace( ostream &out, const Value_P &value, int cr_level )
{
    if( cr_level < 0 ) {
        PrintContext context( PST_NONE, Workspace::get_PrintContext().get_PP(), 100000 );
        value->print1( out, context );
    }
    else {
        if( cr_level < 1 || cr_level > 9 ) {
            throw ConnectionError( "Illegal CR level" );
        }
        PrintContext context( PST_NONE, Workspace::get_PrintContext().get_PP(), 100000 );
        Value_P cr_formatted = Quad_CR::do_CR( cr_level, *value, context );

        PrintContext context2( PST_NONE, Workspace::get_PrintContext().get_PP(), 100000 );
        cr_formatted->print1( out, context2 );
    }
}

void TraceData::send_update( Symbol_Event ev )
{
    const Value_P v = symbol->get_value();

    for( map<NetworkConnection *, TraceDataEntry>::iterator it = active_listeners.begin()
             ; it != active_listeners.end()
             ; it++ ) {
        NetworkConnection *conn = it->first;

        stringstream out;
        if( ev == SEV_ERASED ) {
            out << "sev_erased" << endl << symbol->get_name() << endl;
        }
        else {
            out << "symbol_update" << endl << symbol->get_name() << endl;
            int cr_level = it->second.get_cr_level();
            display_value_for_trace( out, v, cr_level );
        }

        string str = out.str();
        conn->send_notification( str );
    }
}
