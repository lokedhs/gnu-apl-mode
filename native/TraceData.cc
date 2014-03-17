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
#include "Workspace.hh"

TraceData::TraceData( Symbol *symbol_in ) : symbol( symbol_in )
{
}

void TraceData::add_listener( NetworkConnection *connection )
{
    Assert( active_listeners.find( connection ) == active_listeners.end() );

    if( active_listeners.empty() ) {
        symbol->set_monitor_callback( symbol_assignment );
    }

    active_listeners.insert( connection );
}

void TraceData::remove_listener( NetworkConnection *connection )
{
    int n = active_listeners.erase( connection );
    Assert( n == 1 );

    if( active_listeners.empty() ) {
        symbol->set_monitor_callback( NULL );
    }
}

void TraceData::send_update( Symbol_Event ev )
{
    Value_P v = symbol->get_value();

    stringstream out;
    if( ev == SEV_ERASED ) {
        out << "sev_erased" << endl
            << symbol->get_name() << endl;
    }
    else {
        out << "symbol_update" << endl
            << symbol->get_name() << endl;
        v->print( out );
    }

    string str = out.str();
    for( set<NetworkConnection *>::iterator it = active_listeners.begin() ; it != active_listeners.end() ; it++ ) {
        NetworkConnection *conn = *it;

        conn->send_notification( str );
    }
}
