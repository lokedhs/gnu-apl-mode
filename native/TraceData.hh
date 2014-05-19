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

#ifndef TRACE_DATA_HH
#define TRACE_DATA_HH

#include "emacs.hh"
#include "NetworkConnection.hh"
#include "pthread.h"
#include "../Symbol.hh"

#include <set>
#include <map>

class TraceDataEntry {
public:
    TraceDataEntry( int cr_level_in ) : cr_level( cr_level_in ) {}
    int get_cr_level( void ) { return cr_level; }

private:
    int cr_level;
};

class TraceData {
public:
    TraceData( Symbol *symbol_in );
    virtual ~TraceData() {};
    void add_listener( NetworkConnection *connection, int cr_level = -1 );
    void remove_listener( NetworkConnection *connection );
    void send_update( Symbol_Event ev );
    static void display_value_for_trace( ostream &out, const Value_P &value, int cr_level );

private:
    Symbol *symbol;
    map<NetworkConnection *, TraceDataEntry> active_listeners;
};

#endif
