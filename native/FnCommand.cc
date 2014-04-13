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
#include "FnCommand.hh"
#include "emacs.hh"

#include <sstream>

void FnCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    std::string name = args[1];

    std::stringstream out;

    UCS_string ucs_name = ucs_string_from_string( name );
    NamedObject *obj = Workspace::lookup_existing_name( ucs_name );
    if( obj == NULL ) {
        out << "undefined\n";
    }
    else if( !obj->is_user_defined() ) {
        out << "system function\n";
    }
    else {
        const Function *function = obj->get_function();
        if( function == NULL ) {
            out << "symbol is not a function\n";
        }
        else if( function->get_exec_properties()[0] != 0 ) {
            out << "function is not executable\n";
        }
        else {
            out << "function-content\n";
            const UCS_string ucs = function->canonical( false );
            vector<UCS_string> tlines;
            ucs.to_vector( tlines );

            for( vector<UCS_string>::iterator i = tlines.begin() ; i != tlines.end() ; i++ ) {
                out << to_string(*i) << "\n";
            }
        }
    }
    out << END_TAG << "\n";

    conn.write_string_to_fd( out.str() );
}
