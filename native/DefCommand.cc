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
#include "DefCommand.hh"
#include "emacs.hh"

#include "Quad_FX.hh"

void DefCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    vector<string> content = conn.load_block();

    try {
        stringstream out;

        Shape shape( content.size() );
        Value_P function_list_value( new Value( shape, LOC ) );
        for( vector<string>::const_iterator i = content.begin() ; i != content.end() ; i++ ) {
#if 0
            UCS_string s = ucs_string_from_string( *i );
            Shape row_shape( s.size() );
            Value_P row_cell( new Value( row_shape, LOC ) );
            for( int i2 = 0 ; i2 < s.size() ; i2++ ) {
                new (row_cell->next_ravel()) CharCell( s[i2] );
            }
            new (function_list_value->next_ravel()) PointerCell( row_cell );
#else
            new (function_list_value->next_ravel()) PointerCell( make_string_cell( *i, LOC ) );
#endif
        }
        function_list_value->check_value( LOC );

        Quad_FX quad_fx;

        if( args.size() > 1 ) {
            Shape tag_shape( 2 );
            Value_P tag( new Value( tag_shape, LOC ) );
            new (tag->next_ravel()) IntCell( 0 );
            new (tag->next_ravel()) PointerCell( make_string_cell( args[0], LOC ) );
            Token result = quad_fx.eval_AB( tag, function_list_value );
            out << "function defined\n" << result.canonical( PST_CS_NONE ).to_string();
        }
        else {
            Token result = quad_fx.eval_B( function_list_value );
            out << "function defined\n" << result.canonical( PST_CS_NONE ).to_string();
        }
        out << "\n";
        conn.write_string_to_fd( out.str() );
    }
    catch( Error &error ) {
        stringstream out;
        out << "error\n";
        error.print( out );
        out << "\n";
        conn.write_string_to_fd( out.str() );
    }

    conn.write_string_to_fd( END_TAG "\n" );
}
