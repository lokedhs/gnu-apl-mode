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

#include "../Quad_FX.hh"

#include <sstream>

static void log_error( Error &error, ostream &out )
{
    UCS_string l1 = error.get_error_line_1();
    UCS_string l2 = error.get_error_line_2();
    UCS_string l3 = error.get_error_line_3();
    out << l1 << endl << l2 << endl << l3;
}

void DefCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    vector<string> content = conn.load_block();

    try {
        stringstream out;

        Shape shape( content.size() );
        Value_P function_list_value( new Value( shape, LOC ) );
        for( vector<string>::const_iterator i = content.begin() ; i != content.end() ; i++ ) {
            string v;
            if( i->size() == 0 ) {
                v = " ";
            }
            else {
                v = *i;
            }
            new (function_list_value->next_ravel()) PointerCell( make_string_cell( v, LOC ) );
        }
        function_list_value->check_value( LOC );

        Quad_FX quad_fx;

        if( args.size() > 1 ) {
            Shape tag_shape( 2 );
            Value_P tag( new Value( tag_shape, LOC ) );
            new (tag->next_ravel()) IntCell( 0 );
            new (tag->next_ravel()) PointerCell( make_string_cell( args[1], LOC ) );
            function_list_value->check_value( LOC );
            Token result = quad_fx.eval_AB( tag, function_list_value );
            out << "function defined\n" << to_string(result.canonical( PST_CS_NONE));
        }
        else {
            Token result = quad_fx.eval_B( function_list_value );
            if( result.is_apl_val() ) {
                Value_P value = result.get_apl_val();
                if( value->is_int_scalar( 0 ) ) {
                    out << "error\n"
                        << "parse error\n"
                        << "Error parsing expression\n"
                        << value->get_ravel( 0 ).get_int_value();
                }
                else if( value->is_char_string() ) {
                    out << "function defined\n"
                        << value->get_UCS_ravel();
                }
                else {
                    out << "error\n"
                        << "illegal result type";
                }
            }
            else {
                out << "error\n"
                    << "unknown error";
            }
        }
        out << "\n"
            << END_TAG << "\n";
        conn.write_string_to_fd( out.str() );
    }
    catch( Error &error ) {
        stringstream out;
        out << "error\n";

        log_error( error, out );

        out << "\n"
            << END_TAG << "\n";
        conn.write_string_to_fd( out.str() );
    }
}
