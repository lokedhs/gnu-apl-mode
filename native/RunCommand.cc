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

#include "RunCommand.hh"
#include "emacs.hh"
#include "NetworkConnection.hh"

void RunCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    stringstream out;
    while( 1 ) {
        std::string line = conn.read_line_from_fd();
        if( line == END_TAG ) {
            break;
        }
        out << line << "\n";
    }

    Token result = Bif_F1_EXECUTE::execute_statement( ucs_string_from_string( out.str() ) );
    TokenTag tag = result.get_tag();

    stringstream result_stream;
    if( tag == TOK_ERROR ) {
        result_stream << "error:" << result.get_int_val();
    }
    else {
        result_stream << "result:NOT-IMPL";
    }

    result_stream << "\n" << END_TAG << "\n";

    conn.write_string_to_fd( result_stream.str() );
}
