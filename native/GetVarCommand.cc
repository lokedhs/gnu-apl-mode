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
#include "GetVarCommand.hh"
#include "emacs.hh"

#include <sstream>

class InvalidSymbolContent {
public:
    InvalidSymbolContent( const std::string &message_in ) : message( message_in ) {}
    virtual ~InvalidSymbolContent() {}
    const std::string get_message( void ) { return message; }

private:
    const std::string message;
};

static void send_reply( NetworkConnection &conn, std::string message )
{
    stringstream out;
    out << message << "\n"
        << END_TAG << "\n";
    conn.write_string_to_fd( out.str() );
}

static void escape_char( ostream &out, Unicode unicode )
{
    if( unicode == '\\' ) {
        out << "\\\\";
    }
    else if( unicode == '"' ) {
        out << "\\\"";
    }
    else {
        UCS_string ucs_string( unicode );
        out << to_string(ucs_string);
    }
}

void skalar_value_to_el( ostream &out, Value_P value )
{
    Cell &cell = value->get_ravel( 0 );
    if( cell.is_integer_cell() ) {
        out << cell.get_int_value();
    }
    else if( cell.is_real_cell() ) {
        out << cell.get_real_value();
    }
    else if( cell.is_complex_cell() ) {
        out << "(:complex " << cell.get_real_value() << " " << cell.get_imag_value() << ")";
    }
    else if( cell.is_character_cell() ) {
        out << "(:unicode " << (int)cell.get_char_value() << ")";
    }
    else {
        out << "(:unknown)";
    }
}

static void apl_value_to_el( ostream &out, Value_P value );

static void output_onelevel( ostream &out, Value_P value, int level, int start, int end )
{
    const Shape &shape = value->get_shape();
    int size = shape.get_shape_item( level );
    out << "(";
    if( level < shape.get_rank() - 1 ) {
        int step = (end - start) / size;
        for( int i = start ; i < end ; i += step ) {
            if( i > start ) out << " ";
            output_onelevel( out, value, level + 1, i, i + step );
        }
    }
    else {
        for( int i = start ; i < end ; i++ ) {
            if( i > start ) out << " ";
            apl_value_to_el( out, value->get_ravel( i ).to_value( LOC ) );
        }
    }
    out << ")\n";
}

static void apl_value_to_el( ostream &out, Value_P value )
{
    const Shape &shape = value->get_shape();
    if( value->is_empty() ) {
        out << "(:blank (";
        int rank = shape.get_rank();
        for( int i = 0 ; i < rank ; i++ ) {
            out << " " << shape.get_shape_item( i );
        }
        out << "))";
    }
    else if( value->is_skalar() ) {
        skalar_value_to_el( out, value );
    }
    else if( value->is_char_vector() ) {
        out << "\"";
        int size = shape.get_cols();
        for( int i = 0 ; i < size ; i++ ) {
            escape_char( out, value->get_ravel( i ).get_char_value() );
        }
        out << "\"";
    }
    else if( shape.get_rank() == 1 ) {
        out << "(";
        int size = shape.get_cols();
        for( int i = 0 ; i < size ; i++ ) {
            if( i > 0 ) out << " ";
            apl_value_to_el( out, value->get_ravel( i ).to_value( LOC ) );
        }
        out << ")\n";
    }
    else if( shape.get_rank() > 1 ) {
        out << "(:vector (";
        int rank = shape.get_rank();
        for( int i = 0 ; i < rank ; i++ ) {
            if( i > 0 ) out << " ";
            out << shape.get_shape_item( i );
        }
        out << ")\n";
        output_onelevel( out, value, 0, 0, shape.get_volume() );
        out << ")";
    }
    else {
        throw InvalidSymbolContent( "unknown value" );
    }
}

void GetVarCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    if( args.size() != 2 ) {
        throw ConnectionError( "Wrong number of arguments to getvar" );
    }

    SymbolTable &symbol_table = const_cast<SymbolTable &>( Workspace::get_symbol_table() );
    Symbol *symbol = symbol_table.lookup_existing_symbol( ucs_string_from_string( args[1] ) );
    if( symbol == NULL ) {
        send_reply( conn, "undefined" );
        return;
    }
    if( symbol->get_nc() != NC_VARIABLE ) {
        send_reply( conn, "wrong type" );
        return;
    }

    Value_P value = symbol->get_value();
    try {
        stringstream out;
        out.precision( 20 );
        out << "content\n";
        apl_value_to_el( out, value );
        conn.send_reply( out.str() );
    }
    catch( InvalidSymbolContent &exception ) {
        conn.send_reply( exception.get_message() );
    }
}
