#include "NetworkConnection.hh"
#include "GetVarCommand.hh"
#include "emacs.hh"

#include <sstream>

static void send_reply( NetworkConnection &conn, std::string message )
{
    conn.write_string_to_fd( message );
    conn.write_string_to_fd( END_TAG );
}

static void escape_char( stringstream &out, Unicode unicode )
{
    if( unicode == '\\' ) {
        out << "\\\\";
    }
    else if( unicode == '"' ) {
        out << "\\\"";
    }
    else {
        UCS_string ucs_string( unicode );
        out << ucs_string.to_string();
    }
}

static void get_printable_value_scalar( stringstream &out, const Cell &cell )
{
    if( cell.is_integer_cell() ) {
        out << cell.get_int_value();
    }
    else if( cell.is_real_cell() ) {
        out << cell.get_real_value();
    }
    else if( cell.is_character_cell() ) {
        out << "\"";
        escape_char( out, cell.get_char_value() );
        out << "\"";
    }
    else {
        CERR << "Illegal cell type" << endl;
        throw "Foo";
    }
}

static const string make_printable_value( Value_P value )
{
    stringstream out;

    Shape shape = value->get_shape();
    int rank = shape.get_rank();
    if( rank == 0 ) {
        get_printable_value_scalar( out, value->get_ravel( 0 ) );
    }
    else if( rank == 1 ) {
        // This can be either a string or a one-dimensional array.
        // They are differentiated by the type of the first element.
        int cols = shape.get_cols();
        if( cols > 0 && value->get_ravel( 0 ).is_character_cell() ) {
            // This is a string
            out << "\"";
            for( int i = 0 ; i < cols ; i++ ) {
                Cell cell = value->get_ravel( i );
                if( !cell.is_character_cell() ) {
                    throw "Unsupported array form";
                }
                escape_char( out, cell.get_char_value() );
            }
            out << "\"";
        }
    }
    else {
        throw "TODO";
    }

    return out.str();
}

void GetVarCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    if( args.size() != 2 ) {
        throw ConnectionError( "Wrong number of arguments to getvar" );
    }

    SymbolTable symbol_table = Workspace::get_symbol_table();
    Symbol *symbol = symbol_table.lookup_existing_symbol( ucs_string_from_string( args[1] ) );
    if( symbol->get_nc() != NC_VARIABLE ) {
        send_reply( conn, "undefined" );
        return;
    }

    Value_P value = symbol->get_value();
    conn.write_string_to_fd( make_printable_value( value ) );
    conn.write_string_to_fd( "\n" END_TAG "\n" );
}
