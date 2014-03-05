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
#include "network.hh"

#include <pthread.h>

static pthread_mutex_t apl_main_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t apl_main_cond = PTHREAD_COND_INITIALIZER;
static bool apl_active = false;

extern void (*start_input)();
extern void (*end_input)();

extern "C" {
    void *get_function_mux( const char *function_name );
}

void set_active( bool v )
{
    pthread_mutex_lock( &apl_main_lock );
    if( !apl_active && !v ) {
        std::cerr << "Unlocking while the lock is unlocked" << std::endl;
        abort();
    }
    if( v ) {
        while( apl_active ) {
            pthread_cond_wait( &apl_main_cond, &apl_main_lock );
        }
    }
    apl_active = v;
    pthread_cond_broadcast( &apl_main_cond );
    pthread_mutex_unlock( &apl_main_lock );
}

static void active_disable( void )
{
    set_active( false );
}

static void active_enable( void )
{
    set_active( true );
}

Fun_signature get_signature()
{
    set_active( true );
    start_input = active_disable;
    end_input = active_enable;
    return SIG_Z_A_F2_B;
}

static Token list_functions( ostream &out )
{
    out << "Information about the functions" << endl;
    return Token(TOK_APL_VALUE1, Value::Str0_P);
}

Token
eval_B(Value_P B)
{
    return list_functions( CERR );
}

Token eval_AB(Value_P A, Value_P B)
{
    return list_functions( COUT );
}

Token eval_XB(Value_P X, Value_P B)
{
    const APL_Float qct = Workspace::get_CT();
    const int function_number = X->get_ravel(0).get_near_int(qct);

    switch( function_number ) {
    case 0:
        return list_functions( CERR );

    case 1:
    {
        int port;
        if( B->is_empty() ) {
            port = 0;
        }
        else {
            port = B->get_ravel( 0 ).get_near_int( qct );
        }
        return start_listener( port );
    }

    default:
        CERR << "Bad function number: " << function_number << endl;
        DOMAIN_ERROR;
    }

    return Token(TOK_APL_VALUE1, Value::Str0_P);
}

Token eval_AXB(const Value_P A, const Value_P X, const Value_P B)
{
    COUT << "eval_AXB" << endl;
    return Token(TOK_APL_VALUE1, Value::Str0_P);
}

void close_fun( Cause cause )
{
    close_listeners();
}

void *get_function_mux( const char *function_name )
{
    if( strcmp( function_name, "get_signature" ) == 0 ) return (void *)&get_signature;
    if( strcmp( function_name, "eval_B" ) == 0 )        return (void *)&eval_B;
    if( strcmp( function_name, "eval_AB" ) == 0 )       return (void *)&eval_AB;
    if( strcmp( function_name, "eval_XB" ) == 0 )       return (void *)&eval_XB;
    if( strcmp( function_name, "eval_AXB" ) == 0 )      return (void *)&eval_AXB;
    if( strcmp( function_name, "close_fun" ) == 0 )     return (void *)&close_fun;
    return 0;
}

const UCS_string ucs_string_from_string( const std::string &string )
{
    size_t length = string.size();
    const char *buf = string.c_str();
    UTF8_string utf( (const UTF8 *)buf, length );
    return UCS_string( utf );
}

Value_P make_string_cell( const std::string &string, const char *loc )
{
    UCS_string s = ucs_string_from_string( string );
    Shape shape( s.size() );
    Value_P cell( new Value( shape, loc ) );
    for( int i = 0 ; i < s.size() ; i++ ) {
        new (cell->next_ravel()) CharCell( s[i] );
    }
    cell->check_value( loc );
    return cell;
}
