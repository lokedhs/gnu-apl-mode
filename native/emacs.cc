#include "emacs.hh"
#include "network.hh"

extern "C" {
    void *get_function_mux( const char *function_name );
}

Fun_signature get_signature()
{
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
            port = 7293;
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

void *get_function_mux( const char *function_name )
{
    if (!strcmp(function_name, "get_signature"))   return (void *)&get_signature;
    if (!strcmp(function_name, "eval_B"))          return (void *)&eval_B;
    if (!strcmp(function_name, "eval_AB"))         return (void *)&eval_AB;
    if (!strcmp(function_name, "eval_XB"))         return (void *)&eval_XB;
    if (!strcmp(function_name, "eval_AXB"))        return (void *)&eval_AXB;
    return 0;
}
