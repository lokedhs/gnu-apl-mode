#include "TraceSymbol.hh"

TraceSymbol::TraceSymbol( Symbol &old_symbol ) : Symbol( old_symbol.get_name(), old_symbol.id )
{
    next = old_symbol.next;
    symbol = old_symbol.symbol;
    erased = old_symbol.erased;
    value_stack = old_symbol.value_stack;
}

void TraceSymbol::assign( Value_P value, const char *loc )
{
    COUT << "assigning value" << endl;
    Symbol::assign( value, loc );
}
