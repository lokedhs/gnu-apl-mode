#include "Symbol.hh"

#ifndef TRACE_SYMBOL_HH
#define TRACE_SYMBOL_HH

class TraceSymbol : public Symbol
{
public:
    TraceSymbol( Symbol &old_symbol );
    virtual void assign(Value_P value, const char * loc);
};

#endif
