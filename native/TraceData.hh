#ifndef TRACE_DATA_HH
#define TRACE_DATA_HH

#include "emacs.hh"
#include "NetworkConnection.hh"
#include "pthread.h"
#include "Symbol.hh"

#include <set>

class TraceData {
public:
    TraceData( Symbol *symbol_in );
    virtual ~TraceData() {};
    void add_listener( NetworkConnection *connection );
    void remove_listener( NetworkConnection *connection );
    void send_update( Symbol_Event ev );

private:
    Symbol *symbol;
    set<NetworkConnection *> active_listeners;
};

#endif
