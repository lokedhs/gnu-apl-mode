#include "TraceData.hh"
#include "LockWrapper.hh"
#include "FollowCommand.hh"

TraceData::TraceData( Symbol *symbol_in ) : symbol( symbol_in )
{
}

void TraceData::add_listener( NetworkConnection *connection )
{
    Assert( active_listeners.find( connection ) == active_listeners.end() );

    if( active_listeners.empty() ) {
        symbol->set_monitor_callback( symbol_assignment );
    }

    active_listeners.insert( connection );
}

void TraceData::remove_listener( NetworkConnection *connection )
{
    int n = active_listeners.erase( connection );
    Assert( n == 1 );

    if( active_listeners.empty() ) {
        symbol->set_monitor_callback( NULL );
    }
}

void TraceData::send_update( Symbol_Event ev )
{
    stringstream out;
    out << "symbol_update" << endl
        << symbol->get_name();
    string str = out.str();
    for( set<NetworkConnection *>::iterator it = active_listeners.begin() ; it != active_listeners.end() ; it++ ) {
        NetworkConnection *conn = *it;

        conn->send_notification( str );
    }
}
