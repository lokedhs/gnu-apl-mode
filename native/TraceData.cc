#include "TraceData.hh"
#include "LockWrapper.hh"
#include "FollowCommand.hh"
#include "Workspace.hh"

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

// static Value_P make_value( APL_Integer v )
// {
//     Value_P value( new Value( LOC ) );
//     value->get_ravel( 0 ) = IntCell( v );
//     value->check_value( LOC );
//     return value;
// }

void TraceData::send_update( Symbol_Event ev )
{
    Value_P v = symbol->get_value();

    stringstream out;
    if( ev == SEV_ERASED ) {
        out << "sev_erased" << endl;
    }
    else {
        out << "symbol_update" << endl
            << symbol->get_name() << endl;
        v->print( out );
    }

    string str = out.str();
    for( set<NetworkConnection *>::iterator it = active_listeners.begin() ; it != active_listeners.end() ; it++ ) {
        NetworkConnection *conn = *it;

        conn->send_notification( str );
    }
}
