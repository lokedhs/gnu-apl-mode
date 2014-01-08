#ifndef EMACS_HH
#define EMACS_HH

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunknown-pragmas"
#pragma GCC diagnostic ignored "-Wpragmas"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wreturn-type"
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wreorder"
#pragma GCC diagnostic ignored "-Wmismatched-tags"
#pragma GCC diagnostic ignored "-Woverloaded-virtual"
#include "Native_interface.hh"
#pragma GCC diagnostic pop

void set_active( bool v );

#define END_TAG "APL_NATIVE_END_TAG"

class LockWrapper
{
public:
    LockWrapper() { set_active( true ); };
    virtual ~LockWrapper() { set_active( false ); };
};

const UCS_string ucs_string_from_string( const std::string &string );

#endif
