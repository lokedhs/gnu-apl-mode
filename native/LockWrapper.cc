#include "LockWrapper.hh"

LockWrapper::LockWrapper( pthread_mutex_t *lock_in ) : lock( lock_in )
{
    pthread_mutex_lock( lock );
}

LockWrapper::~LockWrapper()
{
    pthread_mutex_unlock( lock );
}
