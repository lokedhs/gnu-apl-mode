#ifndef LOCK_WRAPPER_HH
#define LOCK_WRAPPER_HH

#include <pthread.h>

class LockWrapper {
public:
    LockWrapper( pthread_mutex_t *lock_in );
    virtual ~LockWrapper();

private:
    pthread_mutex_t *lock;
};

#endif
