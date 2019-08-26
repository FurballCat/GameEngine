#pragma once

#include <atomic>

class SpinLock
{
public:
    void Lock()
    {
        while(m_flag.test_and_set(std::memory_order_acquire));
    }
    
    void Unlock()
    {
        m_flag.clear(std::memory_order_release);
    }
    
private:
    std::atomic_flag m_flag;
};

template<typename T>
class ScopedLock
{
public:
    ScopedLock(T& lock)
        : m_lock(lock)
    {
        m_lock.Lock();
    }
    
    ~ScopedLock()
    {
        m_lock.Unlock();
    }
    
private:
    T& m_lock;
};
