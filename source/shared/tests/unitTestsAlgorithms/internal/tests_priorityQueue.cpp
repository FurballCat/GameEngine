#include "pch.h"

#include "core/api.h"
#include "core/types.h"

#include "unitTests/unitTestsFramework.h"

using namespace test;

template<typename T>
class PriorityQueue
{
public:
	using PriorityType = uint32;
	
private:
	struct Element
	{
		PriorityType m_priority = 0;
		T m_value = T();
		
		Element* m_next = nullptr;
		Element* m_prev = nullptr;
	};
	
public:
	~PriorityQueue()
	{
		Element* elem = m_head;
		while(elem != nullptr)
		{
			Element* prevElem = elem;
			elem = elem->m_next;
			
			delete prevElem;
		}
	}
	
	void PushBack(PriorityType priority, T&& element)
	{
		if(m_head == nullptr)
		{
			m_head = new Element();
			m_head->m_priority = priority;
			m_head->m_value = std::move(element);
			m_head->m_prev = nullptr;
			
			return;
		}
		
		Element* elem = m_head;
		while(elem->m_priority < priority && elem->m_next != nullptr)
		{
			elem = elem->m_next;
		}
		
		Element* ptr = new Element();
		ptr->m_priority = priority;
		ptr->m_value = std::move(element);
		
		if(elem->m_priority > priority)
		{
			if(elem->m_prev)
			{
				elem->m_prev->m_next = ptr;
			}
			
			ptr->m_prev = elem->m_prev;
			elem->m_prev = ptr;
			ptr->m_next = elem;
			
			if(elem == m_head)
			{
				m_head = ptr;
			}
		}
		else
		{
			if(elem->m_next)
			{
				elem->m_next->m_prev = ptr;
			}
			
			ptr->m_next = elem->m_next;
			elem->m_next = ptr;
			ptr->m_prev = elem;
		}
	}
	
	class iterator
	{
	public:
		iterator()
		: m_elem(nullptr)
		{
			
		}
		
		iterator(iterator&& other)
		: m_elem(other.m_elem)
		{
			
		}
		
		iterator(iterator& other)
		: m_elem(other.m_elem)
		{
			
		}
		
		iterator(Element* elem)
		: m_elem(elem)
		{
			
		}
		
		T& operator*()
		{
			return m_elem->m_value;
		}
		
		iterator& operator++()
		{
			m_elem = m_elem->m_next;
			return *this;
		}
		
		iterator& operator--()
		{
			m_elem = m_elem->m_prev;
			return *this;
		}
		
		iterator& operator++(int)
		{
			m_elem = m_elem->m_next;
			return *this;
		}
		
		iterator& operator--(int)
		{
			m_elem = m_elem->m_prev;
			return *this;
		}
		
		bool operator!=(iterator& it)
		{
			return m_elem != it.m_elem;
		}
		
		bool operator==(iterator& it)
		{
			return m_elem == it.m_elem;
		}
		
	private:
		Element* m_elem = nullptr;
	};
	
	iterator begin()
	{
		return iterator(m_head);
	}
	
	iterator end()
	{
		return iterator(nullptr);
	}
	
private:
	Element* m_head = nullptr;
};

namespace std
{
	template<typename T>
	typename PriorityQueue<T>::iterator begin(PriorityQueue<T>& queue)
	{
		return queue.begin();
	}
	
	template<typename T>
	typename PriorityQueue<T>::iterator end(PriorityQueue<T>& queue)
	{
		return queue.end();
	}
}

UNITTEST(PriorityQueue, InOrder)
{
	PriorityQueue<uint32> queue;
	
	queue.PushBack(0, 42);
	queue.PushBack(1, 33);
	
	uint32 i = 0;
	for(auto& it : queue)
	{
		if(i==0)
			Assert::AreEqual(42, it);
		else if(i==1)
			Assert::AreEqual(33, it);
		
		i += 1;
	}
}

UNITTEST(PriorityQueue, OutOfOrder)
{
	PriorityQueue<uint32> queue;
	
	queue.PushBack(1, 33);
	queue.PushBack(0, 42);
	
	uint32 i = 0;
	for(auto& it : queue)
	{
		if(i==0)
			Assert::AreEqual(42, it);
		else if(i==1)
			Assert::AreEqual(33, it);
		
		i += 1;
	}
}

UNITTEST(PriorityQueue, LongerQueue)
{
	PriorityQueue<uint32> queue;
	
	queue.PushBack(1, 33);
	queue.PushBack(2, 56);
	queue.PushBack(0, 42);
	
	uint32 i = 0;
	for(auto& it : queue)
	{
		if(i==0)
			Assert::AreEqual(42, it);
		else if(i==1)
			Assert::AreEqual(33, it);
		else if(i==2)
			Assert::AreEqual(56, it);
		
		i += 1;
	}
}

UNITTEST(PriorityQueue, ReleaseMemory)
{
	struct GlobalElem
	{
		GlobalElem()
		: m_counter(nullptr)
		{
			
		}
		
		GlobalElem(GlobalElem&& other)
		: m_counter(other.m_counter)
		{
			other.m_counter = nullptr;
		}
		
		GlobalElem& operator=(GlobalElem&& other)
		{
			m_counter = other.m_counter;
			other.m_counter = nullptr;
			return *this;
		}
		
		GlobalElem(uint32* counter)
		: m_counter(counter)
		{
			*m_counter += 1;
		}
		
		~GlobalElem()
		{
			if(m_counter)
			{
				*m_counter -= 1;
			}
		}
		
		uint32* m_counter = nullptr;
	};
	
	uint32 counter = 0;
	
	{
		PriorityQueue<GlobalElem> queue;
		queue.PushBack(1, GlobalElem(&counter));
		queue.PushBack(2, GlobalElem(&counter));
		queue.PushBack(0, GlobalElem(&counter));
		
		Assert::AreEqual(3, counter);
	}
	
	Assert::AreEqual(0, counter);
}
