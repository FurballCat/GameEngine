#include "pch.h"

#include "core/api.h"
#include "core/types.h"

#include "unitTests/unitTestsFramework.h"

using namespace test;

template<typename KeyType, typename ValueType>
class NaiveHashMap
{
private:
	constexpr static uint32 c_hashCapacity = 10;
	using HashType = std::size_t;
	
	struct Element
	{
		Element* m_next = nullptr;
		KeyType m_key = KeyType();
		ValueType m_value = ValueType();
	};
	
public:
	NaiveHashMap()
	{
		for(uint32 i=0; i<c_hashCapacity; ++i)
		{
			m_heads[i] = nullptr;
		}
	}
	
	void Insert(const KeyType& key, ValueType&& value)
	{
		const uint32 bucketIndex = GetBucketForKey(key);
		
		Element** place = nullptr;
		Element** elem = &m_heads[bucketIndex];
		if(*elem)
		{
			bool found = false;
			while((*elem)->m_next)
			{
				if((*elem)->m_key == key)
				{
					found = true;
					break;
				}
				elem = &((*elem)->m_next);
			}
			
			if(found)
				place = elem;
			else
				place = &((*elem)->m_next);
		}
		else
		{
			place = elem;
		}
		
		if(!(*place))
		{
			*place = new Element();
		}
		
		(*place)->m_key = key;
		(*place)->m_value = std::move(value);
	}
	
	bool Exist(const KeyType& key)
	{
		const uint32 bucketIndex = GetBucketForKey(key);
		
		Element* elem = m_heads[bucketIndex];
		while(elem)
		{
			if(elem->m_key == key)
				return true;
			
			elem = elem->m_next;
		}
		
		return false;
	}
	
private:
	uint32 GetBucketForKey(const KeyType& key)
	{
		HashType hash = std::hash<KeyType>{}(key);
		uint32 bucketIndex = hash % c_hashCapacity;
		return bucketIndex;
	}
	
	Element* m_heads[c_hashCapacity];
};

UNITTEST(HashMap, BasicInsert)
{
	NaiveHashMap<uint32, const char*> map;
	
	map.Insert(42, "Meh");
	map.Insert(23, "Blah");
	
	Assert::IsTrue(map.Exist(42));
	Assert::IsTrue(map.Exist(23));
	Assert::IsFalse(map.Exist(15));
}
