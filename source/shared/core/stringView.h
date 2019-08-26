#pragma once

class StringView : public ArrayView<const char>
{
public:
	using Base = ArrayView<const char>;
	
	StringView() = default;
	
	StringView(const StringView& other)
		: Base(other)
	{
		
	}
	
	StringView& operator=(const StringView& other)
	{
		m_begin = other.m_begin;
		m_end = other.m_end;
		return *this;
	}
	
	StringView(const char* str)
		: Base(str, strlen(str))
	{
		
	}
	
	StringView(const char* str, SizeType size)
	: Base(str, size)
	{
		
	}
	
	template<SizeType TNum>
	StringView(const char (&str)[TNum])
	: Base(str, TNum)
	{
		
	}
	
	StringView SubString(uint32 begin, uint32 length)
	{
		FUR_ASSERT(begin < Size() && begin + length < Size());
		return StringView(Data() + begin, length);
	}
	
	StringView LeftString(const char separator)
	{
		size_t size = Size();
		uint32 i=0;
		
		while(i<size && Get(i) != separator)
			++i;
		
		return SubString(0, i);
	}
	
	bool operator==(const char* other) const
	{
		SizeType size = Size();
		if(size != strlen(other))
			return false;
		
		const char* data1 = Data();
		
		for(uint32 i=0; i<size; ++i)
		{
			if(data1[i] != other[i])
				return false;
		}
		
		return true;
	}
	
	bool operator==(StringView other) const
	{
		SizeType size = Size();
		if(size != other.Size())
			return false;
		
		const char* data1 = Data();
		const char* data2 = other.Data();
		
		for(uint32 i=0; i<size; ++i)
		{
			if(data1[i] != data2[i])
				return false;
		}
		
		return true;
	}
};
