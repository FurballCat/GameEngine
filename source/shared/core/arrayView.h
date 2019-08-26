#pragma once

template<typename T>
class ArrayView
{
public:
	using SizeType = size_t;
	
	ArrayView() = default;
	
	ArrayView(const ArrayView<T>& other)
		: m_begin(other.m_begin)
		, m_end(other.m_end)
	{
	}
	
	ArrayView<T>& operator=(const ArrayView<T>& other)
	{
		m_begin = other.m_begin;
		m_end = other.m_end;
		return *this;
	}
	
	ArrayView(T* begin, T* end)
		: m_begin(begin)
		, m_end(end)
	{
	}
	
	ArrayView(T* ptr, SizeType num)
		: m_begin(ptr)
		, m_end(ptr + num)
	{
	}
	
	template<typename TData, SizeType TNum>
	ArrayView(TData (&ptr)[TNum])
		: m_begin(ptr)
		, m_end(ptr + TNum)
	{
		static_assert(std::is_same<T, TData>::value, "Types provided are not the same.");
	}
	
	T& operator[](SizeType index)
	{
		FUR_ASSERT(m_begin + index < m_end);
		return m_begin[index];
	}
	
	const T& operator[](SizeType index) const
	{
		FUR_ASSERT(m_begin + index < m_end);
		return m_begin[index];
	}
	
	T& Get(SizeType index)
	{
		FUR_ASSERT(m_begin + index < m_end);
		return m_begin[index];
	}
	
	const T& Get(SizeType index) const
	{
		FUR_ASSERT(m_begin + index < m_end);
		return m_begin[index];
	}
	
	SizeType Size() const
	{
		return m_end - m_begin;
	}
	
	SizeType DataSize() const
	{
		return (m_end - m_begin) * sizeof(T);
	}
	
	T* Data()
	{
		return m_begin;
	}
	
	const T* Data() const
	{
		return m_begin;
	}
	
	T* Begin() { return m_begin; }
	T* End() { return m_end; }
	
	const T* Begin() const { return m_begin; }
	const T* End() const { return m_end; }
	
	T* begin() { return m_begin; }
	T* end() { return m_end; }
	
	const T* begin() const { return m_begin; }
	const T* end() const { return m_end; }
	
protected:
	T* m_begin = nullptr;
	T* m_end = nullptr;
};
