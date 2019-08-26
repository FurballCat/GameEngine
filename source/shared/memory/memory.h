#pragma once
#include <cstdlib>

#ifndef USE_MEM_METRICS
#define USE_MEM_METRICS
#endif

constexpr uint64 operator "" _kb( uint64 sizeInBytes )
{
    return sizeInBytes * 1024;
}

constexpr uint64 operator "" _mb( uint64 sizeInBytes )
{
    return sizeInBytes * 1024 * 1024;
}

namespace mem
{
	enum class Tag
	{
		None = 0,
        Jobs,
		Game,
		World,
		Animation,
		Temporary,
	};

	template<Tag _tag>
	inline uint8* Allocate( uint32 size )
	{
		return reinterpret_cast<uint8*>( malloc( size ) );       // TODO: change it to custom allocator implementation
	}
	
	inline uint8* Allocate( uint32 size, Tag _tag )
	{
		return reinterpret_cast<uint8*>( malloc( size ) );       // TODO: change it to custom allocator implementation
	}

	template<Tag _tag, uint32 _alignment>
	inline uint8* AllocateAligned( uint32 size )
	{
		return nullptr;
	}

	template<Tag _tag>
	inline void Free( uint8* ptr )
	{
		free( reinterpret_cast<void*>( ptr ) );                 // TODO: change it to custom deallocation implementation
	}
	
	inline void Free( uint8* ptr, Tag _tag )
	{
		free( reinterpret_cast<void*>( ptr ) );                 // TODO: change it to custom deallocation implementation
	}

	inline void MemoryCopy(void* destination, const void* source, uint32 size)
	{
		std::memcpy(destination, source, size);
	}
	
	MEMORY_API void TestMemoryExport();
}

struct MemBlock
{
	void* m_ptr;
	uint32 m_size;
};

// Furball Cat Game Engine
struct MemAllocator
{
	const char* m_name;
	void* m_internals;
	
	// todo: add aligned alloc/dealloc
	MemBlock (*alloc_func)(const uint32 size, void* internals, const char* info);
	void (*dealloc_func)(MemBlock block, void* internals, const char* info);
	bool (*owns_func)(void* ptr, void* internals);
};

inline MemBlock memAllocate( const uint32 size, MemAllocator& allocator, const char* info )
{
	return allocator.alloc_func(size, allocator.m_internals, info);
}

inline void memFree( MemBlock block, MemAllocator& allocator, const char* info)
{
	allocator.dealloc_func(block, allocator.m_internals, info);
}

inline void memCopy(void* dst, const void* src, const uint32 size)
{
	memcpy(dst, src, size);
}

inline void memZero(void* dst, const uint32 size)
{
	memset(dst, 0, size);
}

inline void memAllocateInfo( const char* info, MemAllocator& allocator )
{
	//allocator.
}

#define S1(x) #x
#define S2(x) S1(x)

#ifdef USE_MEM_METRICS
	#define FUR_MEM_ALLOC(size, allocator) memAllocate(size, allocator, __FILE__ ":" S2(__LINE__));
	#define FUR_MEM_FREE(blk, allocator) memFree(blk, allocator, __FILE__ ":" S2(__LINE__))
#else
	#define FUR_MEM_ALLOC(size, allocator) memAllocate(size, allocator, NULL)
	#define FUR_MEM_FREE(blk, allocator) memFree(blk, allocator, NULL)
#endif
