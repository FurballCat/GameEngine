#pragma once
#include "algorithms.h"

/* magic numbers from http://www.isthe.com/chongo/tech/comp/fnv/ */
static const uint32 InitialFNV = 2166136261U;
static const uint32 FNVMultiple = 16777619U;

/* Fowler / Noll / Vo (FNV) Hash */
template<typename T>
struct Hasher
{
    static uint32 MakeHash( const T& )
	{
		return 0;
	}
};

template<>
struct Hasher<String>
{
	static uint32 MakeHash( const String& s )
	{
		uint32 hash = InitialFNV;
		for( uint32 i = 0; i < s.length(); ++i )
		{
			hash = hash ^ ( s[i] );		// xor the low 8 bits
			hash = hash * FNVMultiple;	// multiply by the magic number
		}
		return hash;
	}
};

template <uint32 N, uint32 I>
struct FnvHash
{
	constexpr static uint32 Hash( const char( &str )[N] )
	{
		return ( FnvHash<N, I - 1>::Hash( str ) ^ str[I - 1] ) * 16777619U;
	}
};

template <uint32 N>
struct FnvHash<N, 1>
{
	constexpr static uint32 Hash( const char( &str )[N] )
	{
		return ( InitialFNV ^ str[0] ) * 16777619U;
	}
};

template<uint32 N>
constexpr ResourceID BuildResourceID(const char( &path )[N])
{
	return FnvHash<N, N-1>::Hash( path );
}

inline ResourceID BuildResourceID(const char* path)
{
	uint32 result = InitialFNV;
	const size_t pathLength = strlen(path);
	
	for(size_t i=0; i<pathLength; ++i)
	{
		result = (result ^ path[i]) * 16777619U;
	}
	
	return (uint64)result;
}

template<uint32 N>
constexpr StringHash BuildResourceID(const char( &str )[N])
{
	return FnvHash<N, N-1>::Hash( str );
}

inline StringHash BuildStringHash(const char* str)
{
	StringHash result = InitialFNV;
	const size_t len = strlen(str);
	
	for(size_t i=0; i<len; ++i)
	{
		result = (result ^ str[i]) * FNVMultiple;
	}
	
	return result;
}

class Name
{
public:
	Name()
		: m_name( "" )
		, m_hash( 0 )
	{
	}

	Name( const String& name )
		: m_hash( Hasher<String>::MakeHash( name ) )
		, m_name( name )
	{
		#ifndef FINAL_BUILD
		CheckHashCollision();
		#endif

	}

	template <uint32 N>
	Name( const char( &name )[N] )
		: m_hash( FnvHash<N, N-1>::Hash( name ) )
		, m_name( name )
	{
		#ifndef FINAL_BUILD
		CheckHashCollision();
		#endif

	}

	bool operator==( const Name& ref )
	{
		return m_hash == ref.m_hash;
	}

	bool operator!=( const Name& ref )
	{
		return m_hash != ref.m_hash;
	}

	operator uint32() const
	{
		return m_hash;
	}

	const char* AsText() const
	{
		return m_name.c_str();
	}

	uint32 GetHash() const
	{
		return m_hash;
	}

private:
	#ifndef FINAL_BUILD

	void CheckHashCollision()
	{
		static Map<uint32, String> allNames;

		if( allNames.find( m_hash ) == allNames.end() )
		{
			allNames.insert( MakePair( m_hash, m_name ) );
			return;
		}

		//ASSERT( allNames[m_hash] == m_name, "Different names have the same hash" );
	}

	#endif

private:
	String m_name;
	uint32 m_hash;
};

// for standard libraries
template<>
struct std::hash<Name> final
{
public:
	size_t operator()( const Name& name ) const
	{
		return std::hash<uint32>()( name.GetHash() );
	}
};

// With this you can write "blah"_n instead of Name("blah")
inline Name operator""_n(const char* name, size_t size)
{
	return Name(name);
}
