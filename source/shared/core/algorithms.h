#pragma once

template<class T, class Pred> inline
T Find( T first, T last, Pred pred )
{
	return std::find_if( first, last, pred );
}

template<class T>
T Random( T min, T max )
{
	std::random_device r;
	std::default_random_engine e1( r() );
	std::uniform_int_distribution<T> uniform_dist( min, max );
	return uniform_dist( e1 );
}

template<class T>
String ToString( T value )
{
	return std::to_string( value );
}

template<class T>
T FromString( const String& text )
{
	T value;
	std::istringstream ss( text );
	ss >> value;

	return value;
}

template< typename T, typename U >
std::pair<T, U> MakePair( T t, U u )
{
	return std::make_pair( t, u );
}

template< typename T >
void ComInterfaceDeleter( T* ptr )
{
	ptr->Release();
}

template<class T, size_t N>
constexpr size_t ArraySize(T (&)[N])
{
    return N;
}
