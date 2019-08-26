#pragma once

class CORE_API TextParser
{
public:
	TextParser( InputStream& stream );

	bool ParseFloat( float& x );
	bool ParseFloat2( float& x, float& y );
	bool ParseFloat3( float& x, float& y, float& z );
	bool ParseFloat4( float& x, float& y, float& z, float& w );

	bool ParseInt( int32& x );
	bool ParseInt2( int32& x, int32& y );
	bool ParseInt3( int32& x, int32& y, int32& z );
	bool ParseInt4( int32& x, int32& y, int32& z, int32& w );

	bool ParseKeyword( const String& keyword );
	bool ParseIdentifier( String& identifier );

	bool ParseText( String& text );

	bool ParseNonWhitespaceCharacter( char& chr );
	
	bool IsParsing() const;
	
	static bool IsDigit( char chr );
	static bool IsAlpha( char chr );
	static bool IsAlphaNumeric( char chr );
	static bool IsWhitespace( char chr );
	static bool IsQuotation( char chr );

private:
	InputStream& m_stream;
};
