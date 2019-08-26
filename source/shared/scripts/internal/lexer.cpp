#include "pch.h"
#include "lexer.h"

using namespace scripts;

namespace
{
    struct NcKeyword
    {
        const char* m_text;
        NcTokenType m_type;
    };
    
    const NcKeyword c_keywords[] =
    {
        {"(", NcTokenType::LeftParenthesis},
        {")", NcTokenType::RightParenthesis},
        {":", NcTokenType::Colon},
        {",", NcTokenType::Comma},
        {"=", NcTokenType::Equals},
        {"func", NcTokenType::Function},
    };
}

NcLexer::NcLexer(InputStream& stream)
    : m_parser(stream)
{
    
}

NcToken NcLexer::NextToken()
{
    for(const auto& keyword : c_keywords)
    {
        if(m_parser.ParseKeyword(keyword.m_text))
            return {keyword.m_text, keyword.m_type};
    }
    
    String text;
    if(m_parser.ParseIdentifier(text))
        return {text, NcTokenType::Name};
    
    return {"", NcTokenType::Unknown};
}
