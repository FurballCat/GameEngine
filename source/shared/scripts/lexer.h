#pragma once
#include "core/textParser.h"
#include "scripts/parserCommon.h"

namespace scripts
{
    struct SCRIPTS_API NcToken
    {
        String m_text;  // TODO: can be optimized if it will be a StringView
        NcTokenType m_type;
    };
    
    class SCRIPTS_API NcLexer
    {
    public:
        NcLexer(InputStream& stream);
        
        NcToken NextToken();
        
    private:
        TextParser m_parser;
    };
}
