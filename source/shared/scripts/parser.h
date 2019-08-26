#pragma once
#include "core/textParser.h"
#include "scripts/parserCommon.h"
#include "scripts/lexer.h"

namespace scripts
{
    class NcParsingHandler;
    class NcLexer;
    
    class SCRIPTS_API NcParser
    {
    public:
        NcParser(NcLexer& lexer, NcParsingHandler& handler);
        
        bool Parse();
        
    private:
        void InitializeFirstTokens();
        NcTokenType Look(uint32 index = 0);
        const String& LookaheadTokenText(uint32 index = 0);
        void Match(NcTokenType type);
        void Consume();
        uint32 CurrentParsingLine() const;
        
        // Rules
        void Statement();
        void FunctionDefinition();
        void Arguments();
        
        static constexpr uint32 MAX_NC_TOKENS = 5;
        
        NcLexer& m_lexer;
        NcParsingHandler& m_handler;
        
        NcToken m_tokens[MAX_NC_TOKENS];
        uint32 m_nextFreeToken;
        
    };
}
