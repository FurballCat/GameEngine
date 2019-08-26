#include "pch.h"
#include "parser.h"
#include "parsingHandler.h"

using namespace scripts;

NcParser::NcParser(NcLexer& lexer, NcParsingHandler& handler)
    : m_lexer(lexer)
    , m_handler(handler)
{
    
}

bool NcParser::Parse()
{
    InitializeFirstTokens();
    Statement();
    return m_handler.IsSuccessful();
}

void NcParser::InitializeFirstTokens()
{
    for(auto& token : m_tokens)
        token = m_lexer.NextToken();
}

NcTokenType NcParser::Look(uint32 index)
{
    const uint32 id = (m_nextFreeToken + index) % MAX_NC_TOKENS;
    return m_tokens[id].m_type;
}

const String& NcParser::LookaheadTokenText(uint32 index)
{
    const uint32 id = (m_nextFreeToken + index) % MAX_NC_TOKENS;
    return m_tokens[id].m_text;
}

void NcParser::Match(NcTokenType type)
{
    if(Look(0) == type)
        Consume();
    else
        m_handler.Error(CurrentParsingLine(), type);
}

void NcParser::Consume()
{
    m_tokens[m_nextFreeToken] = m_lexer.NextToken();
    m_nextFreeToken = (m_nextFreeToken + 1) % MAX_NC_TOKENS;
}

uint32 NcParser::CurrentParsingLine() const
{
    return 0;   // TODO
}

void NcParser::Statement()
{
    if (Look(0) == NcTokenType::Function && Look(1) == NcTokenType::Name && Look(2) == NcTokenType::LeftParenthesis)
    {
        FunctionDefinition();
    }
    else
    {
        m_handler.Error(CurrentParsingLine(), NcTokenType::Unknown);
    }
}

void NcParser::FunctionDefinition()
{
    // Rule: 'func' NAME '(' arguments ')' ':'
    // Example: func Fun(arg1, arg2):
    
    Match(NcTokenType::Function);
    Match(NcTokenType::Name);
    Match(NcTokenType::LeftParenthesis);
    
    Arguments();
    
    Match(NcTokenType::RightParenthesis);
    Match(NcTokenType::Colon);
}

void NcParser::Arguments()
{
    // Rule: NAME? (COMMA NAME)*
    // Example: arg1, arg2, arg3
    
    if(Look(0) == NcTokenType::Name)
        Match(NcTokenType::Name);
    
    while(Look(0) == NcTokenType::Comma)
    {
        Match(NcTokenType::Comma);
        Match(NcTokenType::Name);
    }
}
