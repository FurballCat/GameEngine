#include "pch.h"
#include "parsingHandler.h"

using namespace scripts;

void NcParsingHandler::Info(uint32 line, NcTokenType expectedToken)
{
    
}

void NcParsingHandler::Warning(uint32 line, NcTokenType expectedToken)
{
    
}

void NcParsingHandler::Error(uint32 line, NcTokenType expectedToken)
{
    m_hasErrors = true;
}

bool NcParsingHandler::HasErrors() const
{
    return m_hasErrors;
}

bool NcParsingHandler::IsSuccessful() const
{
    return !HasErrors();
}
