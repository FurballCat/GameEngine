#pragma once
#include "scripts/parserCommon.h"

namespace scripts
{
    class SCRIPTS_API NcParsingHandler
    {
    public:
        void Info(uint32 line, NcTokenType expectedToken);
        void Warning(uint32 line, NcTokenType expectedToken);
        void Error(uint32 line, NcTokenType expectedToken);
        
        bool HasErrors() const;
        bool IsSuccessful() const;
        
    private:
        bool m_hasErrors = false;
    };
}
