#include "pch.h"

#include "core/api.h"
#include "core/types.h"

#include "scripts/api.h"
#include "scripts/parserCommon.h"
#include "scripts/lexer.h"
#include "scripts/parser.h"
#include "scripts/parsingHandler.h"

#include "unitTests/unitTestsFramework.h"

using namespace test;
using namespace scripts;

UNITTEST(Scripts, FunctionParsing)
{
    StringStream stream("func Fun(arg1, arg2):");
    NcLexer lexer(stream);
    NcParsingHandler handler;
    NcParser parser(lexer, handler);
    
    parser.Parse();
    
    Assert::IsTrue(handler.IsSuccessful());
}

UNITTEST(Scripts, InvalidFunctionParsing)
{
    StringStream stream("func (arg1, arg2):");
    NcLexer lexer(stream);
    NcParsingHandler handler;
    NcParser parser(lexer, handler);
    
    parser.Parse();
    
    Assert::IsTrue(handler.HasErrors());
}
