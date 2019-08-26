#include "pch.h"
#include "core/api.h"
#include "core/types.h"
#include "core/textParser.h"
#include "unitTests/unitTestsFramework.h"

using namespace test;

UNITTEST(TextParser, VectorParsing)
{
    float x = -1.0f, y = -1.0f, z = -1.0f, w = -1.0f;
    float tolerance = 0.0001f;
    
    {
        StringStream stream("0.0 0.0 0.0");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseFloat3(x, y, z));
        Assert::AreEqual(0.0f, x, tolerance);
        Assert::AreEqual(0.0f, y, tolerance);
        Assert::AreEqual(0.0f, z, tolerance);
    }
    
    {
        StringStream stream("0.5 2 -3.12 1234");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseFloat4(x, y, z, w));
        Assert::AreEqual(0.5f, x, tolerance);
        Assert::AreEqual(2.0f, y, tolerance);
        Assert::AreEqual(-3.12f, z, tolerance);
        Assert::AreEqual(1234.0f, w, tolerance);
    }
    
    {
        StringStream stream("0 0");
        TextParser parser(stream);
        Assert::IsFalse(parser.ParseFloat3(x, y, z));
    }
}

UNITTEST(TextParser, KeywordParsing)
{
    {
        StringStream stream("p:");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseKeyword("p:"));
    }
    
    {
        StringStream stream("test");
        TextParser parser(stream);
        
        Assert::IsFalse(parser.ParseKeyword("blah"));
        Assert::IsTrue(parser.ParseKeyword("test"));
    }
}

UNITTEST(TextParser, PositionParsing)
{
    float x = 0.0f, y = 0.0f, z = 0.0f;
    float tolerance = 0.0001f;
    
    {
        StringStream stream("p: 1 2 3");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseKeyword("p:"));
        Assert::IsTrue(parser.ParseFloat3(x, y, z));
        Assert::AreEqual(1.0f, x, tolerance);
        Assert::AreEqual(2.0f, y, tolerance);
        Assert::AreEqual(3.0f, z, tolerance);
    }
}

UNITTEST(TextParser, IntParsing)
{
    int value = 0;
    
    {
        StringStream stream("36");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseInt(value));
        Assert::AreEqual(36, value);
    }
}

UNITTEST(TextParser, IdentifierParsing)
{
    std::string identifier;
    
    {
        StringStream stream("test");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseIdentifier(identifier));
        Assert::AreEqual(std::string("test"), identifier);
    }
    
    {
        StringStream stream("blah=36");
        TextParser parser(stream);
        
        Assert::IsTrue(parser.ParseIdentifier(identifier));
        Assert::AreEqual(std::string("blah"), identifier);
    }
}

UNITTEST(TextParser, ExampleFormatParsing)
{
    StringStream stream(
                             "#helixmesh\n"
                             "name: cubeMesh1\n\n"
                             "vertices: 1\n"
                             "p: 1 2 3\n"
                             "n: 0 0 1\n");
    
    TextParser parser(stream);
    Assert::IsTrue(parser.ParseKeyword("#helixmesh"));
    Assert::IsTrue(parser.ParseKeyword("name:"));
    
    std::string name;
    Assert::IsTrue(parser.ParseIdentifier(name));
    Assert::AreEqual(std::string("cubeMesh1"), name);
    
    Assert::IsTrue(parser.ParseKeyword("vertices:"));
    
    int numVertices = 0;
    
    Assert::IsTrue(parser.ParseInt(numVertices));
    
    float x = -1.0f, y = -1.0f, z = 1.0f;
    float tolerance = 0.0001f;
    
    Assert::IsTrue(parser.ParseKeyword("p:"));
    Assert::IsTrue(parser.ParseFloat3(x, y, z));
    Assert::AreEqual(x, 1.0f, tolerance);
    Assert::AreEqual(y, 2.0f, tolerance);
    Assert::AreEqual(z, 3.0f, tolerance);
    
    Assert::IsTrue(parser.ParseKeyword("n:"));
    Assert::IsTrue(parser.ParseFloat3(x, y, z));
    Assert::AreEqual(x, 0.0f, tolerance);
    Assert::AreEqual(y, 0.0f, tolerance);
    Assert::AreEqual(z, 1.0f, tolerance);
}

UNITTEST(TextParser, ParsingFloats)
{
    StringStream stream("2.0937e-019");
    TextParser parser(stream);
    
    float f;
    Assert::IsTrue(parser.ParseFloat(f));
    Assert::AreEqual(2.0937e-019f, f);
}

UNITTEST(TextParser, ParsingText)
{
    {
        StringStream stream("this is a text");
        TextParser parser(stream);
        
        String text;
        Assert::IsTrue(parser.ParseText(text));
        Assert::IsTrue(text == "this");
    }
    {
        StringStream stream("\"this is a text\" and blah blah");
        TextParser parser(stream);
        
        String text;
        Assert::IsTrue(parser.ParseText(text));
        Assert::IsTrue(text == "this is a text");
    }
    {
        StringStream stream("\'this is a text\' and blah blah");
        TextParser parser(stream);
        
        String text;
        Assert::IsTrue(parser.ParseText(text));
        Assert::IsTrue(text == "this is a text");
    }
    {
        StringStream stream("42 is a text");
        TextParser parser(stream);
        
        String text;
        Assert::IsTrue(parser.ParseText(text));
        Assert::IsTrue(text == "42");
    }
    {
        StringStream stream("2.0937e-019 is a text");
        TextParser parser(stream);
        
        String text;
        Assert::IsTrue(parser.ParseText(text));
        Assert::IsTrue(text == "2.0937e-019");
    }
}
