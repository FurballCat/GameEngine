#include "pch.h"
#include "core/api.h"
#include "core/types.h"
#include "core/iniParser.h"
#include "unitTests/unitTestsFramework.h"

using namespace test;

UNITTEST(IniParser, SectionParsing)
{
    StringStream stream("[section]");
    String sectionName;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         sectionName = std::move(section);
                     },
                     [&](String key, String value)
                     {
                         // ...
                     });
    
    Assert::IsTrue(sectionName == "section");
}

UNITTEST(IniParser, EntryParsingIdentifiers)
{
    StringStream stream("key=value");
    String keyData;
    String valueData;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         // ...
                     },
                     [&](String key, String value)
                     {
                         keyData = std::move(key);
                         valueData = std::move(value);
                     });
    
    Assert::IsTrue(keyData == "key");
    Assert::IsTrue(valueData == "value");
}

UNITTEST(IniParser, EntryParsingIdentifiersWithNumber)
{
    StringStream stream("blah24=theblah42");
    String keyData;
    String valueData;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         // ...
                     },
                     [&](String key, String value)
                     {
                         keyData = std::move(key);
                         valueData = std::move(value);
                     });
    
    Assert::IsTrue(keyData == "blah24");
    Assert::IsTrue(valueData == "theblah42");
}

UNITTEST(IniParser, EntryParsingNumbers)
{
    StringStream stream("key=42");
    String keyData;
    String valueData;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         // ...
                     },
                     [&](String key, String value)
                     {
                         keyData = std::move(key);
                         valueData = std::move(value);
                     });
    
    Assert::IsTrue(keyData == "key");
    Assert::IsTrue(valueData == "42");
}

UNITTEST(IniParser, SectionAndEntryParsing)
{
    StringStream stream("[section]\nkey=42");
    String sectionName;
    String keyData;
    String valueData;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         sectionName = std::move(section);
                     },
                     [&](String key, String value)
                     {
                         keyData = std::move(key);
                         valueData = std::move(value);
                     });
    
    Assert::IsTrue(sectionName == "section");
    Assert::IsTrue(keyData == "key");
    Assert::IsTrue(valueData == "42");
}

UNITTEST(IniParser, TextInQuotesParsingNumbers)
{
    StringStream stream("key=\"this is a text\"");
    String keyData;
    String valueData;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         // ...
                     },
                     [&](String key, String value)
                     {
                         keyData = std::move(key);
                         valueData = std::move(value);
                     });
    
    Assert::IsTrue(keyData == "key");
    Assert::IsTrue(valueData == "this is a text");
}

UNITTEST(IniParser, MultipleSectionsAndEntriesParsing)
{
    StringStream stream("[section]\nkey=42\nkey2=blah\n[section2]\nstuff=whatever\nthe=thing");
    DynArray<String> sections;
    DynArray<String> keys;
    DynArray<String> values;
    
    INIParser::Parse(stream,
                     [&](String section)
                     {
                         sections.push_back(std::move(section));
                     },
                     [&](String key, String value)
                     {
                         keys.push_back(std::move(key));
                         values.push_back(std::move(value));
                     });
    
    Assert::IsTrue(2 == sections.size());
    Assert::IsTrue(4 == keys.size());
    Assert::IsTrue(4 == values.size());
    
    if (sections.size() == 2 && keys.size() == 4 && values.size() == 4)
    {
        Assert::IsTrue(sections[0] == "section");
        Assert::IsTrue(keys[0] == "key");
        Assert::IsTrue(values[0] == "42");
        
        Assert::IsTrue(keys[1] == "key2");
        Assert::IsTrue(values[1] == "blah");
        
        Assert::IsTrue(sections[1] == "section2");
        Assert::IsTrue(keys[2] == "stuff");
        Assert::IsTrue(values[2] == "whatever");
        
        Assert::IsTrue(keys[3] == "the");
        Assert::IsTrue(values[3] == "thing");
    }
}
