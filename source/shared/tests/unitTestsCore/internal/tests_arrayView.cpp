#include "pch.h"
#include "core/public.h"
#include "unitTests/unitTestsFramework.h"

using namespace test;

UNITTEST(ArrayViewTests, Construct1)
{
	uint32 data[] = {0, 1, 2, 3, 4, 5};
	ArrayView<uint32> arr(data, 6);
	
	Assert::AreEqual(ArraySize(data), arr.Size());
	
	if(ArraySize(data) == arr.Size())
	{
		uint32 counter = 0;
		for(uint32 el : arr)
		{
			Assert::AreEqual(data[counter], el);
			counter++;
		}
	}
}

UNITTEST(ArrayViewTests, Construct2)
{
	uint32 data[] = {0, 1, 2, 3, 4, 5};
	ArrayView<uint32> arrTemp(data, 6);
	ArrayView<uint32> arr(arrTemp);
	
	Assert::AreEqual(ArraySize(data), arr.Size());
	
	if(ArraySize(data) == arr.Size())
	{
		uint32 counter = 0;
		for(uint32 el : arr)
		{
			Assert::AreEqual(data[counter], el);
			counter++;
		}
	}
}

UNITTEST(ArrayViewTests, Construct3)
{
	uint32 data[] = {0, 1, 2, 3, 4, 5};
	ArrayView<uint32> arr(data);
	
	Assert::AreEqual(ArraySize(data), arr.Size());
	
	if(ArraySize(data) == arr.Size())
	{
		uint32 counter = 0;
		for(uint32 el : arr)
		{
			Assert::AreEqual(data[counter], el);
			counter++;
		}
	}
}

UNITTEST(StringViewTests, Equal)
{
	const char* text = "This cat is really furry";
	StringView view(text);
	
	Assert::IsTrue(view == text);
}

UNITTEST(StringViewTests, SubString)
{
	const char* text = "This cat is really furry";
	StringView view(text);
	
	StringView sub = view.SubString(0, 4);
	Assert::IsTrue(sub == "This");
	
	sub = view.SubString(5, 3);
	Assert::IsTrue(sub == "cat");
}

UNITTEST(StringViewTests, LeftString)
{
	const char* text = "This cat is really furry";
	StringView view(text);
	
	StringView sub = view.LeftString('s');
	Assert::IsTrue(sub == "Thi");
}
