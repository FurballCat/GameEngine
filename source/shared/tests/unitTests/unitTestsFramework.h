#pragma once
#include <vector>
#include <functional>
#include <iostream>

namespace test
{
	using TestFunc = std::function<void()>;

	struct Test
	{
		TestFunc m_func;
		const char* m_category;
		const char* m_name;
	};

	class TestRunner
	{
	public:
		static TestRunner& GetInstance()
		{
			static TestRunner runner;
			return runner;
		}

		void RegisterTest(Test* test)
		{
			m_tests.push_back(test);
		}

		void ReportAssert(const char* message = nullptr)
		{
			m_assertsMessagesForCurrentTest.push_back(message);
		}

		void RunAll()
		{
			m_testsFailed = 0;
			m_testsSucceeded = 0;

			std::cout << "Running " << m_tests.size() << " tests." << std::endl;

			for (auto& test : m_tests)
			{
				m_currentTest = test;
				m_assertsMessagesForCurrentTest.clear();

				std::cout << "  Test " << test->m_category << "::" << test->m_name << " ";

				test->m_func();

				if (m_assertsMessagesForCurrentTest.empty() == false)
				{
					std::cout << "failed." << std::endl;
					for (const char* message : m_assertsMessagesForCurrentTest)
					{
						if (message != nullptr)
							std::cout << "    " << message << std::endl;
					}
					m_testsFailed++;
				}
				else
				{
					std::cout << "succeeded." << std::endl;
					m_testsSucceeded++;
				}
			}

			std::cout << std::endl;
			std::cout << "Summary: " << m_testsSucceeded << " succeeded, " << m_testsFailed <<
				" failed." << std::endl;
		}

	private:
		std::vector<Test*> m_tests;
		Test* m_currentTest;
		std::vector<const char*> m_assertsMessagesForCurrentTest;
		unsigned int m_testsSucceeded;
		unsigned int m_testsFailed;
	};

	struct TestInstance
	{
		TestInstance(Test* test)
		{
			TestRunner::GetInstance().RegisterTest(test);
		}
	};

	class Assert
	{
	public:
		static void IsTrue(bool value, const char* message = nullptr)
		{
			if (value == false)
				TestRunner::GetInstance().ReportAssert(message);
		}

		static void IsFalse(bool value, const char* message = nullptr)
		{
			if (value == true)
				TestRunner::GetInstance().ReportAssert(message);
		}

		template<typename T>
		static void AreEqual(const T& a, const T& b, const char* message = nullptr)
		{
			IsTrue(a == b, message);
		}
        
        static void AreEqual(const float& a, const float& b, float tolerance = 0.0f, const char* message = nullptr)
        {
            float diff = a - b;
            IsTrue(-tolerance <= diff && diff <= tolerance, message);
        }
	};
}

#define UNITTEST( category, name )	\
	void Test##category##name();	\
	::test::Test testobject##category##name = {Test##category##name, #category, #name};	\
	::test::TestInstance testinstance##category##name(&testobject##category##name);	\
	void Test##category##name()

#define RUNUNITTESTS() test::TestRunner::GetInstance().RunAll();
