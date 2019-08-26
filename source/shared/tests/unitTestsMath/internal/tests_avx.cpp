#include "pch.h"

#include "math/types.h"
#include "math/vectorOperations.h"

#include "unitTests/unitTestsFramework.h"

using namespace test;
using namespace math;

UNITTEST(Math, AVX)
{
    Vector4 a = {1.0f, 2.0f, 3.0f, 1.0f};
    Vector4 b = {2.0f, 2.0f, 4.0f, 1.0f};
    
    Vector4 c = a + b;
    
    Assert::AreEqual(c.y, 4.0f, 0.0001f);
}
