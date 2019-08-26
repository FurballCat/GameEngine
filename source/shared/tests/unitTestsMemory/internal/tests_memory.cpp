#include "pch.h"
#include "unitTests/unitTestsFramework.h"
#include "core/api.h"
#include "core/types.h"
#include "memory/memory.h"

using namespace test;
using namespace mem;

UNITTEST(Memory, SimpleAllocation)
{
    uint8* ptr = mem::Allocate<Tag::Game>(64_mb);
    Assert::IsTrue(ptr != nullptr);
    mem::Free<Tag::Game>(ptr);
}
