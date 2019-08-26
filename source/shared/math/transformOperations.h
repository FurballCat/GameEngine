#pragma once
#include "api.h"
#include "types.h"

namespace math
{
    inline Transform CreateTransformIdentity()
    {
        return {{0, 0, 0, 0}, {0, 0, 0, 1}};
    }
}
