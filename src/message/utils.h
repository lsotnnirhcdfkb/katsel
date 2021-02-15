#pragma once

namespace Errors {
    namespace Utils {
        inline int width_of_int(int x) {
            int width = 1;
            while (x /= 10)
                ++width;
            return width;
        }
    }
}
