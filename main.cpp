#include <iostream>
#include <cassert>
#include <algorithm>
#include <random>
#include "big_integer.h"

namespace
{
    unsigned const number_of_iterations = 10;
    size_t const number_of_multipliers = 1000;

    int myrand()
    {
        int val = rand() - RAND_MAX / 2;
        if (val != 0)
            return val;
        else
            return 1;
    }
}

int main() {
    for (unsigned itn = 0; itn != number_of_iterations; ++itn)
    {
        std::vector<int> multipliers;

        for (size_t i = 0; i != number_of_multipliers; ++i)
            multipliers.push_back(myrand());

        big_integer accumulator = 1;

        for (size_t i = 0; i != number_of_multipliers; ++i)
            accumulator *= multipliers[i];

        std::shuffle(multipliers.begin(), multipliers.end(), std::mt19937(std::random_device()()));

        for (size_t i = 1; i != number_of_multipliers; ++i)
            accumulator /= multipliers[i];

        assert(accumulator == multipliers[0]);
    }

    return 0;
}