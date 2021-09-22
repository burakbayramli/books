/**
 * Author: Rohan Ramasamy
 * Date: 12/01/2017
 *
 * Struct to hold initial grid state in initialiser classes
 */

#pragma once

#include<array>


namespace vof {
/**
 * Struct to hold the initial grid state at a point in the domain
 */
struct InitialGridState
{
    InitialGridState(
        double density,
        double pressure,
        double internalEnergy,
        std::array<double, 3> velocity
        );

    double density;
    double pressure;
    double internalEnergy;
    std::array<double, 3> velocity;
};

}
