/**
 * Author: Rohan Ramasamy
 * Date: 12/01/2017
 *
 * Struct to hold initial grid state in initialiser classes
 */

#include <vof/controller/problemInitialisers/InitialGridState.h>


namespace vof {

    InitialGridState::
    InitialGridState(
        double density,
        double pressure,
        double internalEnergy,
        std::array<double, 3> velocity
        ) :
        density(density),
        pressure(pressure),
        internalEnergy(internalEnergy),
        velocity(velocity) {}

}