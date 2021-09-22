/**
 * Author: Rohan Ramasamy
 * Date: 13/02/2017
 *
 * Initialisation class for double mach simulations
 */

#pragma once

#include <vof/controller/problemInitialisers/BaseInitialiser.h>
#include <math.h>


namespace vof {
    class DoubleMachInitialiser
        : public BaseInitialiser
    {
    public:
        DoubleMachInitialiser();

        InitialGridState
        getState(
            double xLoc,
            double yLoc,
            double zLoc
            ) const override;

        std::vector<std::shared_ptr<IEquationOfState> >
        getEquationsOfState() const override;

    private:
        static constexpr double theta = M_PI / 6.0;
        RiemannState shockedState;
        RiemannState unshockedState;

        std::shared_ptr<IEquationOfState> mEos;
    };
}
