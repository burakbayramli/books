/**
 * Author: Rohan Ramasamy
 * Date: 12/02/2017
 *
 * Initialisation class for noh simulations
 */

#pragma once

#include <memory>
#include <vof/controller/problemInitialisers/BaseInitialiser.h>

namespace vof {

class NohInitialiser :
    public BaseInitialiser
{
public:
    /**
     * The Noh problem type states are specified already, so the only option the
     * user should really have access to is the axis that the implosion occurs on
     */
    NohInitialiser(
        const int& dim,
        const int& axis,
        const double& finalTime=0.3
        );

    InitialGridState
    getState(
        double xLoc,
        double yLoc,
        double zLoc
        ) const override;

    std::vector<std::shared_ptr<IEquationOfState> >
    getEquationsOfState() const override;

private:
    int mDim;
    RiemannState mInitialState;
    std::array<double, 3> mVelocities;
};

}
