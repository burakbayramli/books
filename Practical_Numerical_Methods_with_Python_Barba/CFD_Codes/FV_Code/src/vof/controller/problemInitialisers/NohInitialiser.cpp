/**
 * Author: Rohan Ramasamy
 * Date: 10/02/2017
 *
 * Initialisation class for noh simulations
 */

#include <vof/controller/problemInitialisers/NohInitialiser.h>
#include <vof/data/eos/eosModel/IdealGasEquationOfState.h>
#include <vof/algo/boundaryConditions/TransmissiveBoundaryCondition.h>
#include <vof/algo/boundaryConditions/ReflectingBoundaryCondition.h>


namespace vof {

    NohInitialiser::
    NohInitialiser(
        const int& dim,
        const int& axis,
        const double& finalTime
        ) :
        BaseInitialiser(dim, {{100, 1, 1}}, {{0.4, 1.0, 1.0}}),
        mDim(axis),
        mInitialState(1.0, 1e-4, -1.0, std::make_shared<IdealGasEquationOfState>(5.0 / 3.0))
    {
        assert(mDim < 3 && mDim >= 0);
        mVelocities = {{0.0, 0.0, 0.0}};
        mVelocities[axis] = mInitialState.mVelocity;

        for (size_t i = 0; i < mBoundaryConditions.size(); ++i) {
            if (axis * 2 == i) {
                mBoundaryConditions[i] = std::make_shared<ReflectingBoundaryCondition>();
            }
            else {
                mBoundaryConditions[i] = std::make_shared<TransmissiveBoundaryCondition>();
            }
        }
        mFinalTime = finalTime;
    }

    InitialGridState
    NohInitialiser::
    getState(
        double xLoc,
        double yLoc,
        double zLoc
        ) const
    {
        // State is initially constant throughout the domain
        return InitialGridState(mInitialState.mDensity,
                                mInitialState.mPressure,
                                mInitialState.mEos->getInternalEnergyFromDensityAndPressure(mInitialState.mDensity,
                                                                                            mInitialState.mPressure),
                                mVelocities);
    }

    std::vector<std::shared_ptr<IEquationOfState> >
    NohInitialiser::
    getEquationsOfState() const
    {
        std::vector<std::shared_ptr<IEquationOfState> > eosVector;
        eosVector.push_back(mInitialState.mEos);

        return eosVector;
    }
}
