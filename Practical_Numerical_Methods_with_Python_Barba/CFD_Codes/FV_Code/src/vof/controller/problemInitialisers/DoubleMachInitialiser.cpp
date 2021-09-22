/**
 * Author: Rohan Ramasamy
 * Date: 13/02/2017
 *
 * Initialisation class for double mach simulations
 */

#include <vof/data/eos/eosModel/IdealGasEquationOfState.h>
#include <vof/controller/problemInitialisers/DoubleMachInitialiser.h>
#include <vof/controller/problemInitialisers/InitialGridState.h>
#include <vof/algo/boundaryConditions/TransmissiveBoundaryCondition.h>
#include <vof/algo/boundaryConditions/ReflectingBoundaryCondition.h>


namespace vof {
    DoubleMachInitialiser::
    DoubleMachInitialiser() :
            BaseInitialiser(2, {{240, 80, 1}}, {{4.0, 3.0, 0.0}}),
            shockedState(5.714, 116.5, 0.0, std::make_shared<IdealGasEquationOfState>(1.4)),
            unshockedState(1.4, 1.0, 0.0, shockedState.mEos),
            mEos(shockedState.mEos)
    {
        shockedState.mVelocity = 10.0 * unshockedState.mEos->calculateSoundSpeed(unshockedState.mDensity,
                                                                               unshockedState.mPressure);

        for (size_t i = 0; i < mBoundaryConditions.size(); ++i) {
            if (i == 2) {
                mBoundaryConditions[i] = std::make_shared<ReflectingBoundaryCondition>();
            }
            else {
                mBoundaryConditions[i] = std::make_shared<TransmissiveBoundaryCondition>();
            }
        }
        mFinalTime = 0.2;
    }

    InitialGridState
    DoubleMachInitialiser::
    getState(
        double xLoc,
        double yLoc,
        double zLoc
        ) const
    {
        std::array<double, 3> shockedVelocity = {{shockedState.mVelocity * cos(theta),
                                                    -shockedState.mVelocity * sin(theta),
                                                    0.0}};
        std::array<double, 3> unshockedVelocity = {{0.0, 0.0, 0.0}};
        if (xLoc < yLoc * tan(theta)) {
            return InitialGridState(shockedState.mDensity,
                                    shockedState.mPressure,
                                    shockedState.mEos->getInternalEnergyFromDensityAndPressure(shockedState.mDensity,
                                                                                               shockedState.mPressure),
                                    shockedVelocity);
        }
        else {
            return InitialGridState(unshockedState.mDensity,
                                    unshockedState.mPressure,
                                    unshockedState.mEos->getInternalEnergyFromDensityAndPressure(unshockedState.mDensity,
                                                                                                 unshockedState.mPressure),
                                    unshockedVelocity);

        }
    }

    std::vector<std::shared_ptr<IEquationOfState> >
    DoubleMachInitialiser::
    getEquationsOfState() const
    {
        std::vector<std::shared_ptr<IEquationOfState> > eosVector;
        eosVector.push_back(mEos);

        return eosVector;
    }
}
