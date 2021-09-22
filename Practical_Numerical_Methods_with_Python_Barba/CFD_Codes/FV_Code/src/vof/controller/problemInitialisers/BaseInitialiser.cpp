/**
 * Author: Rohan Ramasamy
 * Date: 01/03/2017
 *
 * Base initialisation class for simulations
 */

#include <vof/controller/problemInitialisers/BaseInitialiser.h>
#include <vof/data/geom/Point.h>
#include <vof/data/eos/eosModel/IEquationOfState.h>


namespace vof {

BaseInitialiser::
BaseInitialiser(
    int dim,
    std::array<int, 3> resolutions,
    std::array<double, 3> dimensions
    ) :
    mDim(dim),
    mOutputResults(true),
    mSimResolution(resolutions),
    mSimDimensions(dimensions) {}

std::vector<std::shared_ptr<IEquationOfState> >
BaseInitialiser::
initialiseGrid(
    const std::shared_ptr<EulerianGrid>& grid
    ) const
{
    auto resolution = grid->cellResolution();
    auto cellSize = grid->cellSize();

    for (int i = 0; i < resolution[0]; ++i) {
        for (int j = 0; j < resolution[1]; ++j) {
            for (int k = 0; k < resolution[2]; ++k) {
                double xLoc = (i + 0.5) * cellSize[0];
                double yLoc = (j + 0.5) * cellSize[1];
                double zLoc = (k + 0.5) * cellSize[2];

                InitialGridState currentState = this->getState(xLoc, yLoc, zLoc);

                grid->setIdx({{i, j, k}},
                             currentState.density,
                             currentState.pressure,
                             currentState.internalEnergy,
                             currentState.velocity);
            }
        }
    }
    grid->setGridInitialised();

    // Set up equations of state
    auto equationsOfState = this->getEquationsOfState();
    return equationsOfState;
}

void
BaseInitialiser::
validate() const
{
    assert(mDim < 3 && mDim > 0);
    for (int dim = 0; dim < mDim; ++dim) {
        assert(mSimResolution[dim] > 1);
    }
    for (const auto& bc : mBoundaryConditions) {
        assert(bc != nullptr);
    }
    assert(mFinalTime > 0.0);
}

}
