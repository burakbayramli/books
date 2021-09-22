/**
 * Author: Rohan Ramasamy
 * Date: 12/02/2017
 *
 * Class for implementing a reflecting boundary condition
 */

#include <vof/algo/boundaryConditions/ReflectingBoundaryCondition.h>


namespace vof {
    void
    ReflectingBoundaryCondition::
    applyBoundaryCondition(
        const std::shared_ptr<EulerianGrid> &grid,
        const size_t &direction,
        const bool &high,
        std::array<int, 3> idx
        ) const
    {
        const int& numGhostCells = grid->numGhostCells();
        int sign = high ? 1 : -1;

        auto copyIdx = idx;
        for (int offset = 0; offset < numGhostCells; ++offset) {
            const double& copiedDensity = grid->densities()(copyIdx);
            const double& copiedPressure = grid->pressures()(copyIdx);
            const double& copiedEnergy = grid->internalEnergies()(copyIdx);
            const auto& copiedVelocity = grid->velocities()(copyIdx);
            std::array<double, 3> reflectedVelocity = copiedVelocity;
            reflectedVelocity[direction] = -reflectedVelocity[direction];

            // Move indexed cell into the ghost region
            idx[direction] += 1 * sign;
            grid->setIdx(idx, copiedDensity, copiedPressure, copiedEnergy, reflectedVelocity);

            // Move copyIdx in the opposite direction from the boundary into the domain
            copyIdx[direction] -= 1 * sign;
        }
    }
}