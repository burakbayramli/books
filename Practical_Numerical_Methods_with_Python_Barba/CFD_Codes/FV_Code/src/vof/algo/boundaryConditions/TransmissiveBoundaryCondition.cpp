/**
 * Author: Rohan Ramasamy
 * Date: 08/02/2017
 */
 
 #include <exception>
 
 #include <vof/algo/boundaryConditions/TransmissiveBoundaryCondition.h>
 
 
 namespace vof {
    void
	TransmissiveBoundaryCondition::
	applyBoundaryCondition(
		const std::shared_ptr<EulerianGrid>& grid,
		const size_t& direction,
		const bool& high,
		std::array<int, 3> idx
		) const
	{
		const int& numGhostCells = grid->numGhostCells();
		int sign = high ? 1 : -1;

		const double& copiedDensity = grid->densities()(idx);
		const double& copiedPressure = grid->pressures()(idx);
		const double& copiedEnergy = grid->internalEnergies()(idx);
		const auto& copiedVelocity = grid->velocities()(idx);
		for (int offset = 0; offset < numGhostCells; ++offset) {
			idx[direction] += 1 * sign;
			
			grid->setIdx(idx, copiedDensity, copiedPressure, copiedEnergy, copiedVelocity);
		}
	}
 }
