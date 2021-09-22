/**
 * Author: Rohan Ramasamy
 * Date: 08/02/2017
 */
 
 #pragma once
 
 #include <utility>
 
 #include <vof/data/grid/EulerianGrid.h>
 #include <vof/algo/boundaryConditions/IBoundaryCondition.h>
 
 
 namespace vof {
	class TransmissiveBoundaryCondition
	    : public IBoundaryCondition
	{
	public:
		/**
		 * Apply boundary condition to grid at index location
		 */
		void
		applyBoundaryCondition(
			const std::shared_ptr<EulerianGrid>& grid,
			const size_t& direction,
			const bool& high,
			std::array<int, 3> index
			) const override;
	};
 }
