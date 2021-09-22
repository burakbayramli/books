/**
 * Author: Rohan Ramasamy
 * Date: 08/02/2017
 */
 
 #pragma once
 
 
 namespace vof {
	class IBoundaryCondition 
	{
	public:
		virtual void
		applyBoundaryCondition(
			const std::shared_ptr<EulerianGrid>& grid,
			const size_t& direction,
			const bool& high,
			std::array<int, 3> index
			)const = 0;
	};
 }
