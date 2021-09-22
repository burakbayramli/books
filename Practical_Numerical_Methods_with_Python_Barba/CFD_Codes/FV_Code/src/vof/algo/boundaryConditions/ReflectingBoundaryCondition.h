/**
 * Author: Rohan Ramasamy
 * Date: 12/02/2017
 *
 * Class for implementing a reflecting boundary condition
 */

#pragma once

#include <vof/data/grid/EulerianGrid.h>
#include <vof/algo/boundaryConditions/IBoundaryCondition.h>


namespace vof {
    class ReflectingBoundaryCondition
        : public IBoundaryCondition
    {
    public:
        /**
         * Apply reflecting boundary condition to grid at the index location
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
