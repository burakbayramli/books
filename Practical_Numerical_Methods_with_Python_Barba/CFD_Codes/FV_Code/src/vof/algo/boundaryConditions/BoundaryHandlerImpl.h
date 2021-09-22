/**
 * Author: Rohan Ramasamy
 * Date: 01/03/2017
 */

#pragma once


#include <memory>

#include <vof/data/grid/EulerianGrid.h>
#include <vof/algo/boundaryConditions/IBoundaryCondition.h>


namespace vof {
template<int DIM>
class BoundaryHandlerImpl {
public:
    /**
     * Function to apply the boundary conditions to the high and low boundaries of the domain
     */
    static void
    applyBoundaries(
        const std::shared_ptr<EulerianGrid>& grid,
        const std::array<std::shared_ptr<IBoundaryCondition>, 2 * DIM> boundaryConditions
        );
};

}
