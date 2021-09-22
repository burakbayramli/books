/**
 * Author: Rohan Ramasamy
 * Date: 01/03/2017
 */


#include <vof/algo/boundaryConditions/BoundaryHandlerImpl.h>


namespace vof {
template<int DIM>
void
BoundaryHandlerImpl<DIM>::
applyBoundaries(
    const std::shared_ptr<EulerianGrid>& grid,
    const std::array<std::shared_ptr<IBoundaryCondition>, 2 * DIM> boundaryConditions
    )
{
    throw std::runtime_error("Not implemented!");
}

template<>
void
BoundaryHandlerImpl<1>::
applyBoundaries(
    const std::shared_ptr<EulerianGrid>& grid,
    const std::array<std::shared_ptr<IBoundaryCondition>, 2> boundaryConditions
    )
{
    int numGhostCells = grid->numGhostCells();
    // Apply x boundary condition
    for (int j = 0; j < grid->numY(); ++j) {
        for (int k = 0; k < grid->numZ(); ++k) {
            boundaryConditions[0]->applyBoundaryCondition(grid, 0, false, {{0, j, k}});
            boundaryConditions[1]->applyBoundaryCondition(grid, 0, true, {{grid->numX() - 1, j, k}});
        }
    }
}

template<>
void
BoundaryHandlerImpl<2>::
applyBoundaries(
    const std::shared_ptr<EulerianGrid>& grid,
    const std::array<std::shared_ptr<IBoundaryCondition>, 4> boundaryConditions
    )
{
    int numGhostCells = grid->numGhostCells();
    // Apply x boundary condition
    for (int j = 0; j < grid->numY(); ++j) {
        for (int k = 0; k < grid->numZ(); ++k) {
            boundaryConditions[0]->applyBoundaryCondition(grid, 0, false, {{0, j, k}});
            boundaryConditions[1]->applyBoundaryCondition(grid, 0, true, {{grid->numX() - 1, j, k}});
        }
    }

    // Apply y boundary condition
    for (int i = -numGhostCells; i < grid->numX() + numGhostCells; ++i) {
        for (int k = 0; k < grid->numZ(); ++k) {
            boundaryConditions[2]->applyBoundaryCondition(grid, 1, false, {{i, 0, k}});
            boundaryConditions[3]->applyBoundaryCondition(grid, 1, true, {{i, grid->numY() - 1, k}});
        }
    }
}

template<>
void
BoundaryHandlerImpl<3>::
applyBoundaries(
    const std::shared_ptr<EulerianGrid>& grid,
    const std::array<std::shared_ptr<IBoundaryCondition>, 6> boundaryConditions
    )
{
    int numGhostCells = grid->numGhostCells();
    // Apply x boundary condition
    for (int j = 0; j < grid->numY(); ++j) {
        for (int k = 0; k < grid->numZ(); ++k) {
            boundaryConditions[0]->applyBoundaryCondition(grid, 0, false, {{0, j, k}});
            boundaryConditions[1]->applyBoundaryCondition(grid, 0, true, {{grid->numX() - 1, j, k}});
        }
    }

    // Apply y boundary condition
    for (int i = -numGhostCells; i < grid->numX() + numGhostCells; ++i) {
        for (int k = 0; k < grid->numZ(); ++k) {
            boundaryConditions[2]->applyBoundaryCondition(grid, 1, false, {{i, 0, k}});
            boundaryConditions[3]->applyBoundaryCondition(grid, 1, true, {{i, grid->numY() - 1, k}});
        }
    }

    // Apply z boundary condition
    for (int i = -numGhostCells; i < grid->numX() + numGhostCells; ++i) {
        for (int j = -numGhostCells; j < grid->numY() + numGhostCells; ++j) {
            boundaryConditions[4]->applyBoundaryCondition(grid, 2, false, {{i, j, 0}});
            boundaryConditions[5]->applyBoundaryCondition(grid, 2, true, {{i, j, grid->numZ() - 1}});
        }
    }
}

template class BoundaryHandlerImpl<1>;
template class BoundaryHandlerImpl<2>;
template class BoundaryHandlerImpl<3>;
}