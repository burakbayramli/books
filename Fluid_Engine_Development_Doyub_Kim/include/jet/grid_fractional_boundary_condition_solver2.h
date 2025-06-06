// Copyright (c) 2016 Doyub Kim

#ifndef INCLUDE_JET_GRID_FRACTIONAL_BOUNDARY_CONDITION_SOLVER2_H_
#define INCLUDE_JET_GRID_FRACTIONAL_BOUNDARY_CONDITION_SOLVER2_H_

#include <jet/cell_centered_scalar_grid2.h>
#include <jet/grid_boundary_condition_solver2.h>

#include <memory>

namespace jet {

//!
//! \brief Fractional 2-D boundary condition solver for grids.
//!
//! This class constrains the velocity field by projecting the flow to the
//! signed-distance field representation of the collider. This implementation
//! should pair up with GridFractionalSinglePhasePressureSolver2 to provide
//! sub-grid resolutional velocity projection.
//!
class GridFractionalBoundaryConditionSolver2
    : public GridBoundaryConditionSolver2 {
 public:
    //! Default constructor.
    GridFractionalBoundaryConditionSolver2();

    //! Default destructor.
    virtual ~GridFractionalBoundaryConditionSolver2();

    //!
    //! Constrains the velocity field to conform the collider boundary.
    //!
    //! \param velocity Input and output velocity grid.
    //! \param extrapolationDepth Number of inner-collider grid cells that
    //!     velocity will get extrapolated.
    //!
    void constrainVelocity(
        FaceCenteredGrid2* velocity,
        unsigned int extrapolationDepth = 5) override;

    //! Returns the signed distance field of the collider.
    const CellCenteredScalarGrid2& colliderSdf() const;

 protected:
    //! Invoked when a new collider is set.
    void onColliderUpdated(
        const Size2& gridSize,
        const Vector2D& gridSpacing,
        const Vector2D& gridOrigin) override;

 private:
    CellCenteredScalarGrid2 _colliderSdf;
};

//! Shared pointer type for the GridFractionalBoundaryConditionSolver2.
typedef std::shared_ptr<GridFractionalBoundaryConditionSolver2>
    GridFractionalBoundaryConditionSolver2Ptr;

}  // namespace jet

#endif  // INCLUDE_JET_GRID_FRACTIONAL_BOUNDARY_CONDITION_SOLVER2_H_
