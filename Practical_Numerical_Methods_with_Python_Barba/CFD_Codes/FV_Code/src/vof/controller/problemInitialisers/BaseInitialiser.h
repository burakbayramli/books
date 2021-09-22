/**
 * Author: Rohan Ramasamy
 * Date: 01/03/2017
 */

#pragma once

#include <vof/data/grid/EulerianGrid.h>
#include <vof/algo/boundaryConditions/IBoundaryCondition.h>
#include <vof/controller/problemInitialisers/IBaseInitialiser.h>
#include <vof/controller/problemInitialisers/InitialGridState.h>


namespace vof {
class BaseInitialiser :
    public IBaseInitialiser
{
public:
    BaseInitialiser(
        int dim,
        std::array<int, 3> resolutions,
        std::array<double, 3> dimesions
        );

    /**
     * Functions used to initialise the grid
     */
    std::vector<std::shared_ptr<IEquationOfState> >
    initialiseGrid(
        const std::shared_ptr<EulerianGrid>& grid
        ) const override;

    /**
     * Function to validate grid
     */
    void
    validate() const override;

    /**
     * Setters for resolution, domain size, and dimension
     */
    void
    setResolution(
        int dim,
        int newRes
        ) override
    {
        assert(dim < 3 && dim >= 0);
        mSimResolution[dim] = newRes;
    }
    void
    setDomainMax(
        int dim,
        int newDimension
        ) override
    {
        assert(dim < 3 && dim >= 0);
        mSimDimensions[dim] = newDimension;
    }

    void
    setDimension(
        int dim
        ) override
    {
        mDim = dim;
    }

    void
    setOutputResults(
        bool outputResults
        ) override
    {
        mOutputResults = outputResults;
    }

    /**
     * Accessors for simulation attributes
     */
    std::array<int, 3>
    simResolution() const override { return mSimResolution; }

    std::array<double, 3>
    simDomainMax() const override { return mSimDimensions; }

    int
    simDimension() const override { return mDim; }

    double
    finalTime() const override { return mFinalTime; }

    bool
    outputResults() const override { return mOutputResults; }

    std::array<std::shared_ptr<IBoundaryCondition>, 6>
    boundaryConditions() const override { return mBoundaryConditions; }

protected:
    int mDim;
    std::array<std::shared_ptr<IBoundaryCondition>, 6> mBoundaryConditions;
    double mFinalTime;
    bool mOutputResults;
    std::array<int, 3> mSimResolution;
    std::array<double, 3> mSimDimensions;
};
}
