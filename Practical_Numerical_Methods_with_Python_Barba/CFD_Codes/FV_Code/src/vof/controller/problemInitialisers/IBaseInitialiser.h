/**
 * Author: Rohan Ramasamy
 * Date: 10/02/2017
 * 
 * Base initialisation class for simulations
 */

#include <vof/data/grid/EulerianGrid.h>
#include <vof/algo/boundaryConditions/IBoundaryCondition.h>
#include <vof/controller/problemInitialisers/InitialGridState.h>


namespace vof {
/**
 * Interface class for initialisers of the grid
 */ 
class IBaseInitialiser
{
public:
	/**
	 * Functions used to initialise the grid
	 */
	virtual std::vector<std::shared_ptr<IEquationOfState> >
	initialiseGrid(
		const std::shared_ptr<EulerianGrid>& grid
		) const = 0;
		
	/**
	 * Abstract function overriden by problem types to set simulation
	 */
	virtual InitialGridState
	getState(
		double xLoc,
		double yLoc,
		double zLoc
		) const = 0;
		
	/**
	 * Abstract function overriden by problem types to set equations of state
	 */
    virtual std::vector<std::shared_ptr<IEquationOfState> >
    getEquationsOfState() const = 0;

	/**
	 * Function to validate grid
	 */
    virtual void
	validate() const = 0;

	/**
	 * Setters for resolution, domain size, and dimension
	 */
    virtual void
	setResolution(
		int dim,
		int newRes
		) = 0;

    virtual void
	setDomainMax(
		int dim,
		int newDimension
		) = 0;

    virtual void
	setDimension(
		int dim
		) = 0;

	virtual void
	setOutputResults(
		bool setOutputResults
		) = 0;

	/**
	 * Accessors for simulation attributes
	 */
    virtual std::array<int, 3>
	simResolution() const  = 0;

    virtual std::array<double, 3>
	simDomainMax() const = 0;

    virtual int
	simDimension() const = 0;

    virtual double
	finalTime() const = 0;

	virtual bool
	outputResults() const = 0;

    virtual std::array<std::shared_ptr<IBoundaryCondition>, 6>
	boundaryConditions() const = 0;
};

}
