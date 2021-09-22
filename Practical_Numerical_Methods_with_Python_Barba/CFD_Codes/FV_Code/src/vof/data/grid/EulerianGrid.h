/**
 * Author: Rohan Ramasamy
 * Date: 05/02/2017
 */
 
 #pragma once
 
 #include <vof/data/grid/kernel/StateGrid.h>
 #include <vof/data/geom/Vector.h>
 #include <vof/data/eos/eosModel/IEquationOfState.h>
 #include <vof/data/eos/eosProperties/IdealEquationOfStateProperties.h>


namespace vof {
	 class EulerianGrid
	 {
	 public:
		EulerianGrid(
			const int& numX,
			const int& numY = 1,
			const int& numZ = 1,
			const int& numGhostCells = 1,
			const Vector<3>& cellDiagonal = Vector<3>(1.0)
			);
		
		/**
		 * Check functions
		 */
	    bool
	    gridInitialised() const { return mGridInitialised; }
	    
	    bool
	    fluxesSet() const { return mFluxesSet; }
		
		/**
		 * Accessors
		 */	
	    const std::array<int, 3>&
	    cellResolution() { return mCellResolution; }
		 
	    const int&
	    numX() { return mCellResolution[0]; }
	    
	    const int&
	    numY() { return mCellResolution[1]; }
	    
	    const int&
	    numZ() { return mCellResolution[2]; }
	    
	    const int&
	    numGhostCells() { return mNumGhostCells; }
		 
	    const Vector<3>&
		cellSize() { return mCellDiagonal; }
		 
		const StateGrid<double>&
		densities() { return mDensities; }
		
		const StateGrid<double>&
		pressures() { return mPressures; }
		
		const StateGrid<std::array<double, 3> >&
		velocities() { return mVelocities; }
		
		const StateGrid<double>&
		internalEnergies() { return mInternalEnergies; }
		
		/**
		 * Accessors to flux variables
		 */
		StateGrid<std::array<double, 3> >&
		densityFluxes() { return mDensityFluxes; }
		
		StateGrid<std::array<std::array<double, 3>, 3> >&
		momentumFluxes() { return mMomentumFluxes; }
		
		StateGrid<std::array<double, 3> >&
		energyFluxes() { return mEnergyFluxes; }
		
	 private:
		// EulerianGrid resolution
		std::array<int, 3> mCellResolution;
		int mNumGhostCells;
	 
		// State arrays
		StateGrid<double> mDensities;
		StateGrid<double> mPressures;
		StateGrid<std::array<double, 3> > mVelocities;
		StateGrid<double> mInternalEnergies;
		
		// Flux arrays
		StateGrid<std::array<double, 3> > mDensityFluxes;
		StateGrid<std::array<std::array<double, 3>, 3> > mMomentumFluxes;
		StateGrid<std::array<double, 3> > mEnergyFluxes;
	    StateGrid<IdealEquationOfStateProperties> mEosProps;
		
		// Cell dimensions
		Vector<3> mCellDiagonal;
		
		// Check functions on grid
		bool mGridInitialised;
		bool mFluxesSet;
		
		// Allow friend class access to private members
		friend class BaseInitialiser;
		friend class TransmissiveBoundaryCondition;
	    friend class ReflectingBoundaryCondition;
	    template<int DIM> friend class FluxCalculatorGodunovImpl;
	    template<int DIM> friend class CellUpdaterImpl;
		
		// Function to set state variables on a cell
		void
		setIdx(
			const std::array<int, 3>& idx,
			const double& density,
			const double& pressure,
			const double& internalEnergy,
			const std::array<double, 3>& velocities
			);
			
		void
		setGridInitialised() { mGridInitialised = true; }
			
		void
		setFluxes() { mFluxesSet = true; }
	 };
 }
