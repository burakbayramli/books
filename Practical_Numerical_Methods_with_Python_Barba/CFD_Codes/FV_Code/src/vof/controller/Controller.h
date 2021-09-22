/**
 * Author: Rohan Ramasamy
 * Date: 07/02/2017
 */
 
 #pragma once
 
 #include <vof/data/grid/EulerianGrid.h>
 #include <vof/data/eos/eosModel/IEquationOfState.h>
 #include <vof/algo/boundaryConditions/IBoundaryCondition.h>
	
 namespace vof {
    class IBaseInitialiser;
	 
    template<int DIM, typename FluxCalculatorImpl>
	class Controller
	{
	public:
		/**
		 * Constructor for simulation
		 */
		Controller(
			const std::shared_ptr<IBaseInitialiser> initialiser
			);
		
		/**
		 * Function to initialiseGrid
		 */
		void
		initialiseGrid();
		
		/**
		 * Function to complete run simulation
		 */
		std::shared_ptr<EulerianGrid>
		runSimulation();
		
		/**
		 * Evolve simulation by one complete time step
		 */
		double
		evolveTimeStep(
			const double& currentTime
			);
		
		/**
		 * Calculate the time step
		 */
		double
		calculateTimeStep();
		
		/**
		 * Apply boundary conditions to ghost cells
		 */
		void
		setBoundaryConditions();
		
		/**
		 * Calculate the fluxes across all cells
		 */
		void
		evaluateFluxes();
		
		/**
		 * Apply fluxes to cells updating states
		 */
		void
		updateStates(
			double deltaT
			);
		
	private:
		int mDim;
		double mFinalTime;
		bool mOutputResults;
		std::shared_ptr<IBaseInitialiser> mInitialiser;
		std::shared_ptr<EulerianGrid> mGrid;
		std::vector<std::shared_ptr<IEquationOfState> > mEos;
		std::array<std::shared_ptr<IBoundaryCondition>, 2 * DIM> mBoundaryConditions;
		
		/**
		 * Run checks on the simulation domain before running simulation
		 */
		void
		validateSimulation();
	};
 }
 
