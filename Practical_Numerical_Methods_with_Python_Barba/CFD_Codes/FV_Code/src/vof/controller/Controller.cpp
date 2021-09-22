/**
 * Author: Rohan Ramasamy
 * Date: 07/02/2017
 */
 
 #include <exception>
 #include <iostream>

 #include <chrono>
 
 #include <vof/controller/Controller.h>
 #include <vof/algo/timeStepCalculator/TimeStepCalculator.h>
 #include <vof/algo/boundaryConditions/BoundaryHandlerImpl.h>
 #include <vof/algo/fluxCalculators/FluxCalculatorGodunovImpl.h>
 #include <vof/algo/gridUpdaters/CellUpdaterImpl.h>
 #include <vof/io/VTKWriter.h>
 #include <vof/controller/problemInitialisers/IBaseInitialiser.h>
 #include <vof/statistics/ScopedTimer.h>


namespace vof {
     template <int DIM, typename FluxCalculatorImpl >
     void
     Controller<DIM, FluxCalculatorImpl>::
     validateSimulation()
     {
         assert(DIM == mInitialiser->simDimension());
         mInitialiser->validate();
     }

    template <int DIM, typename FluxCalculatorImpl >
	Controller<DIM, FluxCalculatorImpl>::
	Controller(
		const std::shared_ptr<IBaseInitialiser> initialiser
		) :
		mDim(initialiser->simDimension()),
		mFinalTime(initialiser->finalTime()),
        mOutputResults(initialiser->outputResults())
	{
		const auto& simBoundaryConditions = initialiser->boundaryConditions();
		for (size_t i = 0; i < mBoundaryConditions.size(); ++i) {
			mBoundaryConditions[i] = simBoundaryConditions[i];
		}
		mInitialiser = initialiser;
	}

	 template <int DIM, typename FluxCalculatorImpl >
	void
	Controller<DIM, FluxCalculatorImpl>::
	initialiseGrid()
	{
        validateSimulation();

		auto resolution = mInitialiser->simResolution();
        auto dimensions = mInitialiser->simDomainMax();
		mGrid = std::make_shared<EulerianGrid>(resolution[0],
                                       resolution[1],
                                       resolution[2],
                                       FluxCalculatorImpl::RADIUS,
                                       Vector<3>(dimensions[0] / resolution[0],
                                                 dimensions[1] / resolution[1],
                                                 dimensions[2] / resolution[2]));
        mEos = mInitialiser->initialiseGrid(mGrid);
	}

	 template <int DIM, typename FluxCalculatorImpl >
    std::shared_ptr<EulerianGrid>
	Controller<DIM, FluxCalculatorImpl>::
	runSimulation()
	{
        {
            auto masterTimer = ScopedTimer("Total Sim Time");

            std::cout << "Initialising simulation...\n";
            {
                auto initialiserTimer = ScopedTimer("Sim Initialisation");
                initialiseGrid();
            }
            
            // Initial output
            double time = 0;
            int ts = 0;
            if (mOutputResults) {
                auto fileIOTimer = ScopedTimer("VTKWriter");
                VTKWriter::writeStructuredGridToFile(mGrid, time, ts);
            }
            
            // Run main simulation loop
            std::cout << "Running simulation...\n";
            int numOutput = 100;
            double outputTime = mFinalTime / numOutput;
            double nextOutputTime = outputTime;
            while (time < mFinalTime) {
                double dt = evolveTimeStep(time);
                time += dt;
                ++ts;
                std::cout << time << std::endl;
                
                if (time > nextOutputTime && mOutputResults) {
                    nextOutputTime += outputTime;
                    std::cout << "Outputing" << std::endl;
                    auto fileIOTimer = ScopedTimer("VTKWriter");
                    VTKWriter::writeStructuredGridToFile(mGrid, time, ts);
                }
            }
            
            // Final output
            if (mOutputResults) {
                auto fileIOTimer = ScopedTimer("VTKWriter");
                VTKWriter::writeStructuredGridToFile(mGrid, time, ts);
            }
            std::cout << "Simulation Completed Successfully!\n";
        }
        
        // Get timer statistics
        ScopedTimer::getTimerStatistics();

        return mGrid;
	}

	 template <int DIM, typename FluxCalculatorImpl >
    double
	Controller<DIM, FluxCalculatorImpl>::
	evolveTimeStep(
        const double& currentTime
        )
	{
		setBoundaryConditions();
		double deltaT = calculateTimeStep();
        if (deltaT + currentTime > mFinalTime) {
            deltaT = mFinalTime - currentTime;
        }
		evaluateFluxes();
		updateStates(deltaT);
		
		return deltaT;
	}

	 template <int DIM, typename FluxCalculatorImpl >
    double
	Controller<DIM, FluxCalculatorImpl>::
	calculateTimeStep()
	{
        auto timeStep = ScopedTimer("Time Step Calculator");
		return TimeStepCalculator::calculateTimeStep(DIM, mGrid, mEos[0]);
	}

	 template <int DIM, typename FluxCalculatorImpl >
    void
	Controller<DIM, FluxCalculatorImpl>::
	setBoundaryConditions()
	{
        auto boundariesStep = ScopedTimer("Boundary Conditions");
		BoundaryHandlerImpl<DIM>::applyBoundaries(mGrid, mBoundaryConditions);
	}

	 template <int DIM, typename FluxCalculatorImpl >
    void
	Controller<DIM, FluxCalculatorImpl>::
	evaluateFluxes()
	{
        auto fluxStep = ScopedTimer("Flux Calculator");
		FluxCalculatorImpl::evaluateFluxes(mGrid, mEos[0]);
	}

	 template <int DIM, typename FluxCalculatorImpl >
	void
	Controller<DIM, FluxCalculatorImpl>::
	updateStates(
		double deltaT
		)
	{
        auto updateStep = ScopedTimer("State Updater");
		CellUpdaterImpl<DIM>::updateStates(deltaT, mGrid, mEos[0]);
	}
	
	 template class Controller<1, FluxCalculatorGodunovImpl<1> >;
	 template class Controller<2, FluxCalculatorGodunovImpl<2> >;
	 template class Controller<3, FluxCalculatorGodunovImpl<3> >;
 }
