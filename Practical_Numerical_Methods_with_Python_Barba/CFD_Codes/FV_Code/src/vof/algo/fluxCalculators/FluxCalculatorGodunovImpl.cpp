/**
 * Author: Rohan Ramasamy
 * Date: 09/02/2017
 */
 
 #include <vof/algo/fluxCalculators/FluxCalculatorGodunovImpl.h>
 
 
 namespace vof {
    template<int DIM>
    const int FluxCalculatorGodunovImpl<DIM>::RADIUS = 1;

    template<int DIM>
	void
	FluxCalculatorGodunovImpl<DIM>::
	evaluateFluxes(
		const std::shared_ptr<EulerianGrid>& grid,
		const std::shared_ptr<IEquationOfState>& eos
		)
	{
        throw std::runtime_error("Not Implemented!");
        
        // Get conservative flux arrays
        auto& densityFluxes = grid->densityFluxes();
        auto& momentumFluxes = grid->momentumFluxes();
        auto& energyFluxes = grid->energyFluxes();
        
        // Get Riemann state arrays
        auto densities = grid->densities();
        auto pressures = grid->pressures();
        auto velocities = grid->velocities();
        
        // Get grid variables
        auto cellResolution = grid->cellResolution();
        auto numGhostCells = grid->numGhostCells();
        
        // Loop through all cells in the domain
        for (int i = -numGhostCells; i < cellResolution[0] + numGhostCells; ++i) {
            for (int j = -numGhostCells; j < cellResolution[1] + numGhostCells; ++j) {
                for (int k = -numGhostCells; k < cellResolution[2] + numGhostCells; ++k) {
                    std::array<int, 3> indices{i, j, k};
                    
                    for (int dim = 0; dim < DIM; ++dim) {
                        if (indices[dim] != cellResolution[dim] + numGhostCells - 1) {
                            // Get states for Riemann Problem
                            double leftRho = densities(i, j, k);
                            double leftP = pressures(i, j, k);
                            double leftU = velocities(i, j, k)[dim];
                            double rightRho = densities.getNeighbour(i, j, k, dim);
                            double rightP = pressures.getNeighbour(i, j, k, dim);
                            double rightU = velocities.getNeighbour(i, j, k, dim)[dim];
                            
                            // Solve and sample Riemann problem
                            RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
                                                                       RiemannState(rightRho, rightP, rightU, eos));
                            auto midState = ExactRiemannSolver::calculateMidState(initialConditions);
                            bool isLeft = false;
                            auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);
                            
                            // Calculate velocity components
                            std::array<double, 3> velocityComponents;
                            for (int velDim = 0; velDim < DIM; ++velDim) {
                                if (velDim == dim) {
                                    velocityComponents[dim] = sampleState.mVelocity;
                                }
                                else {
                                    velocityComponents[dim] = isLeft ? velocities(i, j, k)[dim] : velocities.getNeighbour(i, j, k, dim)[dim];
                                }
                            }
                            
                            // Calculate density flux
                            densityFluxes(i, j, k)[dim] = sampleState.mDensity * sampleState.mVelocity;
                            
                            // Calculate momentum fluxes
                            for (int momDim = 0; momDim < DIM; ++momDim) {
                                if (momDim == dim) {
                                    momentumFluxes(i, j, k)[dim][momDim] = sampleState.mDensity * sampleState.mVelocity * velocityComponents[momDim] + sampleState.mPressure;
                                }
                                else {
                                    momentumFluxes(i, j, k)[dim][momDim] = sampleState.mDensity * sampleState.mVelocity * velocityComponents[momDim];
                                }
                            }
                            
                            // Calculate energy flux
                            double kineticEnergy;
                            for (int kinDim = 0; kinDim < DIM; ++kinDim) {
                                kineticEnergy += 0.5 * velocityComponents[kinDim] * velocityComponents[kinDim];
                            }
                            double energyFlux = sampleState.mDensity * (kineticEnergy + eos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
                                                                                                                                     sampleState.mPressure));
                            energyFlux += sampleState.mPressure;
                            energyFluxes(i, j, k)[dim] = sampleState.mVelocity * energyFlux;
                        }
                    }
                }
            }
        }
        grid->setFluxes();
	}

    template<>
    void
	FluxCalculatorGodunovImpl<1>::
	evaluateFluxes(
		const std::shared_ptr<EulerianGrid> &grid,
		const std::shared_ptr<IEquationOfState> &eos
		)
	{
		auto& densityFluxes = grid->densityFluxes();
		auto& momentumFluxes = grid->momentumFluxes();
		auto& energyFluxes = grid->energyFluxes();

		auto densities = grid->densities();
		auto pressures = grid->pressures();
		auto velocities = grid->velocities();

		auto cellResolution = grid->cellResolution();
		auto numGhostCells = grid->numGhostCells();

		int j = 0;
		int k = 0;
		for (int i = -numGhostCells; i < cellResolution[0] + numGhostCells; ++i) {
			// Calculate flux in the x direction
			if (i != cellResolution[0] + numGhostCells - 1) {
				double leftRho = densities(i, j, k);
				double leftP = pressures(i, j, k);
				double leftU = velocities(i, j, k)[0];

				double rightRho = densities(i + 1, j, k);
				double rightP = pressures(i + 1, j, k);
				double rightU = velocities(i + 1, j, k)[0];

				RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
														   RiemannState(rightRho, rightP, rightU, eos));
				auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

				bool isLeft = false;
				auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);

				densityFluxes(i, j, k)[0] = sampleState.mDensity * sampleState.mVelocity;
				momentumFluxes(i, j, k)[0][0] = sampleState.mDensity * sampleState.mVelocity * sampleState.mVelocity + sampleState.mPressure;
				double kineticEnergy = 0.5 * sampleState.mVelocity * sampleState.mVelocity;
				double energyFlux = sampleState.mDensity * (kineticEnergy + eos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																														 sampleState.mPressure));
				energyFlux += sampleState.mPressure;
				energyFluxes(i, j, k)[0] = sampleState.mVelocity * energyFlux;
			}
		}
		grid->setFluxes();
	}

    template<>
	void
	FluxCalculatorGodunovImpl<2>::
	evaluateFluxes(
		const std::shared_ptr<EulerianGrid> &grid,
	    const std::shared_ptr<IEquationOfState> &eos
	    )
	{
#pragma omp parallel
{
        auto& densityFluxes = grid->densityFluxes();
        auto& momentumFluxes = grid->momentumFluxes();
        auto& energyFluxes = grid->energyFluxes();
        
        const auto& densities = grid->densities();
        const auto& pressures = grid->pressures();
        const auto& velocities = grid->velocities();
        
        auto cellResolution = grid->cellResolution();
        auto numGhostCells = grid->numGhostCells();
        
        int k = 0;
    
        const std::shared_ptr<EulerianGrid>& threadGrid = grid;
        const std::shared_ptr<IEquationOfState> threadEos = eos;
#pragma omp for schedule(static) collapse(2)
        for (int i = -numGhostCells; i < cellResolution[0] + numGhostCells; ++i) {
			for (int j = -numGhostCells; j < cellResolution[1] + numGhostCells; ++j) {

				// Calculate flux in the x direction
				if (i != cellResolution[0] + numGhostCells - 1) {
					double leftRho = densities(i, j, k);
					double leftP = pressures(i, j, k);
					double leftU = velocities(i, j, k)[0];

					double rightRho = densities(i + 1, j, k);
					double rightP = pressures(i + 1, j, k);
					double rightU = velocities(i + 1, j, k)[0];

					RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
															   RiemannState(rightRho, rightP, rightU, eos));
					auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

					bool isLeft = false;
					auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);
					double yVelocity = isLeft ? velocities(i, j, k)[1] : velocities(i + 1, j, k)[1];

					densityFluxes(i, j, k)[0] = sampleState.mDensity * sampleState.mVelocity;
					momentumFluxes(i, j, k)[0][0] = sampleState.mDensity * sampleState.mVelocity * sampleState.mVelocity + sampleState.mPressure;
					momentumFluxes(i, j, k)[0][1] = sampleState.mDensity * sampleState.mVelocity * yVelocity;
					double kineticEnergy = 0.5 * sampleState.mVelocity * sampleState.mVelocity;
					kineticEnergy += 0.5 * yVelocity * yVelocity;
					double energyFlux = sampleState.mDensity * (kineticEnergy + threadEos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																															 sampleState.mPressure));
					energyFlux += sampleState.mPressure;
					energyFluxes(i, j, k)[0] = sampleState.mVelocity * energyFlux;
				}

				// Calculate flux in the y direction
				if (j != cellResolution[1] + numGhostCells - 1) {
					double leftRho = densities(i, j, k);
					double leftP = pressures(i, j, k);
					double leftU = velocities(i, j, k)[1];

					double rightRho = densities(i, j + 1, k);
					double rightP = pressures(i, j + 1, k);
					double rightU = velocities(i, j + 1, k)[1];

					RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
															   RiemannState(rightRho, rightP, rightU, eos));
					auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

					bool isLeft = false;
					auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);
					double xVelocity = isLeft ? velocities(i, j, k)[0] : velocities(i, j + 1, k)[0];

					densityFluxes(i, j, k)[1] = sampleState.mDensity * sampleState.mVelocity;
					momentumFluxes(i, j, k)[1][0] = sampleState.mDensity * sampleState.mVelocity * xVelocity;
					momentumFluxes(i, j, k)[1][1] = sampleState.mDensity * sampleState.mVelocity * sampleState.mVelocity + sampleState.mPressure;
					double kineticEnergy = 0.5 * sampleState.mVelocity * sampleState.mVelocity;
					kineticEnergy += 0.5 * xVelocity * xVelocity;
					double energyFlux = sampleState.mDensity * (kineticEnergy + threadEos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																															 sampleState.mPressure));
					energyFlux += sampleState.mPressure;
					energyFluxes(i, j, k)[1] = sampleState.mVelocity * energyFlux;
				}
			}
		}
		threadGrid->setFluxes();
}
	}

    template<>
	void
	FluxCalculatorGodunovImpl<3>::
	evaluateFluxes(
		const std::shared_ptr<EulerianGrid>& grid,
		const std::shared_ptr<IEquationOfState>& eos
		)
	{
		auto& densityFluxes = grid->densityFluxes();
		auto& momentumFluxes = grid->momentumFluxes();
		auto& energyFluxes = grid->energyFluxes();
		
		auto densities = grid->densities();
		auto pressures = grid->pressures();
		auto velocities = grid->velocities();
		
		auto cellResolution = grid->cellResolution();
		auto numGhostCells = grid->numGhostCells();
		
		for (int i = -numGhostCells; i < cellResolution[0] + numGhostCells; ++i) {
			for (int j = -numGhostCells; j < cellResolution[1] + numGhostCells; ++j) {
				for (int k = -numGhostCells; k < cellResolution[2] + numGhostCells; ++k) {
            		// Calculate flux in the x direction
					if (i != cellResolution[0] + numGhostCells - 1) {
						double leftRho = densities(i, j, k);
						double leftP = pressures(i, j, k);
						double leftU = velocities(i, j, k)[0];

						double rightRho = densities(i + 1, j, k);
						double rightP = pressures(i + 1, j, k);
						double rightU = velocities(i + 1, j, k)[0];

						RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
																   RiemannState(rightRho, rightP, rightU, eos));
						auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

						bool isLeft = false;
						auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);
						double yVelocity = isLeft ? velocities(i, j, k)[1] : velocities(i + 1, j, k)[1];
						double zVelocity = isLeft ? velocities(i, j, k)[2] : velocities(i + 1, j, k)[2];

						densityFluxes(i, j, k)[0] = sampleState.mDensity * sampleState.mVelocity;
						momentumFluxes(i, j, k)[0][0] = sampleState.mDensity * sampleState.mVelocity * sampleState.mVelocity + sampleState.mPressure;
						momentumFluxes(i, j, k)[0][1] = sampleState.mDensity * sampleState.mVelocity * yVelocity;
						momentumFluxes(i, j, k)[0][2] = sampleState.mDensity * sampleState.mVelocity * zVelocity;
						double kineticEnergy = 0.5 * sampleState.mVelocity * sampleState.mVelocity;
						kineticEnergy += 0.5 * yVelocity * yVelocity;
						kineticEnergy += 0.5 * zVelocity * zVelocity;
						double energyFlux = sampleState.mDensity * (kineticEnergy + eos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																																 sampleState.mPressure));
						energyFlux += sampleState.mPressure;
						energyFluxes(i, j, k)[0] = sampleState.mVelocity * energyFlux;
					}

					// Calculate flux in the y direction
					if (j != cellResolution[1] + numGhostCells - 1) {
						double leftRho = densities(i, j, k);
						double leftP = pressures(i, j, k);
						double leftU = velocities(i, j, k)[1];

						double rightRho = densities(i, j + 1, k);
						double rightP = pressures(i, j + 1, k);
						double rightU = velocities(i, j + 1, k)[1];

						RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
																   RiemannState(rightRho, rightP, rightU, eos));
						auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

						bool isLeft = false;
						auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);
						double xVelocity = isLeft ? velocities(i, j, k)[0] : velocities(i, j + 1, k)[0];
						double zVelocity = isLeft ? velocities(i, j, k)[2] : velocities(i, j + 1, k)[2];

						densityFluxes(i, j, k)[1] = sampleState.mDensity * sampleState.mVelocity;
						momentumFluxes(i, j, k)[1][0] = sampleState.mDensity * sampleState.mVelocity * xVelocity;
						momentumFluxes(i, j, k)[1][1] = sampleState.mDensity * sampleState.mVelocity * sampleState.mVelocity + sampleState.mPressure;
						momentumFluxes(i, j, k)[1][2] = sampleState.mDensity * sampleState.mVelocity * zVelocity;
						double kineticEnergy = 0.5 * sampleState.mVelocity * sampleState.mVelocity;
						kineticEnergy += 0.5 * xVelocity * xVelocity;
						kineticEnergy += 0.5 * zVelocity * zVelocity;
						double energyFlux = sampleState.mDensity * (kineticEnergy + eos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																																 sampleState.mPressure));
						energyFlux += sampleState.mPressure;
						energyFluxes(i, j, k)[1] = sampleState.mVelocity * energyFlux;
					}

					// Calculate flux in z direction
					if (k != cellResolution[2] + numGhostCells - 1) {
						double leftRho = densities(i, j, k);
						double leftP = pressures(i, j, k);
						double leftU = velocities(i, j, k)[2];

						double rightRho = densities(i, j, k + 1);
						double rightP = pressures(i, j, k + 1);
						double rightU = velocities(i, j, k + 1)[2];

						RiemannInitialConditions initialConditions(RiemannState(leftRho, leftP, leftU, eos),
																   RiemannState(rightRho, rightP, rightU, eos));
						auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

						bool isLeft = false;
						auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, 0.0, isLeft);
						double xVelocity = isLeft ? velocities(i, j, k)[0] : velocities(i, j, k + 1)[0];
						double yVelocity = isLeft ? velocities(i, j, k)[1] : velocities(i, j, k + 1)[1];

						densityFluxes(i, j, k)[2] = sampleState.mDensity * sampleState.mVelocity;
						momentumFluxes(i, j, k)[2][0] = sampleState.mDensity * sampleState.mVelocity * xVelocity;
						momentumFluxes(i, j, k)[2][1] = sampleState.mDensity * sampleState.mVelocity * yVelocity;
						momentumFluxes(i, j, k)[2][2] = sampleState.mDensity * sampleState.mVelocity * sampleState.mVelocity + sampleState.mPressure;
						double kineticEnergy = sampleState.mVelocity * sampleState.mVelocity;
						kineticEnergy += xVelocity * xVelocity;
						kineticEnergy += yVelocity * yVelocity;
						kineticEnergy *= 0.5;
						double energyFlux = sampleState.mDensity * (kineticEnergy + eos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																																 sampleState.mPressure));
						energyFlux += sampleState.mPressure;
						energyFluxes(i, j, k)[2] = sampleState.mVelocity * energyFlux;
					}
				}
			}
		}
		grid->setFluxes();
	}

    template class FluxCalculatorGodunovImpl<1>;
    template class FluxCalculatorGodunovImpl<2>;
    template class FluxCalculatorGodunovImpl<3>;
 }
