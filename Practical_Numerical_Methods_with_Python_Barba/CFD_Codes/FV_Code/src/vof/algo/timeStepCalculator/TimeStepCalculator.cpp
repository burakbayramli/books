/**
 * Author: Rohan Ramasamy
 * Date: 07/02/2017
 */
 
 #include <math.h>
 #include <iostream>

 #include <vof/algo/timeStepCalculator/TimeStepCalculator.h>
 #include <vof/data/grid/EulerianGrid.h>
 
 
 namespace vof {
	double
	TimeStepCalculator::
	calculateTimeStep(
		const int& simDimension,
		const std::shared_ptr<EulerianGrid>& grid,
		const std::shared_ptr<IEquationOfState>& eos
		)
	{
		auto numX = grid->numX();
		auto numY = grid->numY();
		auto numZ = grid->numZ();
		
		// Get maximum velocities
        double maxVel = std::numeric_limits<double>::min();

        // Get State Arrays
        const StateGrid<double>& pressures = grid->pressures();
        const StateGrid<double>& densities = grid->densities();
        const StateGrid<std::array<double, 3> >& velocities = grid->velocities();
    
		for (int i = 0; i < numX; ++i) {
            for (int j = 0; j < numY; ++j) {
				for (int k = 0; k < numZ; ++k) {
					const auto rho = densities(i, j, k);
                    const auto pressure = pressures(i, j, k);
                    const auto vel = velocities(i, j, k);
                    
                    const auto velX = vel[0];
                    const auto velY = vel[1];
                    const auto velZ = vel[2];
                    
					double soundSpeed = eos->calculateSoundSpeed(rho, pressure);
					
					double totalVel = sqrt(velX * velX + velY * velY + velZ * velZ) + soundSpeed;
                    maxVel = std::max(maxVel, totalVel);
				}
			}
		}

		// Get minimum cell spacing
		auto cellDiagonal = grid->cellSize();
		double minSpacing = std::numeric_limits<double>::max();
		for (int dim = 0; dim < simDimension; ++dim) {
			minSpacing = std::min(minSpacing, cellDiagonal[dim]);
		}

		// Current the cfl condition is hard coded in
		double cfl = 0.4;
		return cfl * minSpacing / maxVel;
	}
 }
