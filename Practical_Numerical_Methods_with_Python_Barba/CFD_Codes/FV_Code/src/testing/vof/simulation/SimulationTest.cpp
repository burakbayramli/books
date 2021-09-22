/**
 * Author: Rohan Ramasamy
 * Date: 24/02/2017
 *
 * The test class for simulations
 */


#include <fstream>
#include <chrono>

#include <vof/algo/fluxCalculators/FluxCalculatorGodunovImpl.h>

#include <testing/vof/simulation/SimulationTest.h>


namespace vof {

template<int DIM>
void
SimulationTest<DIM>::
runAndCompareSim(
    const std::shared_ptr<IBaseInitialiser> initialiser,
    const std::string& fName,
    const int& simTime
    )
{
    // Record initial time
    auto start = std::chrono::steady_clock::now();
    // Run sim
    Controller<DIM, FluxCalculatorGodunovImpl<DIM> > controller(initialiser);
    std::shared_ptr<EulerianGrid> finalGrid = controller.runSimulation();
    // Record and check final time is within limiting final time for simulation
    auto end = std::chrono::steady_clock::now();
    EXPECT_GT(simTime, std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count());

    // Compare result with file output
    std::string filePath = "../src/testing/vof/simulation/archives/" + fName;
    std::ifstream input(filePath, std::ios::in);
    int iteration = 0;
    const auto& densities = finalGrid->densities();
    const auto& pressures = finalGrid->pressures();
    const auto& velocities = finalGrid->velocities();
    const auto& internalEnergies = finalGrid->internalEnergies();
    int i, j, k;
    double archiveX, archiveY, archiveZ, archiveDensity, archivePressure, archiveVelocityX, archiveVelocityY, archiveVelocityZ, archiveInternalEnergy;
    while (!input.eof()) {
        input >> i >> j >> k >> archiveX >> archiveY >> archiveZ >> archiveDensity >> archivePressure
              >> archiveVelocityX >> archiveVelocityY  >> archiveVelocityZ >> archiveInternalEnergy;

        EXPECT_NEAR(densities(i, j, k) - archiveDensity, 0.0, TOL);
        EXPECT_NEAR(pressures(i, j, k) - archivePressure, 0.0, TOL);
        EXPECT_NEAR(velocities(i, j, k)[0] - archiveVelocityX, 0.0, TOL);
        EXPECT_NEAR(velocities(i, j, k)[1] - archiveVelocityY, 0.0, TOL);
        EXPECT_NEAR(velocities(i, j, k)[2] - archiveVelocityZ, 0.0, TOL);
        EXPECT_NEAR(internalEnergies(i, j, k) - archiveInternalEnergy, 0.0, TOL) << archiveInternalEnergy << " " << internalEnergies(i, j, k);
        ++iteration;
    }
}

template class SimulationTest<1>;
template class SimulationTest<2>;
template class SimulationTest<3>;
}