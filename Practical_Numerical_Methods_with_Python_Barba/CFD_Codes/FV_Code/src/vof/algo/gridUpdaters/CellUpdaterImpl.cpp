//
// Created by Rohan Ramasamy on 28/02/2017.
//

#include <vof/algo/gridUpdaters/CellUpdaterImpl.h>


namespace vof {
    template<int DIM>
    void
    CellUpdaterImpl<DIM>::
    updateStates(
        const double& deltaT,
        const std::shared_ptr<EulerianGrid>& grid,
        const std::shared_ptr<IEquationOfState>& eos
        )
    {
        throw std::runtime_error("Not implemented!");
    }

    template<>
    void
    CellUpdaterImpl<1>::
    updateStates(
        const double &deltaT,
        const std::shared_ptr<EulerianGrid>& grid,
        const std::shared_ptr<IEquationOfState> &eos
        )
    {
        if (!grid->fluxesSet()) {
            throw std::runtime_error("Fluxes are not set!");
        }
        // Get state and flux arrays
        const auto& densityFluxes = grid->densityFluxes();
        const auto& momentumFluxes = grid->momentumFluxes();
        const auto& energyFluxes = grid->energyFluxes();
        const auto& densities = grid->densities();
        const auto& velocities = grid->velocities();
        const auto& internalEnergies = grid->internalEnergies();

        const auto& resolution = grid->cellResolution();
        const auto& cellDiagonal = grid->cellSize();
        double dTOverDx = deltaT / cellDiagonal[0];
        int k = 0;
        int j = 0;
        for (int i = 0; i < resolution[0]; ++i) {
            // Calculate Fluxes
            double densityFlux = (densityFluxes(i - 1, j, k)[0] - densityFluxes(i, j, k)[0]) * dTOverDx;
            double xMomentumFlux = (momentumFluxes(i - 1, j, k)[0][0] - momentumFluxes(i, j, k)[0][0]) * dTOverDx;
            double yMomentumFlux = (momentumFluxes(i - 1, j, k)[0][1] - momentumFluxes(i, j, k)[0][1]) * dTOverDx;
            double energyFlux = (energyFluxes(i - 1, j, k)[0] - energyFluxes(i, j, k)[0]) * dTOverDx;

            // Get new Conservative States
            double newDensity = densities(i, j, k) + densityFlux;
            double newU = densities(i, j, k) * velocities(i, j, k)[0] + xMomentumFlux;
            newU /= newDensity;

            double newTotalEnergy = internalEnergies(i, j, k);
            newTotalEnergy += 0.5 * velocities(i, j, k)[0] * velocities(i, j, k)[0];
            newTotalEnergy *= densities(i, j, k);
            newTotalEnergy += energyFlux;
            double newKineticEnergy = 0.5 * newDensity * (newU * newU);
            double newInternalEnergy = newTotalEnergy - newKineticEnergy;
            newInternalEnergy /= newDensity;

            double newPressure = eos->getPressureFromDensityAndInternalEnergy(newDensity, newInternalEnergy);

            grid->setIdx({{i, j, k}},
                         newDensity,
                         newPressure,
                         newInternalEnergy,
                         {{newU, 0.0, 0.0}});
        }
        grid->setFluxes();
    }

    template<>
    void
    CellUpdaterImpl<2>::
    updateStates(
        const double &deltaT,
        const std::shared_ptr<EulerianGrid>& grid,
        const std::shared_ptr<IEquationOfState> &eos
        )
    {
        if (!grid->fluxesSet()) {
            throw std::runtime_error("Fluxes are not set!");
        }

        // Get state and flux arrays
        const auto& densityFluxes = grid->densityFluxes();
        const auto& momentumFluxes = grid->momentumFluxes();
        const auto& energyFluxes = grid->energyFluxes();
        const auto& densities = grid->densities();
        const auto& velocities = grid->velocities();
        const auto& internalEnergies = grid->internalEnergies();
    
        const auto& resolution = grid->cellResolution();
        const auto& cellDiagonal = grid->cellSize();
        const double dTOverDx = deltaT / cellDiagonal[0];
        const double dTOverDy = deltaT / cellDiagonal[1];
        int k = 0;
    
        for (int i = 0; i < resolution[0]; ++i) {
            for (int j = 0; j < resolution[1]; ++j) {
                // Calculate Fluxes
                double densityFlux = (densityFluxes(i - 1, j, k)[0] - densityFluxes(i, j, k)[0]) * dTOverDx;
                densityFlux += (densityFluxes(i, j - 1, k)[1] - densityFluxes(i, j, k)[1]) * dTOverDy;

                double xMomentumFlux = (momentumFluxes(i - 1, j, k)[0][0] - momentumFluxes(i, j, k)[0][0]) * dTOverDx;
                xMomentumFlux += (momentumFluxes(i, j - 1, k)[1][0] - momentumFluxes(i, j, k)[1][0]) * dTOverDy;

                double yMomentumFlux = (momentumFluxes(i - 1, j, k)[0][1] - momentumFluxes(i, j, k)[0][1]) * dTOverDx;
                yMomentumFlux += (momentumFluxes(i, j - 1, k)[1][1] - momentumFluxes(i, j, k)[1][1]) * dTOverDy;

                double energyFlux = (energyFluxes(i - 1, j, k)[0] - energyFluxes(i, j, k)[0]) * dTOverDx;
                energyFlux += (energyFluxes(i, j - 1, k)[1] - energyFluxes(i, j, k)[1]) * dTOverDy;

                // Get new Conservative States
                double newDensity = densities(i, j, k) + densityFlux;
                double newU = densities(i, j, k) * velocities(i, j, k)[0] + xMomentumFlux;
                newU /= newDensity;
                double newV = densities(i, j, k) * velocities(i, j, k)[1] + yMomentumFlux;
                newV /= newDensity;

                double newTotalEnergy = internalEnergies(i, j, k);
                newTotalEnergy += 0.5 * velocities(i, j, k)[0] * velocities(i, j, k)[0];
                newTotalEnergy += 0.5 * velocities(i, j, k)[1] * velocities(i, j, k)[1];
                newTotalEnergy *= densities(i, j, k);
                newTotalEnergy += energyFlux;
                double newKineticEnergy = 0.5 * newDensity * (newU * newU + newV * newV);
                double newInternalEnergy = newTotalEnergy - newKineticEnergy;
                newInternalEnergy /= newDensity;

                double newPressure = eos->getPressureFromDensityAndInternalEnergy(newDensity, newInternalEnergy);

                {
                    grid->setIdx({{i, j, k}},
                                 newDensity,
                                 newPressure,
                                 newInternalEnergy,
                                 {{newU, newV, 0.0}});
                }
            }
        }
        grid->setFluxes();
    }

    template<>
    void
    CellUpdaterImpl<3>::
    updateStates(
        const double& deltaT,
        const std::shared_ptr<EulerianGrid>& grid,
        const std::shared_ptr<IEquationOfState>& eos
        )
    {
        if (!grid->fluxesSet()) {
            throw std::runtime_error("Fluxes are not set!");
        }
        // Get state and flux arrays
        const auto& densityFluxes = grid->densityFluxes();
        const auto& momentumFluxes = grid->momentumFluxes();
        const auto& energyFluxes = grid->energyFluxes();
        const auto& densities = grid->densities();
        const auto& velocities = grid->velocities();
        const auto& internalEnergies = grid->internalEnergies();

        const auto& resolution = grid->cellResolution();
        const auto& cellDiagonal = grid->cellSize();
        double dTOverDx = deltaT / cellDiagonal[0];
        double dTOverDy = deltaT / cellDiagonal[1];
        double dTOverDz = deltaT / cellDiagonal[2];
        for (int i = 0; i < resolution[0]; ++i) {
            for (int j = 0; j < resolution[1]; ++j) {
                for (int k = 0; k < resolution[2]; ++k) {
                    // Calculate Fluxes
                    double densityFlux = (densityFluxes(i - 1, j, k)[0] - densityFluxes(i, j, k)[0]) * dTOverDx;
                    densityFlux += (densityFluxes(i, j - 1, k)[1] - densityFluxes(i, j, k)[1]) * dTOverDy;
					densityFlux += (densityFluxes(i, j, k - 1)[2] - densityFluxes(i, j, k)[2]) * dTOverDz;

                    double xMomentumFlux = (momentumFluxes(i - 1, j, k)[0][0] - momentumFluxes(i, j, k)[0][0]) * dTOverDx;
                    xMomentumFlux += (momentumFluxes(i, j - 1, k)[1][0] - momentumFluxes(i, j, k)[1][0]) * dTOverDy;
					xMomentumFlux += (momentumFluxes(i, j, k - 1)[2][0] - momentumFluxes(i, j, k)[2][0]) * dTOverDz;

                    double yMomentumFlux = (momentumFluxes(i - 1, j, k)[0][1] - momentumFluxes(i, j, k)[0][1]) * dTOverDx;
                    yMomentumFlux += (momentumFluxes(i, j - 1, k)[1][1] - momentumFluxes(i, j, k)[1][1]) * dTOverDy;
					yMomentumFlux += (momentumFluxes(i, j, k - 1)[2][1] - momentumFluxes(i, j, k)[2][1]) * dTOverDz;

					double zMomentumFlux = (momentumFluxes(i - 1, j, k)[0][2] - momentumFluxes(i, j, k)[0][2]) * dTOverDx;
					zMomentumFlux += (momentumFluxes(i, j - 1, k)[1][2] - momentumFluxes(i, j, k)[1][2]) * dTOverDy;
					zMomentumFlux += (momentumFluxes(i, j, k - 1)[2][2] - momentumFluxes(i, j, k)[2][2]) * dTOverDz;

                    double energyFlux = (energyFluxes(i - 1, j, k)[0] - energyFluxes(i, j, k)[0]) * dTOverDx;
                    energyFlux += (energyFluxes(i, j - 1, k)[1] - energyFluxes(i, j, k)[1]) * dTOverDy;
					energyFlux += (energyFluxes(i, j, k - 1)[2] - energyFluxes(i, j, k)[2]) * dTOverDz;

                    // Get new Conservative States
                    double newDensity = densities(i, j, k) + densityFlux;
                    double newU = densities(i, j, k) * velocities(i, j, k)[0] + xMomentumFlux;
                    newU /= newDensity;
                    double newV = densities(i, j, k) * velocities(i, j, k)[1] + yMomentumFlux;
                    newV /= newDensity;
					double newW = densities(i, j, k) * velocities(i, j, k)[2] + zMomentumFlux;
					newW /= newDensity;

                    double newTotalEnergy = internalEnergies(i, j, k);
                    newTotalEnergy += 0.5 * velocities(i, j, k)[0] * velocities(i, j, k)[0];
                    newTotalEnergy += 0.5 * velocities(i, j, k)[1] * velocities(i, j, k)[1];
                    newTotalEnergy += 0.5 * velocities(i, j, k)[2] * velocities(i, j, k)[2];
                    newTotalEnergy *= densities(i, j, k);
                    newTotalEnergy += energyFlux;
					double newKineticEnergy = 0.5 * newDensity * (newU * newU + newV * newV + newW * newW);
                    double newInternalEnergy = newTotalEnergy - newKineticEnergy;
                    newInternalEnergy /= newDensity;

                    double newPressure = eos->getPressureFromDensityAndInternalEnergy(newDensity, newInternalEnergy);

                    grid->setIdx({{i, j, k}},
                                 newDensity,
                                 newPressure,
                                 newInternalEnergy,
                                 {{newU, newV, newW}});
                }
            }
        }
        grid->setFluxes();
    }

    template class CellUpdaterImpl<1>;
    template class CellUpdaterImpl<2>;
    template class CellUpdaterImpl<3>;
}
