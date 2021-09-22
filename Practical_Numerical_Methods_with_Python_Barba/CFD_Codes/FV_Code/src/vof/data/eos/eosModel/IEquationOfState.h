/**
 * Author: Rohan Ramasamy
 * Date: 27/01/2017
 * 
 * The interface class for an equation of state object
 */


#pragma once

#include <vof/algo/riemann_solvers/ExactRiemannSolver.h>


namespace vof
{
	class IEquationOfState
	{
	public:
		/**
		 * Calculate density from pressure and internal energy
		 */
		virtual double
		getDensityFromPressureAndInternalEnergy(
			const double& pressure,
			const double& internalEnergy
			) const = 0;

		/**
	 	 * Calculate pressure from density and internal energy
	 	 */
		virtual double
		getPressureFromDensityAndInternalEnergy(
			const double& density,
			const double& internalEnergy
			) const = 0;

		/**
		 * Calculates the internal energy from density and pressure
		 */
		virtual double
		getInternalEnergyFromDensityAndPressure(
			const double& density,
			const double& pressure
			) const = 0;

		/**
	 	 * Calculate the mass flux - used in Riemann problems
	 	 */
		virtual double
		calculateMassFlux(
			const RiemannState& aheadState,
			const double& pressureStar
		    ) const = 0;
		    
		/**
		 * Calculate the RiemannState within the rarefaction fan
		 */
		virtual RiemannState
		calculateRarefactionState(
			const RiemannState& aheadState,
			const double& speed,
			const bool& right
			) const = 0;
		    
		/**
		 * Calculate the sound speed
		 */
		 virtual double
		 calculateSoundSpeed(
			const double& density,
			const double& pressure
			) const = 0;
			
		/**
		 * Calculate density in rarefaction
		 */
		 virtual double
		 calculateDensityInRarefaction(
			const RiemannState& rarefactionState,
			const double& pressure
			) const = 0;
			
		/**
		 * Calculate density in shock
		 */
		 virtual double
		 calculateDensityBehindShock(
			const RiemannState& aheadState,
			const double& pressure
			) const = 0;
	};
}
