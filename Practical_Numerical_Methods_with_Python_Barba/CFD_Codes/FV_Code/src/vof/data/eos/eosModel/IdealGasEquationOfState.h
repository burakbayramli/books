/**
 * Author: Rohan Ramasamy
 * Date: 27/01/2017
 * 
 * The class for an ideal gas equation of state object
 */


#pragma once

#include <vof/data/eos/eosModel/IEquationOfState.h>
#include <vof/data/eos/eosProperties/IdealEquationOfStateProperties.h>


namespace vof
{
	struct RiemannState;

	class IdealGasEquationOfState :
	    public IEquationOfState
	{
	public:
		/**
		 * Constructor
		 */
		IdealGasEquationOfState(
			const double& gamma
			);

		/**
		 * Calculate density from pressure and internal energy
		 */
		virtual double
	 	getDensityFromPressureAndInternalEnergy(
	 		const double& pressure,
	 		const double& internalEnergy
	 		) const override;

	 	/**
	 	 * Calculate pressure from density and internal energy
	 	 */
	 	virtual double
	 	getPressureFromDensityAndInternalEnergy(
	 		const double& density,
	 		const double& internalEnergy
	 		) const override;

		/**
		 * Calculates the internal energy from density and pressure
		 */
	 	virtual double
	 	getInternalEnergyFromDensityAndPressure(
	 		const double& density,
	 		const double& pressure
	 		) const override;

	 	/**
	 	 * Calculate the mass flux - used in Riemann problems
	 	 */
	 	virtual double
	 	calculateMassFlux(
			const RiemannState& aheadState,
			const double& pressureStar
		    ) const override;

		/**
		 * Calculate the RiemannState within the rarefaction fan
		 */
		RiemannState
		calculateRarefactionState(
			const RiemannState& aheadState,
			const double& speed,
			const bool& right
			) const override;

		/**
		 * Calculate density in rarefaction
		 */
		 double
		 calculateDensityInRarefaction(
			const RiemannState& rarefactionState,
			const double& pressure
			) const override;
			
		/**
		 * Calculate density in shock
		 */
		 double
		 calculateDensityBehindShock(
			const RiemannState& aheadState,
			const double& pressure
			) const override;
			
	 private:
	 	IdealEquationOfStateProperties mEosProps;

	 	/**
	 	 * Calculate the sound speed for a given pressure and density
	 	 */
	 	double
	 	calculateSoundSpeed(
	 		const double& density,
	 		const double& pressure
	 		) const override;
	};
}
