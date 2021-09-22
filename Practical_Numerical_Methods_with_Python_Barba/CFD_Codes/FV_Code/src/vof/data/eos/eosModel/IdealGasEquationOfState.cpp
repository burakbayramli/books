/**
 * Author: Rohan Ramasamy
 * Date: 27/01/2017
 * 
 * The class for an ideal gas equation of state object
 */


#include <vof/data/eos/eosModel/IdealGasEquationOfState.h>
#include <vof/algo/riemann_solvers/ExactRiemannSolver.h>

#include <cmath>
#include <stdexcept>
#include <iostream>


namespace vof
{
	IdealGasEquationOfState::
	IdealGasEquationOfState(
		const double& gamma
		) :
	    mEosProps(gamma) {}

    double
	IdealGasEquationOfState::
	getDensityFromPressureAndInternalEnergy(
 		const double& pressure,
 		const double& internalEnergy
 		) const
	{
		return pressure / (internalEnergy * mEosProps.gammaMinusOne);
	}

 	double
 	IdealGasEquationOfState::
	getPressureFromDensityAndInternalEnergy(
 		const double& density,
 		const double& internalEnergy
 		) const
 	{
 		return density * internalEnergy * mEosProps.gammaMinusOne;
 	}

 	double
 	IdealGasEquationOfState::
	getInternalEnergyFromDensityAndPressure(
 		const double& density,
 		const double& pressure
 		) const
 	{
 		return pressure / (density * mEosProps.gammaMinusOne);
 	}

 	double
 	IdealGasEquationOfState::
	calculateMassFlux(
		const RiemannState& aheadState,
		const double& pressureStar
		) const
	{
		if (pressureStar > aheadState.mPressure) {
			double A = mEosProps.twoOverGammaPlusOne / aheadState.mDensity;
			double B = mEosProps.gammaMinusOneOverGammaPlusOne * aheadState.mPressure;
			
			double f = std::sqrt(A / (pressureStar + B));
			f *= pressureStar - aheadState.mPressure;
			
			return f;
		}
		else {
			double fAhead = calculateSoundSpeed(aheadState.mDensity, aheadState.mPressure) * mEosProps.twoOverGammaMinusOne;
			double densityStar = std::pow(pressureStar / aheadState.mPressure, mEosProps.oneOverGamma) * aheadState.mDensity;
			double fStar = calculateSoundSpeed(densityStar, pressureStar) * mEosProps.twoOverGammaMinusOne;
			
			return fStar - fAhead;
		}
	}
	
	RiemannState
	IdealGasEquationOfState::
	calculateRarefactionState(
		const RiemannState& aheadState,
		const double& speed,
		const bool& right
		) const
	{
		if (right) {
			double aheadSoundSpeed = calculateSoundSpeed(aheadState.mDensity, aheadState.mPressure);
			double rho = mEosProps.twoOverGammaPlusOne;
			rho -= (aheadState.mVelocity - speed) * mEosProps.gammaMinusOneOverGammaPlusOne / aheadSoundSpeed;
			rho = aheadState.mDensity * std::pow(rho, mEosProps.twoOverGammaMinusOne);
			
			double pressure = aheadState.mPressure * std::pow(rho / aheadState.mDensity, mEosProps.gamma);
			
			double velocity = aheadState.mVelocity - aheadSoundSpeed * mEosProps.twoOverGammaMinusOne;
			velocity += mEosProps.twoOverGammaMinusOne * calculateSoundSpeed(rho, pressure);
			
			return RiemannState(rho, pressure, velocity, aheadState.mEos);
		}
		else {
			double aheadSoundSpeed = calculateSoundSpeed(aheadState.mDensity, aheadState.mPressure);
			double rho = (aheadState.mVelocity - speed) * mEosProps.gammaMinusOneOverGammaPlusOne / aheadSoundSpeed;
			rho += mEosProps.twoOverGammaPlusOne;
			rho = aheadState.mDensity * std::pow(rho, mEosProps.twoOverGammaMinusOne);
			
			double pressure = aheadState.mPressure * std::pow(rho / aheadState.mDensity, mEosProps.gamma);
			
			double velocity = aheadState.mVelocity + aheadSoundSpeed * mEosProps.twoOverGammaMinusOne;
			velocity -= mEosProps.twoOverGammaMinusOne * calculateSoundSpeed(rho, pressure);
			
			return RiemannState(rho, pressure, velocity, aheadState.mEos);
		}
	}
	
	double
 	IdealGasEquationOfState::
	calculateDensityInRarefaction(
		const RiemannState& rarefactionState,
		const double& pressure
		) const
	{
		return rarefactionState.mDensity * std::pow(pressure / rarefactionState.mPressure, mEosProps.oneOverGamma);
	}

	double
 	IdealGasEquationOfState::
	calculateDensityBehindShock(
		const RiemannState& aheadState,
		const double& pressure
		) const
	{
		double rhoStar = pressure * mEosProps.gammaPlusOne + aheadState.mPressure * mEosProps.gammaMinusOne;
		rhoStar /= aheadState.mPressure * mEosProps.gammaPlusOne + pressure * mEosProps.gammaMinusOne;
		return rhoStar * aheadState.mDensity;
	}

 	double
 	IdealGasEquationOfState::
 	calculateSoundSpeed(
 		const double& density,
 		const double& pressure
 		) const
 	{
 		return std::sqrt(mEosProps.gamma * pressure / density);
 	}
}
