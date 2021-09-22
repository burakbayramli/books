/**
 * Author: Rohan Ramasamy
 * Date: 27/01/2017
 * 
 * The class for an exact riemann solver
 */

#include <stdexcept>
#include <cmath>
#include <limits>
#include <iostream>

#include <vof/algo/riemann_solvers/ExactRiemannSolver.h>
#include <vof/data/eos/eosModel/IEquationOfState.h>


namespace vof
{
	RiemannMidState
	ExactRiemannSolver::
	calculateMidState(
	    RiemannInitialConditions initialConditions
	    )
	{
		auto leftState = initialConditions.mLeftState;
		auto rightState = initialConditions.mRightState;

		// If the states are approximately the same, return average of both
		if (std::abs(leftState.mPressure - rightState.mPressure) < TOL &&
			std::abs(leftState.mDensity - rightState.mDensity) < TOL &&
		    std::abs(leftState.mVelocity - rightState.mVelocity) < TOL) {
			return RiemannMidState((leftState.mPressure + rightState.mPressure) / 2.0,
								   (leftState.mVelocity + rightState.mVelocity) / 2.0);
		}

		// Guess p0
		double p0 = (leftState.mPressure + rightState.mPressure) / 2.0;

		// Get first guess based off Godunov's solution
		auto initialGuess = guessInitialMidState(p0, initialConditions);

		double averageDifference = std::numeric_limits<double>::max();
		int numIterations = 0;
		
		double pressureMinusOne = p0;
		double pressureCurrent = initialGuess.mPressureStar;
		double leftUStarCurrent = initialGuess.mVelocityStar;
		double rightUStarCurrent = initialGuess.mVelocityStar;
		double leftUStarPlusOne, rightUStarPlusOne, pressurePlusOne, wLeft, wRight;
		while (std::fabs(averageDifference) > TOL)
		{
			// Throw exception if max iterations exceeded
			if (numIterations > MAX_ITERATIONS) {
			    throw std::runtime_error("Maximum number of iterations exceeded");
			}

			// Calculate mass fluxes
			wLeft = leftState.mEos->calculateMassFlux(leftState, pressureCurrent);
			wRight = rightState.mEos->calculateMassFlux(rightState, pressureCurrent);

			// Calculate new pressures and velocities
			leftUStarPlusOne = initialConditions.mLeftState.mVelocity;
			leftUStarPlusOne -= wLeft;

			rightUStarPlusOne = initialConditions.mRightState.mVelocity;	
			rightUStarPlusOne += wRight;
			
			pressurePlusOne = -(rightUStarPlusOne - leftUStarPlusOne) * std::fabs(pressureCurrent - pressureMinusOne);
			pressurePlusOne /= std::fabs(leftUStarPlusOne - leftUStarCurrent) + std::fabs(rightUStarPlusOne - rightUStarCurrent);
			pressurePlusOne += pressureCurrent;
			pressurePlusOne = std::fmax(MIN_PRESSURE, pressurePlusOne);

			// Set new minus one values
			pressureMinusOne = pressureCurrent;
			pressureCurrent = pressurePlusOne;
			leftUStarCurrent = leftUStarPlusOne;
			rightUStarCurrent = rightUStarPlusOne;
			
			// Get new difference in velocity
			averageDifference = leftUStarCurrent - rightUStarCurrent;
			
			++numIterations;
		}

		return RiemannMidState(pressurePlusOne, (leftUStarPlusOne + rightUStarPlusOne) / 2.0);
	}

	RiemannState
	ExactRiemannSolver::
	sample(
	    RiemannInitialConditions initialConditions,
	    RiemannMidState midStates,
	    double sampleSpeed,
		bool& isLeft
	    )
	{
		double uStar = midStates.mVelocityStar;
		double pStar = midStates.mPressureStar;
		if (uStar < sampleSpeed) {
			// Right Star Condition
			isLeft = false;
			auto rightState = initialConditions.mRightState;
			if (rightState.mPressure > pStar) {
				// Rarefaction
				double rhoStar = rightState.mEos->calculateDensityInRarefaction(rightState, pStar);
				double highWaveSpeed = rightState.mVelocity + rightState.mEos->calculateSoundSpeed(rightState.mDensity, rightState.mPressure);
				double lowWaveSpeed = uStar + rightState.mEos->calculateSoundSpeed(rhoStar, pStar);
				if (highWaveSpeed <= sampleSpeed) {
					return rightState;
				}
				else {
					if (lowWaveSpeed >= sampleSpeed) {
						return RiemannState(rhoStar, pStar, uStar, rightState.mEos);
					}
					else {
						RiemannState midState(rhoStar, pStar, uStar, rightState.mEos);
						return rightState.mEos->calculateRarefactionState(rightState,
																   sampleSpeed,
																   true);
					}
				}
			}
			else {
				// Shock
				double rhoStar = rightState.mEos->calculateDensityBehindShock(rightState, pStar);
				double W = std::sqrt((pStar - rightState.mPressure) / (1.0 / rightState.mDensity - 1.0 / rhoStar));
				double shockSpeed = rightState.mVelocity + W / rightState.mDensity;
				if (shockSpeed <= sampleSpeed) {
					return rightState;
				}
				else {
					return RiemannState(rhoStar, pStar, uStar, rightState.mEos);
				}
			}
		}
		else {
			// Left Star Condition
			isLeft = true;
			auto leftState = initialConditions.mLeftState;
			if (leftState.mPressure >= pStar) {
				// Rarefaction
				double rhoStar = leftState.mEos->calculateDensityInRarefaction(leftState, pStar);
				double highWaveSpeed = leftState.mVelocity - leftState.mEos->calculateSoundSpeed(leftState.mDensity, leftState.mPressure);
				double lowWaveSpeed = uStar - leftState.mEos->calculateSoundSpeed(rhoStar, pStar);
				if (highWaveSpeed >= sampleSpeed) {
					return leftState;
				}
				else {
					if (lowWaveSpeed <= sampleSpeed) {
						return RiemannState(rhoStar, pStar, uStar, leftState.mEos);
					}
					else {
						RiemannState midState(rhoStar, pStar, uStar, leftState.mEos);
						return leftState.mEos->calculateRarefactionState(leftState,
																   sampleSpeed,
																   false);
					}
				}
			}
			else {
				// Shock
				double rhoStar = leftState.mEos->calculateDensityBehindShock(leftState, pStar);
				double W = std::sqrt((pStar - leftState.mPressure) / (1.0 / leftState.mDensity - 1.0 / rhoStar));
				double shockSpeed = leftState.mVelocity - W / leftState.mDensity;
				if (shockSpeed >= sampleSpeed) {
					return leftState;
				}
				else {
					return RiemannState(rhoStar, pStar, uStar, leftState.mEos);
				}
			}
		}
	}

	RiemannMidState
	ExactRiemannSolver::
	guessInitialMidState(
	    const double& pressureAverage,
		const RiemannInitialConditions& initialConditions
		)
	{
		// Get mass flux from pressure average
		double leftMassFlux = godunovMassFlux(pressureAverage, initialConditions.mLeftState);
		double rightMassFlux = godunovMassFlux(pressureAverage, initialConditions.mRightState);
		
		// Calculate pStar
		double leftPressure = initialConditions.mLeftState.mPressure;
		double rightPressure = initialConditions.mRightState.mPressure;
		double leftVelocity = initialConditions.mLeftState.mVelocity;
		double rightVelocity = initialConditions.mRightState.mVelocity;
		double pStarGuess = (leftVelocity + rightVelocity + rightPressure / rightMassFlux + leftPressure / leftMassFlux);
		pStarGuess /= (1.0 / rightMassFlux + 1.0 / leftMassFlux);
		pStarGuess = std::fmax(MIN_PRESSURE, pStarGuess);
	
		// Recalculate mass flux from pressure average
		leftMassFlux = godunovMassFlux(pStarGuess, initialConditions.mLeftState);
		rightMassFlux = godunovMassFlux(pStarGuess, initialConditions.mRightState);
			
		// Calculate uStar
		double uStarGuess = (leftPressure - rightPressure + rightMassFlux * rightVelocity + leftMassFlux * leftVelocity);
		uStarGuess /= rightMassFlux + leftMassFlux;
		
		// If there is a strong rarefaction, change the pressure guess
		bool pressureWithinRange = (pStarGuess > std::min(leftPressure, rightPressure)) && (pStarGuess < std::max(leftPressure, rightPressure));
		if (!pressureWithinRange && (leftVelocity - rightVelocity < 0.0)) {
			pStarGuess = 1e-2;
		}
		return RiemannMidState(pStarGuess, uStarGuess);
	}
	
	double
	ExactRiemannSolver::
	godunovMassFlux(
		 const double& pressureStar,
		 const RiemannState& aheadState
		 )
	{
		double pressureRatio = pressureStar / aheadState.mPressure;
		
		double phi;
		double gamma = 1.4;
		if (pressureRatio >= 1.0) {
			double gammaPlusOneOverTwo = (gamma + 1.0) / 2.0;
			double gammaMinusOneOverTwo = (gamma - 1.0) / 2.0;
			
			phi = std::sqrt(gammaPlusOneOverTwo * pressureRatio + gammaMinusOneOverTwo);
		}
		else {
		    phi = (gamma - 1.0) * (1.0 - pressureRatio);
		    phi /= 2 * std::sqrt(gamma) * (1 - std::pow(pressureRatio, (gamma - 1) / (2.0 * gamma)));
		}
		
		return std::sqrt(aheadState.mDensity * aheadState.mPressure) * phi;
	}
}
