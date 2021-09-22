/**
 * Author: Rohan Ramasamy
 * Date: 27/01/2017
 * 
 * The class for an exact riemann solver
 */


#pragma once

#include <memory>


namespace vof
{
	class IEquationOfState;

	// Structures for defining the thermodynamic state in the system
	struct RiemannState
	{
		RiemannState(
			const double& density,
			const double& pressure,
			const double& velocity, 
			const std::shared_ptr<IEquationOfState>& eos
			) :
		    mDensity(density),
		    mPressure(pressure),
		    mVelocity(velocity),
		    mEos(eos) {}

		double mDensity;
		double mPressure;
		double mVelocity;
		std::shared_ptr<IEquationOfState> mEos;
	};

	struct RiemannInitialConditions
	{
		RiemannInitialConditions(
			RiemannState leftState,
			RiemannState rightState
			) :
		    mLeftState(leftState),
		    mRightState(rightState) {}

	    RiemannState mLeftState;
	    RiemannState mRightState;
	};

	struct RiemannMidState
	{
		RiemannMidState(
			const double& pressureStar,
			const double& velocityStar
			) :
		    mPressureStar(pressureStar),
		    mVelocityStar(velocityStar) {}

	    double mPressureStar;
	    double mVelocityStar;
	};

	// Actual Riemann Solver
	class ExactRiemannSolver
	{
	public:
		static const int MAX_ITERATIONS = 20;
		static constexpr double MIN_PRESSURE = 1e-6;
		static constexpr double TOL = 1e-6;

		/**
		 *	Calculate the mid state of the riemann problem
		 */ 
		static RiemannMidState
		calculateMidState(
			RiemannInitialConditions initialConditions
			);

		/**
		 * Sample riemann state travelling at the samples speed
		 */
		static RiemannState
		sample(
			RiemannInitialConditions initialConditions,
			RiemannMidState midStates,
			double sampleSpeed,
			bool& isLeft
			);

	private:
		/**
		 * Make an initial guess at the mid state using Godunov's method
		 */
		static RiemannMidState
		guessInitialMidState(
			const double& pressureAverage,
			const RiemannInitialConditions& initialConditions
			);
			
		/**
		 * Get mass flux by Godunov's method
		 */
		 static double
		 godunovMassFlux(
		     const double& pressureStar,
		     const RiemannState& aheadState
		     ); 
	};
}
