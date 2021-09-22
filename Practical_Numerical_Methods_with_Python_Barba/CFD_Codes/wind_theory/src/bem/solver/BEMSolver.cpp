/**
 *  Author: Rohan Ramasamy
 *  Date: 27/12/16
 *
 *  This file contains a class for solving the blade element momentum equations to approximate
 * the performance of a wind turbine.
 */

 #include "BEMSolver.h"

 #include <math.h>
 #include <iostream>
 #include <cassert>


 namespace bem {
 	BEMSolver::
 	BEMSolver(
 			int numBlades,
 			double ratedPower,
 			double ratedAngularRotation,
 			double ratedWindSpeed,
 			AirfoilInterpolator airfoil,
 			int numPts
 			) :
 			mInitialised(false),
 			mNumBlades(numBlades),
 			mPRated(ratedPower),
 			mWRated(ratedAngularRotation),
 			mURated(ratedWindSpeed),
 			mSolidity(0.0),
 			mAirfoil(airfoil)
	{
		mChord = std::vector<double>(numPts, 0.0);
		mInherantTwist = std::vector<double>(numPts, 0.0);
		mLocalSolidity = std::vector<double>(numPts, 0.0);
		mAlpha = mAirfoil.getIdealAngleOfAttack();
	}

	void
	BEMSolver::
	initialise()
	{
		// If already initialised, skip
		if (mInitialised) {
			return;
		}

		// Assumed values for approximation
		double assumedPowerCoefficient = 0.4;
		double assumedLiftCoefficient = 1.0;
		double airDensity = 1.225;

		// Calculate radius and tip speed ratio
		mRadius = sqrt((2 * mPRated) / (assumedPowerCoefficient * airDensity * M_PI * mURated * mURated));
		double tsr = mWRated * mRadius / mURated;
		mRatedCondition.first = tsr;

		// Set chord shape using ideal wake theory
		for (size_t i = 0; i < mChord.size(); ++i) {
			double chordFraction = (static_cast<double>(i) + 0.5) / mChord.size();
			double localTSR = tsr * chordFraction;

			mInherantTwist[i] = 2.0 / 3.0 * atan(1.0 / localTSR);
			mChord[i] = 8.0 * M_PI * chordFraction * mRadius;
			mChord[i] /= (mNumBlades * assumedLiftCoefficient);
			mChord[i] *= (1 - cos(mInherantTwist[i]));

			mLocalSolidity[i] = mNumBlades * mChord[i] / (2 * M_PI * chordFraction * mRadius);
			mSolidity += mNumBlades * mChord[i] / (mRadius * mChord.size() * M_PI);
		}
		// Set blade to be initialised
		mInitialised = true;
	}

	std::pair<double, double>
	BEMSolver::
	findRatedCondition()
	{
		double powerCoeff = findPowerCoefficient(0.0, mURated, true);

		std::cout << powerCoeff << std::endl;

		mRatedCondition.second = powerCoeff;
	}

	double
	BEMSolver::
	findPowerCoefficient(
		double activePitch,
		double windSpeed,
		bool isRatedCondition
		)
	{
		if (!mInitialised) {
			throw std::runtime_error("BEM Solver is uninitialised!");
		}

		activePitch = activePitch / 180.0 * M_PI;

		// Initialise lambda functions for axial induction factors
		auto getAxialFactor = [](double tipLoss, double solidity, double liftCoeff, double twist) {
			double denominator = 4.0 * tipLoss * pow(sin(twist), 2);
			denominator /= solidity * liftCoeff * cos(twist);
			denominator += 1.0;

			return 1.0 / denominator;
		};

		auto getAngularFactor = [](double tipLoss, double solidity, double liftCoeff, double twist) {
			double denominator = 4.0 * tipLoss * cos(twist);
			denominator /= (solidity * liftCoeff);
			denominator -= 1.0;

			return 1.0 / denominator;
		};

		auto getTwist = [](double axialFactor, double angularFactor, double lambda) {
			return atan((1.0 - axialFactor) / ((1 + angularFactor) * lambda));
		};

		auto getTipLoss = [](double radiusRatio, double twist, int numBlades) {
			double exponent = numBlades / 2.0 * (1 - radiusRatio);
			exponent /= radiusRatio * sin(twist);

			return 2.0 / M_PI * acos(exp(-exponent));
		};

		auto getThrustCoefficient = [](double solidity, double lift, double drag, double axialFactor, double twist) {
			double numerator = solidity * pow(1 - axialFactor, 2.0);
			numerator *= lift * cos(twist) + drag * sin(twist);

			return numerator / pow(sin(twist), 2.0);
		};

		// Initialise induction factors
		double designLift = mAirfoil.getLiftCoefficient(mAlpha);
		std::vector<double> axialFactors(mChord.size(), 0.0);
		std::vector<double> angularFactors(mChord.size(), 0.0);
		std::vector<double> radii(mChord.size(), 0.0);
		std::vector<double> lambdas(mChord.size(), 0.0);
		std::vector<double> twists(mChord.size(), 0.0);
		for (size_t i = 0; i < axialFactors.size(); ++i) {
			double localRadius = (static_cast<double>(i) + 0.5) / mChord.size() * mRadius;
			radii[i] = localRadius;
			lambdas[i] = localRadius * mWRated / windSpeed;
			twists[i] = mInherantTwist[i];

			axialFactors[i] = getAxialFactor(1.0, mLocalSolidity[i], designLift, twists[i]);
			angularFactors[i] = (1.0 - 3.0 * axialFactors[i]) / (4.0 * axialFactors[i] - 1.0);
		}

		// Loop through induction factor iteration
		double axialDiff = 1.0;
		double angularDiff = 1.0;
		double TOL = 1e-4;
		for (size_t i = 0; i < axialFactors.size(); ++i) {
			double liftCoefficient = mAirfoil.getLiftCoefficient(mAlpha);
			double dragCoefficient = mAirfoil.getDragCoefficient(mAlpha);
			double alpha = mAlpha / 180.0 * M_PI;
			double tipLoss = isRatedCondition ? 1.0 : getTipLoss(radii[i] / mRadius, twists[i], mNumBlades);
			int numIterations = 0;
			while (axialDiff > TOL && angularDiff > TOL) {
				if (numIterations > 1000) {
					throw std::runtime_error("Maximum number of iterations reached!");
				}

				// Get new induction factors
				double prevAxialFactor = axialFactors[i];
				double prevAngularFactor = angularFactors[i];
				
				double thrustCoeff = getThrustCoefficient(mLocalSolidity[i], liftCoefficient, dragCoefficient,
				                                          axialFactors[i], twists[i]);
				if (thrustCoeff < 0.96) {
					axialFactors[i] = getAxialFactor(tipLoss, mLocalSolidity[i], 
						                             liftCoefficient, twists[i]);
				}
				else {
					axialFactors[i] = (1.0 / tipLoss);
					axialFactors[i] *= (0.143 + sqrt(0.0203 - 0.6427 * (0.889 - thrustCoeff)));
				}
				angularFactors[i] = getAngularFactor(tipLoss, mLocalSolidity[i], 
					                                 liftCoefficient, twists[i]);

				// Calculate new twist, coefficients and tip loss
				twists[i] = getTwist(axialFactors[i], angularFactors[i], lambdas[i]);
				
				if (!isRatedCondition) {
					alpha = twists[i] - mInherantTwist[i] + mAlpha / 180.0 * M_PI - activePitch;
				}
				liftCoefficient = mAirfoil.getLiftCoefficient(alpha / M_PI * 180.0);
				dragCoefficient = mAirfoil.getDragCoefficient(alpha / M_PI * 180.0);
				tipLoss = getTipLoss(radii[i] / mRadius, twists[i], mNumBlades);

				axialDiff = fabs(axialFactors[i] - prevAxialFactor);
				angularDiff = fabs(angularFactors[i] - prevAngularFactor);

				++numIterations;
			}

			if (isRatedCondition) {
				mInherantTwist[i] = twists[i];
			}

			axialDiff = 1.0;
			angularDiff = 1.0;
		}
		
		// Calculate Cp
		double powerCoeff = 0.0;
		for (size_t i = 0; i < mChord.size(); ++i) {
			double tipLoss = getTipLoss(radii[i] / mRadius, twists[i], mNumBlades);
			double twist = twists[i];
			double alpha = twists[i] - mInherantTwist[i] + mAlpha / 180.0 * M_PI - activePitch;
			double liftCoefficient = mAirfoil.getLiftCoefficient(alpha / M_PI * 180.0);
			double dragCoefficient = mAirfoil.getDragCoefficient(alpha / M_PI * 180.0);

			double deltaCp = (cos(twist) - lambdas[i] * sin(twist)) * (sin(twist) + lambdas[i] * cos(twist));
			deltaCp *= tipLoss * pow(sin(twist), 2.0) * pow(lambdas[i], 2.0);
			deltaCp *= 1 - dragCoefficient / (liftCoefficient * tan(twist));

			powerCoeff += deltaCp;
		}
		powerCoeff *= 8 / (mChord.size() * mRatedCondition.first);

		return powerCoeff;
	}

	std::vector<double>
	BEMSolver::
	calculatePowerCurve(
		std::vector<double> windSpeeds,
		double pitchTwist
		)
	{
		std::vector<double> powerCoefficients(windSpeeds.size(), 0.0);
		std::vector<double> tipSpeedRatios(windSpeeds.size(), 0.0);

		for (size_t i = 0; i < powerCoefficients.size(); ++i) {
			tipSpeedRatios[i] = mWRated * mRadius / windSpeeds[i];
			powerCoefficients[i] = findPowerCoefficient(pitchTwist, windSpeeds[i]);
			
			std::cout << tipSpeedRatios[i] << " " << powerCoefficients[i] << std::endl;
		}

		return powerCoefficients;
	}
 }