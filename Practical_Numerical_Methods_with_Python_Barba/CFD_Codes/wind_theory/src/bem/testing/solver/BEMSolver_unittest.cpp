/*
Author: Rohan Ramasamy
Data: 26/12/16

This file contains unit tests for AirfoilInterpolator
*/

// Linked Includes
#include "gtest/gtest.h"

#include <vector>
#include <iostream>
#include <math.h>

#include "../../solver/BEMSolver.h"


namespace bem 
{
	
	class BEMSolverTest : public ::testing::Test {
	public:
		virtual void SetUp() {}

		virtual void TearDown() {}
	};

	TEST_F(BEMSolverTest, idealChordDistributionWithWakeRotationTwelveBlades) {
		// Values to make tip speed ratio = 1
		int numBlades = 12;
		double wRated = 1.0;
		double uRated = 1.0;
		int numPts = 10;
		double pRated =  0.4 * 1.225 * M_PI / 2.0;

		// Dummy interpolator
		std::vector<double> alphas = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 14.0, 16.0 };
		std::vector<double> liftCoefficients = { 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.1, 1.0, 1.0, 1.0, 1.0 };
		std::vector<double> dragCoefficients = { 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.035, 0.05, 0.06, 0.08, 0.11 };
		AirfoilInterpolator interpolator(alphas, dragCoefficients, liftCoefficients);

		BEMSolver solver(numBlades, pRated, wRated, uRated, interpolator, numPts);
		solver.initialise();

		auto chord = solver.chordDistribution();
		auto twist = solver.twistDistribution();

		// Values from Wind Energy Handbook p124
		std::vector<double> exactChord = { 0.131, 0.131, 0.192, 0.234, 0.263, 0.280, 0.288, 0.291, 0.289, 0.284 };
		std::vector<double> exactTwist = { 54.3, 54.3, 50.6, 47.1, 43.8, 40.8, 37.9, 35.4, 33.1, 31.0 };
		for (int i = 1; i < numPts; ++i) {
			EXPECT_NEAR(exactChord[i], chord[i], 0.1);
			EXPECT_NEAR(exactTwist[i], twist[i] / M_PI * 180.0, 0.1);
		}
		double exactSolidity = 0.8787;
		EXPECT_NEAR(exactSolidity, solver.solidity(), 0.01);
	}

	TEST_F(BEMSolverTest, idealChordDistributionWithWakeRotationThreeBlades) {
		// Values to make tip speed ratio = 6
		int numBlades = 3;
		double wRated = 1.0;
		double uRated = 1.0;
		int numPts = 10;
		double pRated =  0.4 * 1.225 * M_PI * 6.0 * 6.0 / 2.0;

		// Dummy interpolator
		std::vector<double> alphas = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 14.0, 16.0 };
		std::vector<double> liftCoefficients = { 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.1, 1.0, 1.0, 1.0, 1.0 };
		std::vector<double> dragCoefficients = { 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.035, 0.05, 0.06, 0.08, 0.11 };
		AirfoilInterpolator interpolator(alphas, dragCoefficients, liftCoefficients);

		BEMSolver solver(numBlades, pRated, wRated, uRated, interpolator, numPts);
		solver.initialise();

		auto chord = solver.chordDistribution();
		auto twist = solver.twistDistribution();

		// Values from Wind Energy Handbook p124
		std::vector<double> exactChord = { 0.191, 0.191, 0.159, 0.128, 0.105, 0.088, 0.076, 0.067, 0.059, 0.053 };
		std::vector<double> exactTwist = { 32.0, 32.0, 22.5, 17.0, 13.5, 11.2, 9.6, 8.4, 7.4, 6.6 };
		for (int i = 1; i < numPts; ++i) {
			EXPECT_NEAR(exactChord[i], chord[i] / solver.radius(), 0.1);
			EXPECT_NEAR(exactTwist[i], twist[i] / M_PI * 180.0, 0.1);
		}
		double exactSolidity = 0.102;
		EXPECT_NEAR(exactSolidity, solver.solidity(), 0.01);

		solver.findRatedCondition();
		std::vector<double> windSpeeds = { 0.9, 0.95, 1.0, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35 };
		solver.calculatePowerCurve(windSpeeds, 0.0);
	}

	TEST_F(BEMSolverTest, idealChordDistributionWithWakeRotationTwoBlades) {
		// Values to make tip speed ratio = 10
		int numBlades = 2;
		double wRated = 1.0;
		double uRated = 1.0;
		int numPts = 10;
		double pRated =  0.4 * 1.225 * M_PI * 10.0 * 10.0 / 2.0;

		// Dummy interpolator
		std::vector<double> alphas = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 14.0, 16.0 };
		std::vector<double> liftCoefficients = { 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.1, 1.0, 1.0, 1.0, 1.0 };
		std::vector<double> dragCoefficients = { 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.035, 0.05, 0.06, 0.08, 0.11 };
		AirfoilInterpolator interpolator(alphas, dragCoefficients, liftCoefficients);

		BEMSolver solver(numBlades, pRated, wRated, uRated, interpolator, numPts);
		solver.initialise();

		auto chord = solver.chordDistribution();
		auto twist = solver.twistDistribution();

		// Values from Wind Energy Handbook p124
		std::vector<double> exactChord = { 0.143, 0.143, 0.1, 0.075, 0.06, 0.05, 0.042, 0.037, 0.033, 0.029 };
		std::vector<double> exactTwist = { 22.5, 22.5, 14.5, 10.6, 8.4, 6.9, 5.8, 5.1, 4.5, 4.0 };
		for (int i = 1; i < numPts; ++i) {
			EXPECT_NEAR(exactChord[i], chord[i] / solver.radius(), 0.1);
			EXPECT_NEAR(exactTwist[i], twist[i] / M_PI * 180.0, 0.1);
		}
		double exactSolidity = 0.046655;
		EXPECT_NEAR(exactSolidity, solver.solidity(), 0.01);

		solver.findRatedCondition();
		std::vector<double> windSpeeds = { 0.8, 0.85, 0.9, 0.95, 1.0, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35 };
		solver.calculatePowerCurve(windSpeeds, 0.0);
	}
}