/*
Author: Rohan Ramasamy
Data: 26/12/16

This file contains unit tests for AirfoilInterpolator
*/

// Linked Includes
#include "gtest/gtest.h"

#include <vector>
#include <iostream>

#include "../../interpolator/AirfoilInterpolator.h"


namespace bem 
{
	class AirfoilInterpolatorTest : public ::testing::Test {
	public:
		virtual void SetUp() {}

		virtual void TearDown() {}
	};

	TEST_F(AirfoilInterpolatorTest, ConstructorTest) {
		// Mismatched sizes
		std::vector<double> alphas = {0.0, 1.0, 2.0, 3.0};
		std::vector<double> liftCoefficients = {0.0, 1.0};
		std::vector<double> dragCoefficients = {0.0};
		EXPECT_ANY_THROW(AirfoilInterpolator(alphas, dragCoefficients, liftCoefficients));

		// Negative alpha
		alphas = {-1.0, 1.0, 2.0};
		liftCoefficients = {0.0, 1.0, 2.0, 3.0};
		dragCoefficients = {0.0, 1.0, 2.0, 3.0};
		EXPECT_ANY_THROW(AirfoilInterpolator(alphas, dragCoefficients, liftCoefficients));

		// Non-monotonic alpha
		alphas = {1.0, 0.5, 2.0};
		liftCoefficients = {0.0, 1.0, 2.0, 3.0};
		dragCoefficients = {0.0, 1.0, 2.0, 3.0};
		EXPECT_ANY_THROW(AirfoilInterpolator(alphas, dragCoefficients, liftCoefficients));

		// Data set too small
		alphas = {1.0, 0.5};
		liftCoefficients = {0.0, 1.0};
		dragCoefficients = {0.0, 1.0};
		EXPECT_ANY_THROW(AirfoilInterpolator(alphas, dragCoefficients, liftCoefficients));
	}

	TEST_F(AirfoilInterpolatorTest, InterpolateOnDataPoints) {
		std::vector<double> alphas = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
		std::vector<double> liftCoefficients = { 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.1, 1.0 };
		std::vector<double> dragCoefficients = { 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.035, 0.05 };
		AirfoilInterpolator interpolator(alphas, dragCoefficients, liftCoefficients);

		for (size_t i = 0; i < alphas.size(); ++i) {
			EXPECT_EQ(liftCoefficients[i], interpolator.getLiftCoefficient(alphas[i]));
			EXPECT_EQ(dragCoefficients[i], interpolator.getDragCoefficient(alphas[i]));
		}
	}

	TEST_F(AirfoilInterpolatorTest, InterpolateOutsideDataLimits) {
		std::vector<double> alphas = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
		std::vector<double> liftCoefficients = { 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.1, 1.0 };
		std::vector<double> dragCoefficients = { 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.035, 0.05 };
		AirfoilInterpolator interpolator(alphas, dragCoefficients, liftCoefficients);

		EXPECT_ANY_THROW(interpolator.getLiftCoefficient(10.6));
		EXPECT_ANY_THROW(interpolator.getDragCoefficient(10.6));
		EXPECT_ANY_THROW(interpolator.getLiftCoefficient(-0.1));
		EXPECT_ANY_THROW(interpolator.getDragCoefficient(-0.1));
	}

	TEST_F(AirfoilInterpolatorTest, OptimumAngle) {
		std::vector<double> alphas = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
		std::vector<double> liftCoefficients = { 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.1, 1.0 };
		std::vector<double> dragCoefficients = { 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.035, 0.05 };
		AirfoilInterpolator interpolator(alphas, dragCoefficients, liftCoefficients);

		EXPECT_EQ(7.0, interpolator.getIdealAngleOfAttack());
	}

}
