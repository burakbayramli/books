/**
 *  Author: Rohan Ramasamy
 *  Date: 26/12/16
 *
 *  This file contains a class for interpolating empirical data that characterises a blade lift
 *  and drag forces.
 */

 #include "AirfoilInterpolator.h"
 
 #include <cassert>
 #include <math.h>
 #include <stdexcept>
 #include <iostream>
 #include <sstream>

 namespace bem 
 {

 	AirfoilInterpolator::
	AirfoilInterpolator(
		std::vector<double> alpha,
		std::vector<double> dragCoefficients,
		std::vector<double> liftCoefficients
		) :
	    mAlpha(alpha),
	    mDragCoefficients(dragCoefficients),
	    mLiftCoefficients(liftCoefficients) 
    {
    	// Run checks on data
    	if (mAlpha.size() != mDragCoefficients.size()) throw std::runtime_error("Inconsistent data set sizes!");
    	if (mAlpha.size() != mLiftCoefficients.size()) throw std::runtime_error("Inconsistent data set sizes!");
    	if (mAlpha.size() < 3) throw std::runtime_error("Data set is too small!");

    	if (mAlpha[0] < 0.0) throw std::runtime_error("Alpha is assumed non-negative!");
    	for (size_t i = 0; i < mAlpha.size() - 1; ++i) {
    		if (mAlpha[i] > mAlpha[i + 1]) throw std::runtime_error("Alpha is not monotonic!");
    	}
    }

	std::pair<int, double>
	AirfoilInterpolator::
	getIndexAndScaling(
		double interpolatedAlpha
		)
	{
		// Check interpolated alpha is within the range of data
		if (mAlpha[0] > interpolatedAlpha || mAlpha[mAlpha.size() - 1] < interpolatedAlpha) {
			std::stringstream ss;
			ss << "Alpha: " << interpolatedAlpha << " is outside of range!";
			throw std::runtime_error(ss.str());
		} 
		if (isnan(interpolatedAlpha)) throw std::runtime_error("Alpha is NaN!");

		// Find closest index below interpolated value
		size_t idx = mAlpha.size() / 2;
		size_t lowIdx = 0;
		size_t highIdx = mAlpha.size() - 1;
		bool foundIdx = false;
		while (!foundIdx) {
			if (mAlpha[idx] < interpolatedAlpha) {
				lowIdx = idx;
				idx = (highIdx + lowIdx) / 2;
			}
			else if (mAlpha[idx] == interpolatedAlpha) {
				// Do nothing
			}
			else {
				highIdx = idx;
				idx = (highIdx + lowIdx) / 2;
			}

			foundIdx = (mAlpha[idx] <= interpolatedAlpha) && 
			           (mAlpha[idx + 1] >= interpolatedAlpha);
		} 

		// Get scaling
		double scaling = (interpolatedAlpha - mAlpha[idx]) / (mAlpha[idx + 1] - mAlpha[idx]);

		return std::make_pair(idx, scaling);
	}

    double
    AirfoilInterpolator::
	getLiftCoefficient(
		double interpolatedAlpha
		)
	{
		auto idxAndScaling = getIndexAndScaling(interpolatedAlpha);

		double lowerLift = mLiftCoefficients[idxAndScaling.first];
		double upperLift = mLiftCoefficients[idxAndScaling.first + 1];

		return lowerLift + idxAndScaling.second * (upperLift - lowerLift);
	}

	double
	AirfoilInterpolator::
	getDragCoefficient(
		double interpolatedAlpha
		)
	{
		auto idxAndScaling = getIndexAndScaling(interpolatedAlpha);

		double lowerDrag = mDragCoefficients[idxAndScaling.first];
		double upperDrag = mDragCoefficients[idxAndScaling.first + 1];

		return lowerDrag + idxAndScaling.second * (upperDrag - lowerDrag);
	}

	double
	AirfoilInterpolator::
	getIdealAngleOfAttack()
	{
		size_t alphaMaxIdx = 0;
		double minCdOverCl = std::numeric_limits<double>::max();
		for (size_t i = 0; i < mAlpha.size(); ++i) {
			double currentCdOverCl =  mDragCoefficients[i] / mLiftCoefficients[i];
			if (minCdOverCl > currentCdOverCl) {
				minCdOverCl = currentCdOverCl;
				alphaMaxIdx = i;
			}
		}
		return mAlpha[alphaMaxIdx];
	}

 }
