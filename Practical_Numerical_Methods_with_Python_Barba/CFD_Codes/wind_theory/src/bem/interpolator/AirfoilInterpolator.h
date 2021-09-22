/**
 *  Author: Rohan Ramasamy
 *  Date: 26/12/16
 *
 *  This file contains a class for interpolating empirical data that characterises a blade lift
 *  and drag forces.
 */

#include <vector>
#include <utility>

namespace bem 
{

	class AirfoilInterpolator {
	public:
		/**
		 * Constructor storing data for lift and drag curves
		 */
		AirfoilInterpolator(
			std::vector<double> alpha,
			std::vector<double> dragCoefficients,
			std::vector<double> liftCoefficients
			);

		/**
		 * Find the corresponding lift coefficient to the given angle attack.
		 */
		double
		getLiftCoefficient(
			double interpolatedAlpha
			);

		/**
		 * Find the corresponding drag coefficient to the given angle attack.
		 */
		double
		getDragCoefficient(
			double interpolatedAlpha
			);

		/**
		 * Finds the point of maximum Cl/Cd to get the angle of attack
		 */
		double
		getIdealAngleOfAttack();

	private:
		/**
		 * Function used to get the index and scaling between the two closest points
		 */
		std::pair<int, double>
		getIndexAndScaling(
			double interpolatedAlpha
			);

		std::vector<double> mAlpha, mDragCoefficients, mLiftCoefficients;
	};

}
