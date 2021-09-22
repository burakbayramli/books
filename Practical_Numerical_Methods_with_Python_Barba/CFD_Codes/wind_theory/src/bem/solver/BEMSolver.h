/**
 *  Author: Rohan Ramasamy
 *  Date: 27/12/16
 *
 *  This file contains a class for solving the blade element momentum equations to approximate
 * the performance of a wind turbine.
 */

 #include "../interpolator/AirfoilInterpolator.h"

 #include <utility>
 #include <vector>


 namespace bem {

 	class BEMSolver {
 	public:
 		/**
 		 * Construct BEM Solver. Rated input parameters define the optimum point. The 
 		 * AirfoilInterpolator defines the blade lift and drag characteristics. numPts
 		 * defines the number of segments the blade is split into. 
 		 */
 		BEMSolver(
 			int numBlades,
 			double ratedPower,
 			double ratedAngularRotation,
 			double ratedWindSpeed,
 			AirfoilInterpolator airfoil,
 			int numPts
 			);

 		/**
 		 * Check to see if rated condition has been found. The radius is calculated, and 
 		 * the chord and twist have been set to the values of the ideal rotor with wake 
 		 * rotation.
 		 */
		inline
 		bool
 		isInitialised()
 		{
 			return mInitialised;
 		}

 		/**
 		 * Set to be initialised once rated condition is found.
 		 */ 
 		void
 		initialise();


 		/**
 		 * Use rated conditions to get TSR and rated power coefficient.
 		 */
 		std::pair<double, double>
 		findRatedCondition();

 		/**
 		 * Generate the power curve against tip speed ratio for a given twist.
 		 */
 		std::vector<double>
 		calculatePowerCurve(
 			std::vector<double> windSpeeds,
 			double pitchTwist
 			);

 		/**
 		 * Return the rated condition as a pair: first is the TSR and second is the 
 		 * power coefficient
 		 */

 		inline
 		double
 		radius()
 		{
 			return mRadius;
 		}

 		inline
 		double
 		solidity()
 		{
 			return mSolidity;
 		}

 		inline
 		std::pair<double, double>
 		ratedCondition()
 		{
 			return mRatedCondition;
 		}

 		/**
 		 * Return chord distribution
 		 */
 		inline
 		std::vector<double>
 		chordDistribution()
 		{
 			return mChord;
 		}

 		/**
 		 * Return inherant blade twist distribution
 		 */
		inline
		std::vector<double>
		twistDistribution()
		{
			return mInherantTwist;
		}		

 	private:
 		/**
 		 *	Function to calculate power coefficient for an arbitrary wind speed and active
 		 *  pitch angle on the blades. This assumes that the angular rotation is fixed at 
 		 *  wRated.
 		 */
		double
		findPowerCoefficient(
			double activePitch,
			double windSpeed,
			bool isRatedCondition=false
			);

 		bool mInitialised;
 		int mNumBlades;
 		double mPRated, mWRated, mURated, mRadius, mAlpha, mSolidity;
 		std::pair<double, double> mRatedCondition;
 		AirfoilInterpolator mAirfoil;
 		std::vector<double> mChord, mInherantTwist, mLocalSolidity;
 	};

 }