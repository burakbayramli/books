/**
 * Author: Rohan Ramasamy
 * Date: 10/02/2017
 * 
 * Initialisation class for shock tube simulations
 */

#include <vof/controller/problemInitialisers/ShockTubeInitialiser.h>
#include <vof/data/eos/eosModel/IdealGasEquationOfState.h>
#include <vof/algo/boundaryConditions/TransmissiveBoundaryCondition.h>


namespace vof {
 
 ShockTubeInitialiser::
 ShockTubeInitialiser(
	const double& membraneLoc,
	const int& dim,
	const RiemannState& leftState,
	const RiemannState& rightState,
    const double& finalTime
	) :
    BaseInitialiser(1, {{100, 1, 1}}, {{1.0, 1.0, 1.0}}),
	mDim(dim),
	mMembraneLoc(membraneLoc),
	mLeftState(leftState),
	mRightState(rightState)
 {
	 mLeftEos = mLeftState.mEos;
	 mRightEos = mRightState.mEos;

     for (size_t i = 0; i < mBoundaryConditions.size(); ++i) {
         mBoundaryConditions[i] = std::make_shared<TransmissiveBoundaryCondition>();
     }
     mFinalTime = finalTime;
 }
	
 InitialGridState
 ShockTubeInitialiser::
 getState(
	double xLoc,
	double yLoc,
	double zLoc
	) const
 {
	double dimLoc;
	if (mDim == 0) {
		dimLoc = xLoc;
	}
	else if (mDim == 1) {
		dimLoc = yLoc;
	}
	else if (mDim == 2) {
		dimLoc = zLoc;
	}
	else {
		throw std::runtime_error("Not implemented error!");
	}

	std::array<double, 3> velocities = {{0.0, 0.0, 0.0}};
	if (dimLoc < mMembraneLoc) {
		velocities[mDim] = mLeftState.mVelocity;
		return InitialGridState(mLeftState.mDensity,
		                        mLeftState.mPressure,
		                        mLeftEos->getInternalEnergyFromDensityAndPressure(mLeftState.mDensity, mLeftState.mPressure),
		                        velocities);
	}
	else {
		velocities[mDim] = mRightState.mVelocity;
		return InitialGridState(mRightState.mDensity,
		                        mRightState.mPressure,
		                        mRightEos->getInternalEnergyFromDensityAndPressure(mRightState.mDensity, mRightState.mPressure),
		                        velocities);
	}
 }
 
 std::vector<std::shared_ptr<IEquationOfState> >
 ShockTubeInitialiser::
 getEquationsOfState() const
 {
	 std::vector<std::shared_ptr<IEquationOfState> > eosVector;
	 eosVector.push_back(mLeftState.mEos);
	 eosVector.push_back(mRightState.mEos);
	 
	 return eosVector;
 }

 Sod1Initialiser::
 Sod1Initialiser(
	 int dim
	 ) :
     ShockTubeInitialiser(0.3,
						  dim,
						  RiemannState(1.0, 1.0, 0.75, std::make_shared<IdealGasEquationOfState>(1.4)),
						  RiemannState(0.125, 0.1, 0.0, std::make_shared<IdealGasEquationOfState>(1.4)),
                          0.25
	                      )
 {
	assert(mDim < 3 && mDim >= 0);
 }

 Sod2Initialiser::
 Sod2Initialiser(
	 int dim
 	 ) :
	 ShockTubeInitialiser(0.5,
						  dim,
						  RiemannState(1.0, 0.4, -2.0, std::make_shared<IdealGasEquationOfState>(1.4)),
						  RiemannState(1.0, 0.4, 2.0, std::make_shared<IdealGasEquationOfState>(1.4)),
                          0.15
						  )
 {
	 assert(mDim < 3 && mDim >= 0);
 }

 Sod3Initialiser::
 Sod3Initialiser(
	 int dim
 	 ) :
	 ShockTubeInitialiser(0.5,
						  dim,
						  RiemannState(1.0, 1000.0, 0.0, std::make_shared<IdealGasEquationOfState>(1.4)),
						  RiemannState(1.0, 0.01, 0.0, std::make_shared<IdealGasEquationOfState>(1.4)),
						  0.012
                          )
 {
	 assert(mDim < 3 && mDim >= 0);
 }

 Sod4Initialiser::
 Sod4Initialiser(
    int dim
 	) :
    ShockTubeInitialiser(0.4,
	   				     dim,
					     RiemannState(5.99924, 460.894, 19.5975, std::make_shared<IdealGasEquationOfState>(1.4)),
					     RiemannState(5.99242, 46.095, -6.19633, std::make_shared<IdealGasEquationOfState>(1.4)),
                         0.035
	                     )
 {
	 assert(mDim < 3 && mDim >= 0);
 }


 Sod5Initialiser::
 Sod5Initialiser(
	 int dim
 	 ) :
	 ShockTubeInitialiser(0.8,
						  dim,
						  RiemannState(1.0, 1000.0, -19.5975, std::make_shared<IdealGasEquationOfState>(1.4)),
						  RiemannState(1.0, 0.01, -19.59745, std::make_shared<IdealGasEquationOfState>(1.4)),
                          0.012
						  )
 {
	 assert(mDim < 3 && mDim >= 0);
 }
 
 }
