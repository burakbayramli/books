/**
 * Author: Rohan Ramasamy
 * Date: 10/02/2017
 * 
 * Initialisation class for shock tube simulations
 */

 #include <vof/controller/problemInitialisers/BaseInitialiser.h>
 #include <vof/algo/riemann_solvers/ExactRiemannSolver.h>
 
 
 namespace vof {
	class ShockTubeInitialiser
	    : public BaseInitialiser
	{
	public:
		ShockTubeInitialiser(
			const double& membraneLoc,
			const int& dim,
			const RiemannState& leftState,
			const RiemannState& rightState,
            const double& finalTime
			);
	
		InitialGridState
		getState(
			double xLoc,
			double yLoc,
			double zLoc
			) const override;
	
		std::vector<std::shared_ptr<IEquationOfState> >
		getEquationsOfState() const override;

    protected:
        int mDim;

	private:
		double mMembraneLoc;
		RiemannState mLeftState;
		RiemannState mRightState;
		std::shared_ptr<IEquationOfState> mLeftEos;
		std::shared_ptr<IEquationOfState> mRightEos;
	};

     /**
      * Shock tube problem types
      */
    class Sod1Initialiser
        : public ShockTubeInitialiser
    {
    public:
        Sod1Initialiser(
            int dim
            );
    };

     class Sod2Initialiser
             : public ShockTubeInitialiser
     {
     public:
         Sod2Initialiser(
             int dim
             );
     };

     class Sod3Initialiser
             : public ShockTubeInitialiser
     {
     public:
         Sod3Initialiser(
             int dim
             );
     };

     class Sod4Initialiser
             : public ShockTubeInitialiser
     {
     public:
         Sod4Initialiser(
             int dim
             );
     };

     class Sod5Initialiser
             : public ShockTubeInitialiser
     {
     public:
         Sod5Initialiser(
             int dim
             );
     };
 }
