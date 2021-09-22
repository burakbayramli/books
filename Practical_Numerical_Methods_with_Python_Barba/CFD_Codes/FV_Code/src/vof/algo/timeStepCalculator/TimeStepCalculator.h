/**
 * Author: Rohan Ramasamy
 * Date: 07/02/2017
 */
 
 #include <vof/data/eos/eosModel/IEquationOfState.h>
 #include <vof/data/grid/EulerianGrid.h>
 
 
 namespace vof {
	class TimeStepCalculator
	{
	public:
		static double
		calculateTimeStep(
			const int& simDimension,
			const std::shared_ptr<EulerianGrid>& grid,
			const std::shared_ptr<IEquationOfState>& eos
			);
	};
 }
