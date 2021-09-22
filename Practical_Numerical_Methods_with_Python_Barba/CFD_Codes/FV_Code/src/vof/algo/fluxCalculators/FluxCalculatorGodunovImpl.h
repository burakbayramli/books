/**
 * Author: Rohan Ramasamy
 * Date: 09/02/2017
 */
 
 #pragma once
 
 #include <vof/data/grid/EulerianGrid.h>
 #include <vof/data/eos/eosModel/IEquationOfState.h>
 
 
 namespace vof {
    template<int DIM>
	class FluxCalculatorGodunovImpl
	{
	public:
		static const int RADIUS;

		static void
		evaluateFluxes(
			const std::shared_ptr<EulerianGrid>& grid,
			const std::shared_ptr<IEquationOfState>& eos
			);
	};
 }
