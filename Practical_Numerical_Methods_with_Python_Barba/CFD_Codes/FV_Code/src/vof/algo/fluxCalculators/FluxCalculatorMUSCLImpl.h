/**
 * Author: Rohan Ramasamy
 * Date: 18/06/2017
 */


#pragma once

#include <vof/data/grid/EulerianGrid.h>
#include <vof/data/eos/eosModel/IEquationOfState.h>


namespace vof {
template<int DIM, typename FluxLimiter>
class FluxCalculatorMUSCLImpl
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
