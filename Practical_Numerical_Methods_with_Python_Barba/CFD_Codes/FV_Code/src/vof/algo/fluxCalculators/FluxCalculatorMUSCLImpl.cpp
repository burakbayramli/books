/**
 * Author: Rohan Ramasamy
 * Date: 09/02/2017
 */

#include <vof/algo/fluxCalculators/FluxCalculatorMUSCLImpl.h>


namespace vof {
template<int DIM, typename FluxLimiter>
const int FluxCalculatorMUSCLImpl<DIM, FluxLimiter>::RADIUS = 2;

template<int DIM, typename FluxLimiter>
void
FluxCalculatorMUSCLImpl<DIM, FluxLimiter>::
evaluateFluxes(
    const std::shared_ptr<EulerianGrid>& grid,
    const std::shared_ptr<IEquationOfState>& eos
    )
{
    throw std::runtime_error("Not implemented!");
}

}
