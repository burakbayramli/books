/**
 * Author: Rohan Ramasamy
 * Date: 12/02/2017
 */

#include <vof/data/eos/eosProperties/IdealEquationOfStateProperties.h>


namespace vof {
IdealEquationOfStateProperties::
IdealEquationOfStateProperties(
    const double& gamma
    ) :
    gamma(gamma),
    gammaMinusOne(gamma - 1),
    gammaPlusOne(gamma + 1),
    gammaMinusOneOverGammaPlusOne((gamma - 1) / (gamma + 1)),
    oneOverGamma(1 / gamma),
    twoOverGammaMinusOne(2 / (gamma - 1)),
    twoOverGammaPlusOne(2 / (gamma + 1)) {}

void
IdealEquationOfStateProperties::
setGamma(
    double newGamma
    )
{
    gamma = newGamma;
    gammaMinusOne = newGamma - 1;
    gammaPlusOne = newGamma + 1;
    gammaMinusOneOverGammaPlusOne = (newGamma - 1) / (newGamma + 1);
    oneOverGamma = 1 / newGamma;
    twoOverGammaMinusOne = 2 / (newGamma - 1);
    twoOverGammaPlusOne = 2 / (newGamma + 1);
}
}