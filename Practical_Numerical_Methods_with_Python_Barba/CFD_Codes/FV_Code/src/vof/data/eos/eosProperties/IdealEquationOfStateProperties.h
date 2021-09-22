/**
 * Author: Rohan Ramasamy
 * Date: 12/02/2017
 *
 * Properties class for a single fluid component ideal gas.
 */

#pragma once

namespace vof {
struct IdealEquationOfStateProperties {
    IdealEquationOfStateProperties() {}

    IdealEquationOfStateProperties(
        const double& gamma
        );

    double gamma;
    double gammaMinusOne;
    double gammaPlusOne;
    double gammaMinusOneOverGammaPlusOne;
    double oneOverGamma;
    double twoOverGammaMinusOne;
    double twoOverGammaPlusOne;

private:
    /**
     * Function to be used by multi-component eos state updater
     */
    void
    setGamma(
        double newGamma
        );
};
}
