/**
 * Author: Rohan Ramasamy
 * Date: 28/02/2017
 */

#pragma once

#include <vof/data/grid/EulerianGrid.h>


namespace vof {
template<int DIM>
class CellUpdaterImpl {
public:
    static void
    updateStates(
        const double& deltaT,
        const std::shared_ptr<EulerianGrid>& grid,
        const std::shared_ptr<IEquationOfState>& eos
        );
};
}
