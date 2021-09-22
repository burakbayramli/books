/**
 * Author: Rohan Ramasamy
 * Date: 17/06/2017
 */

#pragma once

#include <vof/data/grid/EulerianGrid.h>


namespace vof {
    class VTKWriter
    {
    public:
        static void
        writeStructuredGridToFile(
            const std::shared_ptr<EulerianGrid> &grid,
            const double &time,
            const int& timeStep
            );
    };
}
