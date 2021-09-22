/**
 * Author: Rohan Ramasamy
 * Date: 24/02/2017
 *
 * The test class for Noh
 */

#include <testing/vof/simulation/SimulationTest.h>
#include <vof/controller/problemInitialisers/NohInitialiser.h>


namespace vof {
    class NohSimulationTest :
            public SimulationTest<1>
    {
        virtual void SetUp() {}

        virtual void TearDown() {}
    };

    TEST_F(NohSimulationTest, Noh) {
        auto initialiser = std::make_shared<NohInitialiser>(1, 0);
        initialiser->setOutputResults(false);

        runAndCompareSim(initialiser, "NohArchive", 45);
    }

}