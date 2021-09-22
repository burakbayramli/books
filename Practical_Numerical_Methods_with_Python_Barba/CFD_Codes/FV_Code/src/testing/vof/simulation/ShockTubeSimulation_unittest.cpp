/**
 * Author: Rohan Ramasamy
 * Date: 12/01/2017
 *
 * The test class for Shock Tube
 */

#include <testing/vof/simulation/SimulationTest.h>
#include <vof/controller/problemInitialisers/ShockTubeInitialiser.h>

namespace vof {
    class ShockTubeSimulationTest :
        public SimulationTest<1>
    {
        virtual void SetUp() {}

        virtual void TearDown() {}
    };

    TEST_F(ShockTubeSimulationTest, Sod1) {
        auto initialiser = std::make_shared<Sod1Initialiser>(0.0);
        initialiser->setOutputResults(false);

        runAndCompareSim(initialiser, "Sod1Archive", 45);
    }

    TEST_F(ShockTubeSimulationTest, Sod2) {
        auto initialiser = std::make_shared<Sod2Initialiser>(0.0);
        initialiser->setOutputResults(false);

        runAndCompareSim(initialiser, "Sod2Archive", 35);
    }

    TEST_F(ShockTubeSimulationTest, Sod3) {
        auto initialiser = std::make_shared<Sod3Initialiser>(0.0);
        initialiser->setOutputResults(false);

        runAndCompareSim(initialiser, "Sod3Archive", 45);
    }

    TEST_F(ShockTubeSimulationTest, Sod4) {
        auto initialiser = std::make_shared<Sod4Initialiser>(0.0);
        initialiser->setOutputResults(false);

        runAndCompareSim(initialiser, "Sod4Archive", 50);
    }

    TEST_F(ShockTubeSimulationTest, Sod5) {
        auto initialiser = std::make_shared<Sod5Initialiser>(0.0);
        initialiser->setOutputResults(false);

        runAndCompareSim(initialiser, "Sod5Archive", 45);
    }
}
