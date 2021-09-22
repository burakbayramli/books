/**
 * Author: Rohan Ramasamy
 * Date: 02/03/2017
 *
 * The test class for Double Mach
 */

#include <testing/vof/simulation/SimulationTest.h>
#include <vof/controller/problemInitialisers/DoubleMachInitialiser.h>


namespace vof {
class DoubleMachSimulationTest :
    public SimulationTest<2>
{
    virtual void SetUp() {}

    virtual void TearDown() {}
};

TEST_F(DoubleMachSimulationTest, DISABLED_DoubleMach) {
    auto initialiser = std::make_shared<DoubleMachInitialiser>();
    initialiser->setOutputResults(false);

    runAndCompareSim(initialiser, "DoubleMachArchive", 60000);
}

}