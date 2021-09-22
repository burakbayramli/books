/**
 * Author: Rohan Ramasamy
 * Date: 12/01/2017
 *
 * The test class for simulations
 */

#include <gtest/gtest.h>
#include <vof/controller/Controller.h>


namespace vof {
    template<int DIM>
    class SimulationTest : public ::testing::Test
    {
    public:
        virtual void SetUp() {}

        virtual void TearDown() {}

        /**
         * Run a simulation and compare it with an archived result, ensuring the
         * time taken is reasonable
         */
        void
        runAndCompareSim(
            const std::shared_ptr<IBaseInitialiser> initialiser,
            const std::string& fName,
            const int& simTime
            );

    private:
        static constexpr double TOL = 1e-2;
    };

}

