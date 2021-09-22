/**
 * Author: Rohan Ramasamy
 * Date: 27/01/2017
 *
 */
 
#include <gtest/gtest.h>

//#include <vof/controller/problemInitialisers/ShockTubeInitialiser.h>
//#include <vof/controller/problemInitialisers/NohInitialiser.h>
#include <vof/controller/problemInitialisers/DoubleMachInitialiser.h>
#include <vof/controller/Controller.h>
#include <vof/algo/fluxCalculators/FluxCalculatorGodunovImpl.h>


// int main(int argc, char **argv)
// {
//    ::testing::InitGoogleTest(&argc, argv);
//    return RUN_ALL_TESTS();
// }

int main() {

  std::cout << "Initialising problem..." << std::endl;
  auto initialiser = std::make_shared<vof::DoubleMachInitialiser>();
  initialiser->setResolution(0, 400);
  initialiser->setResolution(1, 300);
  initialiser->setDimension(2);

  std::cout << "Running simulation..." << std::endl;
  vof::Controller<2, vof::FluxCalculatorGodunovImpl<2> > controller(initialiser);
  controller.runSimulation();
  std::cout << "Simulation completed successfully!" << std::endl;

  return 0;
}

