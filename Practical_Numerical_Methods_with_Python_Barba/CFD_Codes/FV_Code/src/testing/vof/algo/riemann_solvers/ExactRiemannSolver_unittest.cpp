/**
 * Author: Rohan Ramasamy
 * Date: 30/01/2017
 * 
 * The class for an exact riemann solver
 */


#include <gtest/gtest.h>

#include <fstream>
#include <iostream>

#include <vof/data/eos/eosModel/IdealGasEquationofState.h>
#include <vof/algo/riemann_solvers/ExactRiemannSolver.h>


namespace vof 
{
	class ExactRiemannSolverTest : public ::testing::Test {
	public:
		static constexpr double TOL = 1e-2; 
	
		virtual void SetUp() {}

		virtual void TearDown() {}
		
		void
		runRiemannProblem(
		    RiemannInitialConditions initialConditions,
		    RiemannMidState expectedMidState
		    )
		{
			auto calculatedMidState = ExactRiemannSolver::calculateMidState(initialConditions);
			
			EXPECT_NEAR(expectedMidState.mPressureStar - calculatedMidState.mPressureStar, 0.0, TOL);
			EXPECT_NEAR(expectedMidState.mVelocityStar - calculatedMidState.mVelocityStar, 0.0, TOL);
		}
		
		void
		sampleFunction(
			const RiemannInitialConditions initialConditions,
			const std::string& fName,
			double t,
			double xOffset
			)
		{
			std::ofstream output;
			output.open(fName);
			auto midState = ExactRiemannSolver::calculateMidState(initialConditions);
			
			double x = -xOffset;
			bool isLeft;
			while (x < -xOffset + 1.0) {
				auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, x / t, isLeft);
				
				output << x + xOffset << " " << sampleState.mDensity << " " << sampleState.mPressure 
				       << " " << sampleState.mVelocity << " "
					   << sampleState.mEos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity,
																					sampleState.mPressure) << "\n";
				
				x += 0.01;
			}
			output.close();
		}
		
		
		void
		sampleFunctionTest(
			const RiemannInitialConditions initialConditions,
			const std::string& fName
			)
		{
			auto midState = ExactRiemannSolver::calculateMidState(initialConditions);

			std::string filePath = "../src/testing/vof/algo/riemann_solvers/sod/" + fName;
			std::ifstream input(filePath, std::ios::in);
			double sampleSpeed = 0;
			double density = 0;
			double pressure = 0;
			double velocity = 0;
			double energy = 0;
			std::string line;
			int iteration = 0;
			bool isLeft;
			while (!input.eof()) {
				input >> sampleSpeed >> density >> pressure >> velocity >> energy;
				
				auto sampleState = ExactRiemannSolver::sample(initialConditions, midState, sampleSpeed, isLeft);
				
				//std::cout << sampleSpeed << " " << density << " " << pressure << " " << velocity << " " << energy << std::endl;
				EXPECT_NEAR(sampleState.mDensity, density, TOL);
				EXPECT_NEAR(sampleState.mPressure, pressure, TOL);
				EXPECT_NEAR(sampleState.mVelocity, velocity, TOL);
				EXPECT_NEAR(sampleState.mEos->getInternalEnergyFromDensityAndPressure(sampleState.mDensity, sampleState.mPressure),
						   energy, TOL);
				++iteration;
			}
		}
	};
	
	TEST_F(ExactRiemannSolverTest, Toro1) 
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 1.0, 0.0, eos);
		RiemannState rightState(0.125, 0.1, 0.0, eos);
		
		RiemannInitialConditions initialConditions(leftState, rightState);
		RiemannMidState expectedMidState(0.30313, 0.92745);
		
		runRiemannProblem(initialConditions, expectedMidState);
	}
	
	TEST_F(ExactRiemannSolverTest, Toro1Sample)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 1.0, 0.0, eos);
		RiemannState rightState(0.125, 0.1, 0.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);

		sampleFunctionTest(initialConditions, "Sod1");
	}

	TEST_F(ExactRiemannSolverTest, Toro2)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 0.4, -2.0, eos);
		RiemannState rightState(1.0, 0.4, 2.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);
		RiemannMidState expectedMidState(0.00189, 0.0);

		runRiemannProblem(initialConditions, expectedMidState);
	}

	TEST_F(ExactRiemannSolverTest, Toro2Sample)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 0.4, -2.0, eos);
		RiemannState rightState(1.0, 0.4, 2.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);

		sampleFunctionTest(initialConditions, "Sod2");
	}

	TEST_F(ExactRiemannSolverTest, Toro3)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 1000.0, 0.0, eos);
		RiemannState rightState(1.0, 0.01, 0.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);
		RiemannMidState expectedMidState(460.894, 19.5975);

		runRiemannProblem(initialConditions, expectedMidState);
	}

	TEST_F(ExactRiemannSolverTest, Toro3Sample)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 1000.0, 0.0, eos);
		RiemannState rightState(1.0, 0.01, 0.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);

		sampleFunctionTest(initialConditions, "Sod3");
	}

	TEST_F(ExactRiemannSolverTest, Toro4)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 0.01, 0.0, eos);
		RiemannState rightState(1.0, 100.0, 0.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);
		RiemannMidState expectedMidState(46.0950, -6.19633);

		runRiemannProblem(initialConditions, expectedMidState);
	}

	TEST_F(ExactRiemannSolverTest, Toro4Sample)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(1.0, 0.01, 0.0, eos);
		RiemannState rightState(1.0, 100.0, 0.0, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);

		sampleFunctionTest(initialConditions, "Sod4");
	}

	TEST_F(ExactRiemannSolverTest, Toro5)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(5.99924, 460.894, 19.5975, eos);
		RiemannState rightState(5.99242, 46.0950, -6.19633, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);
		RiemannMidState expectedMidState(1691.64, 8.68975);

		runRiemannProblem(initialConditions, expectedMidState);
	}

	TEST_F(ExactRiemannSolverTest, Toro5Sample)
	{
		std::shared_ptr<IEquationOfState> eos = std::make_shared<IdealGasEquationOfState>(1.4);
		RiemannState leftState(5.99924, 460.894, 19.5975, eos);
		RiemannState rightState(5.99242, 46.0950, -6.19633, eos);

		RiemannInitialConditions initialConditions(leftState, rightState);

		sampleFunctionTest(initialConditions, "Sod5");
	}

	// Sample Tests - Used to write out sample results to file for testing of the Riemann Solver
//	TEST_F(ExactRiemannSolverTest, Toro1SamplePlot)
//	{
//		std::shared_ptr<IEquationOfState> eosModel = std::make_shared<IdealGasEquationOfState>(1.4);
//		RiemannState leftState(1.0, 1.0, 0.0, eosModel);
//		RiemannState rightState(0.125, 0.1, 0.0, eosModel);
//
//		RiemannInitialConditions initialConditions(leftState, rightState);
//
//		sampleFunction(initialConditions, "Sod1", 0.25, 0.5);
//	}
//
//	TEST_F(ExactRiemannSolverTest, Toro2SamplePlot)
//	{
//		std::shared_ptr<IEquationOfState> eosModel = std::make_shared<IdealGasEquationOfState>(1.4);
//		RiemannState leftState(1.0, 0.4, -2.0, eosModel);
//		RiemannState rightState(1.0, 0.4, 2.0, eosModel);
//
//		RiemannInitialConditions initialConditions(leftState, rightState);
//
//		sampleFunction(initialConditions, "Sod2", 0.15, 0.5);
//	}
//
//	TEST_F(ExactRiemannSolverTest, Toro3SamplePlot)
//	{
//		std::shared_ptr<IEquationOfState> eosModel = std::make_shared<IdealGasEquationOfState>(1.4);
//		RiemannState leftState(1.0, 1000.0, 0.0, eosModel);
//		RiemannState rightState(1.0, 0.01, 0.0, eosModel);
//
//		RiemannInitialConditions initialConditions(leftState, rightState);
//
//		sampleFunction(initialConditions, "Sod3", 0.012, 0.5);
//	}
//
//	TEST_F(ExactRiemannSolverTest, Toro4SamplePlot)
//	{
//		std::shared_ptr<IEquationOfState> eosModel = std::make_shared<IdealGasEquationOfState>(1.4);
//		RiemannState leftState(1.0, 0.01, 0.0, eosModel);
//		RiemannState rightState(1.0, 100.0, 0.0, eosModel);
//
//		RiemannInitialConditions initialConditions(leftState, rightState);
//
//		sampleFunction(initialConditions, "Sod4", 0.035, 0.5);
//	}
//
//
//	TEST_F(ExactRiemannSolverTest, Toro5SamplePlot)
//	{
//		std::shared_ptr<IEquationOfState> eosModel = std::make_shared<IdealGasEquationOfState>(1.4);
//		RiemannState leftState(5.99924, 460.894, 19.5975, eosModel);
//		RiemannState rightState(5.99242, 46.0950, -6.19633, eosModel);
//
//		RiemannInitialConditions initialConditions(leftState, rightState);
//
//		sampleFunction(initialConditions, "Sod5", 0.035, 0.5);
//	}
//
//	TEST_F(ExactRiemannSolverTest, Toro6SamplePlot)
//	{
//		std::shared_ptr<IEquationOfState> eosModel = std::make_shared<IdealGasEquationOfState>(1.4);
//		RiemannState leftState(1.0, 1.0, 0.75, eosModel);
//		RiemannState rightState(0.125, 0.1, 0.0, eosModel);
//
//		RiemannInitialConditions initialConditions(leftState, rightState);
//
//		sampleFunction(initialConditions, "Sod6", 0.2, 0.3);
//	}
}
