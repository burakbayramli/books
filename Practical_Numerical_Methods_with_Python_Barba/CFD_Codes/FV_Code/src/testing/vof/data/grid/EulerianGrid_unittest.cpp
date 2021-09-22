/**
 * Author: Rohan Ramasamy
 * Date: 30/01/2017
 * 
 * Test framework for Grid class
 */


#include <gtest/gtest.h>

#include <utility>

#include <vof/data/grid/EulerianGrid.h>
#include <vof/data/grid/kernel/StateGrid.h>
#include <vof/controller/problemInitialisers/BaseInitialiser.h>


namespace vof {
	class EulerianGridTest : public ::testing::Test {
	public:
		static constexpr double TOL = 1e-2; 
	
		virtual void SetUp() {}

		virtual void TearDown() {}
		
		/**
		 * Uses a local class to set up a grid with a uniform state for 
		 * testing
		 */
		std::shared_ptr<EulerianGrid>
		constructUniformGrid(
			double constant,
			int numX,
			int numY, 
			int numZ,
			int numGhostCells,
			const Vector<3>& cellDiagonal
			) 
		{
			std::shared_ptr<EulerianGrid> grid = std::make_shared<EulerianGrid>(numX, numY, numZ, numGhostCells, cellDiagonal);
			
			class ConstantGridInitialiser :
			    public BaseInitialiser
			{
			public:
				ConstantGridInitialiser(
					double constant
					) :
				    BaseInitialiser(3, {10, 10, 10}, {1.0, 1.0, 1.0}),
					mConstant(constant) {}
				
				InitialGridState
				getState(
					double xLoc,
					double yLoc,
					double zLoc
					) const override
				{
					xLoc = 0.0;
					yLoc = 0.0;
					zLoc = 0.0;
					return InitialGridState(mConstant, mConstant, mConstant,
					                        {{mConstant, mConstant, mConstant}});
				}
				
				std::vector<std::shared_ptr<IEquationOfState> >
				getEquationsOfState() const override
				{
					return std::vector<std::shared_ptr<IEquationOfState> >();
				}
				
			private:
				double mConstant;
			};
			
			std::shared_ptr<IBaseInitialiser> initialiser = std::make_shared<ConstantGridInitialiser>(constant);
			
			auto eosVector = initialiser->initialiseGrid(grid);
			
			return grid;
		}
	};
	
	TEST_F(EulerianGridTest, testUniformGrid) {
		int res = 10;
		double constant = 5.0;
		int ghostCells = 1;
		const auto grid = constructUniformGrid(constant, res, res, res, ghostCells, Vector<3>(1.0));
		
		const StateGrid<double>& densities = grid->densities();
		const StateGrid<double>& pressures = grid->pressures();
		const StateGrid<double>& internalEnergies = grid->internalEnergies();
		const StateGrid<std::array<double, 3> > velocities = grid->velocities();
		
		for (int i = 0; i < res; ++i) {
			for (int j = 0; j < res; ++j) {
				for (int k = 0; k < res; ++k) {
					EXPECT_EQ(densities(i, j, k), constant);
					EXPECT_EQ(pressures(i, j, k), constant);
					EXPECT_EQ(internalEnergies(i, j, k), constant);
					for (int dim = 0; dim < 2; ++dim) {
						EXPECT_EQ(velocities(i, j, k)[dim], constant);
					}
				}
			}
		}
		
		// Test inside boundary region
		EXPECT_NO_THROW(densities(-1, -1, -1));
		EXPECT_NO_THROW(pressures(res, res, res));
		
		// Test outside of grid
		EXPECT_ANY_THROW(densities(-2, 1, 4));
		EXPECT_ANY_THROW(densities(-1, res + 1, 4));
	}
}
