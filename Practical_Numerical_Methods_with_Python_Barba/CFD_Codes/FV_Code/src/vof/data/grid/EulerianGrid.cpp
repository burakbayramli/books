/**
 * Author: Rohan Ramasamy
 * Date: 05/02/2017
 */

 #include <cmath>

 #include <vof/data/grid/EulerianGrid.h>

 
 namespace vof {
	 EulerianGrid::
	 EulerianGrid(
		const int& numX,
		const int& numY,
		const int& numZ,
		const int& numGhostCells,
		const Vector<3>& cellDiagonal
		) :
		mCellResolution{{numX, numY, numZ}},
		mNumGhostCells(numGhostCells),
		mDensities(numX, numY, numZ, numGhostCells),
		mPressures(numX, numY, numZ, numGhostCells),
		mVelocities(numX, numY, numZ, numGhostCells),
		mInternalEnergies(numX, numY, numZ, numGhostCells),
		mDensityFluxes(numX, numY, numZ, numGhostCells),
		mMomentumFluxes(numX, numY, numZ, numGhostCells),
		mEnergyFluxes(numX, numY, numZ, numGhostCells),
		mEosProps(numX, numY, numZ, numGhostCells),
		mCellDiagonal(cellDiagonal),
		mGridInitialised(false),
		mFluxesSet(false) {}
		
	void
	EulerianGrid::
	setIdx(
		const std::array<int, 3>& idx,
		const double& density,
		const double& pressure,
		const double& internalEnergy,
		const std::array<double, 3>& velocity
		)
	{
		mDensities(idx) = density;
		mPressures(idx) = pressure;
		mInternalEnergies(idx) = internalEnergy;
		mVelocities(idx) = velocity;
	}
		
 }
