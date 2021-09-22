/**
 * Author: Rohan Ramasamy
 * Date: 05/02/2017
 */

#pragma once

#include <cassert>
#include <vector>
#include <array>


namespace vof {
	template<typename T>
	class StateGrid 
	{
	public:
		/**
		 * Constructor - always constructs a 3D array
		 */
		StateGrid(
			const int& numI,
			const int& numJ = 1,
			const int& numK = 1,
			const int& numGhostCells=1
			);
		
		/**
		 * Accessors to grid data
		 */
	    const T&
		operator () (
			const int& i,
			const int& j = 0,
			const int& k = 0
			) const;
		
		const T&
		operator () (
			const std::array<int, 3>& idx
			) const;
		
		/**
		 * Convert 3D index into vector index
		 */
		inline int
		getIdx(
			const int& i,
			const int& j = 0,
			const int& k = 0
			) const
		{
			bool xValid = i < mNumI + mNumGhostCells && i >= -mNumGhostCells;
			bool yValid = j < mNumJ + mNumGhostCells && j >= -mNumGhostCells;
			bool zValid = k < mNumK + mNumGhostCells && k >= -mNumGhostCells;
			if (!xValid || !yValid || !zValid) {
				throw std::runtime_error("Invalid index!");
			}
			
			return (i + mNumGhostCells) + (j + mNumGhostCells) * (mNumI + 2 * mNumGhostCells) + \
					(k + mNumGhostCells) * (mNumI + 2 * mNumGhostCells) * (mNumJ + 2 * mNumGhostCells);
		};
		
		inline int
		getIdx(
			std::array<int, 3> idx
			) const
		{
			return getIdx(idx[0], idx[1], idx[2]);
		}
		
        /**
         * Get the neighbour cell to the specified index in the specified direction
         */
        const T&
        getNeighbour(
            const int& i,
            const int& j,
            const int& k,
            const int& dim
            );
		
	private:
		std::vector<T> mStates;
		int mNumI;
		int mNumJ;
		int mNumK;
		int mNumGhostCells;
		
		// Allow EulerianGrid class access to private members for changing
		// grid state
		friend class EulerianGrid;
		template<int DIM> friend class FluxCalculatorGodunovImpl;
		
		// Accessor for setting state
		T&
		operator () (
			const std::array<int, 3>& idx
			);
			
		T&
		operator () (
			const int& i,
			const int& j,
			const int& k
			);
	};
}
