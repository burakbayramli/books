/**
 * Author: Rohan Ramasamy
 * Date: 05/02/2017
 */
 
 #include <vof/data/grid/kernel/StateGrid.h>
 #include <vof/data/eos/eosProperties/IdealEquationOfStateProperties.h>
 
 
 namespace vof {
	 template<typename T>
	 StateGrid<T>::
	 StateGrid(
		const int& numI,
		const int& numJ,
		const int& numK,
		const int& numGhostCells
		) :
		mNumI(numI),
		mNumJ(numJ),
		mNumK(numK),
		mNumGhostCells(numGhostCells)
	{
		int boundaryCells = numGhostCells * 2;
		mStates.resize((numI + boundaryCells) *
		               (numJ + boundaryCells) * 
		               (numK + boundaryCells));
	}
	
	template<typename T>
	const T&
	StateGrid<T>::
	operator () (
		const int& i,
		const int& j,
		const int& k
		) const
	{
		int stateIdx = getIdx(i, j, k);
		return mStates[stateIdx];
	}
	
	template<typename T>
	const T&
	StateGrid<T>::
	operator ()(
		const std::array<int, 3>& idx
		) const
	{
		return this->operator()(idx[0], idx[1], idx[2]);
	}
	
	template<typename T>
	T&
	StateGrid<T>::
	operator () (
		const std::array<int, 3>& idx
		)
	{
		int stateIdx = getIdx(idx);
		return mStates[stateIdx];
		
	}
	
	template<typename T>
	T&
	StateGrid<T>::
	operator () (
		const int& i,
		const int& j,
		const int& k
		)
	{
		int stateIdx = getIdx(i, j, k);
		return mStates[stateIdx];
	}
     
    template<typename T>
    const T&
    StateGrid<T>::
    getNeighbour(
        const int& i,
        const int& j,
        const int& k,
        const int& dim
        )
     {
         assert(1 <= dim <= 3);
         
         int stateIdx;
         if (dim == 0) {
             stateIdx = getIdx(i + 1, j, k);
         }
         else if (dim == 1) {
             stateIdx = getIdx(i, j + 1, k);
         }
         else {
             stateIdx = getIdx(i, j, k + 1);
         }
         return mStates[stateIdx];
     }
     
	
 template class StateGrid<double>;
 template class StateGrid<std::array<double, 3> >;
 template class StateGrid<std::array<std::array<double, 3>, 3> >;
 template class StateGrid<IdealEquationOfStateProperties>;
 }
