/*
Author: Rohan Ramasamy
Data: 31/03/16
*/

#include <vof/data/geom/Point.h>
#include <vof/data/geom/Vector.h>

#include <stdexcept>
#include <array>


namespace vof {
template<int DIM>
Point<DIM>::
Point() 
{
	mPoints.fill(0);
}

template<int DIM>
Point<DIM>::
Point(
	const double& x)
{
	for (int i = 0; i < DIM; ++i) {		
		mPoints[i] = x;
	}
}

template<int DIM>
double
Point<DIM>::
x() const
{ 
	return mPoints[0];
}

template<int DIM>
double
Point<DIM>::
operator[](
	const int& i) const
{
	if (i >= 0 && i < DIM) {
		return mPoints[i];
	}
	else {
		throw std::invalid_argument("Invalid for point dimension");
	}
}

template<int DIM>
Point<DIM>
Point<DIM>::
operator+(
	const Vector<DIM>& addedVector) const
{
	Point<DIM> returnedPoint;
	for (int i = 0; i < DIM; ++i) {
		returnedPoint.mPoints[i] = mPoints[i] + addedVector[i];
	}
	return returnedPoint;
}


template<int DIM>
Point<DIM>
Point<DIM>::
operator-(
	const Vector<DIM>& subtractedVector) const
{
	Point<DIM> returnedPoint;
	for (int i = 0; i < DIM; ++i) {
		returnedPoint.mPoints[i] = mPoints[i] - subtractedVector[i];
	}
	return returnedPoint;	
}

template<int DIM>
Vector<DIM>
Point<DIM>::
operator-(
		const Point<DIM>& secondPoint) const
{
	Vector<DIM> returnedVector;
	for (int i = 0; i < DIM; ++i) {
		returnedVector.mPoints[i] = mPoints[i] - secondPoint.mPoints[i];
	}
	return returnedVector;
}

template<int DIM>
Vector<DIM>
Point<DIM>::
origin() const 
{
	return Vector<DIM>(*this);
}

template class Point<1>;
template class Point<2>;
template class Point<3>;
} // namespace vof
