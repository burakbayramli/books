/*
Author: Rohan Ramasamy
Data: 31/03/16
*/

#include <vof/data/geom/Vector.h>
#include <vof/data/geom/Point.h>

#include <stdexcept>
#include <array>


namespace vof {
template<int DIM>
Vector<DIM>::
Vector() 
{
	mPoints.fill(0);
}

template<int DIM>
Vector<DIM>::
Vector(
	const double& x)
{
	for (int i = 0; i < DIM; ++i) {
		mPoints[i] = x;
	}
}

template<int DIM>
Vector<DIM>::
Vector(
	const Point<DIM>& point)
{
	for (int i = 0; i < DIM; ++i) {
		mPoints[i] = point[i];
	}
}

template<int DIM>
Vector<DIM>::
Vector(
	const Vector<DIM>& vector
	)
{
	for (int i = 0; i < DIM; ++i) {
		mPoints[i] = vector[i];
	}
}

template<int DIM>
double
Vector<DIM>::
x() const
{
	return mPoints[0];
}

template<int DIM>
double
Vector<DIM>::
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
Vector<DIM>
Vector<DIM>::
operator+(
	const Vector<DIM>& addedVector) const
{
	Vector<DIM> returnedVector;
	for(int i = 0; i < DIM; ++i) {
		returnedVector.mPoints[i] = mPoints[i] + addedVector.mPoints[i];
	}
	return returnedVector;
}

template<int DIM>
Vector<DIM>
Vector<DIM>::
operator-(
	const Vector<DIM>& subtractedVector) const
{
	Vector<DIM> returnedVector;
	for(int i = 0; i < DIM; ++i) {
		returnedVector.mPoints[i] = mPoints[i] - subtractedVector.mPoints[i];
	}
	return returnedVector;
}

template<int DIM>
Vector<DIM>
Vector<DIM>::
operator*(
	const double& multiplier) const
{
	Vector<DIM> returnedVector;
	for (int i = 0; i < DIM; ++i) {
		returnedVector.mPoints[i] = mPoints[i] * multiplier;
	}
	return returnedVector;
}

template<int DIM>
double
Vector<DIM>::
dot(
	const Vector<DIM>& dottedVector) const
{
	double sum = 0;
	for (int i = 0; i < DIM; ++i) {
		sum += mPoints[i] * dottedVector.mPoints[i];
	}	
	return sum;
}

template class Vector<1>;
template class Vector<2>;
template class Vector<3>;
} // namespace vof
