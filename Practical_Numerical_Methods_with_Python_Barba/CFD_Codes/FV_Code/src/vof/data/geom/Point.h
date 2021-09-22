/*
Author: Rohan Ramasamy
Data: 31/03/16

Comments: This class is designed to represent a point in 1, 2 or 3 dimensions. It should be 
able to interact with the vector class in the typical ways for geometric function
*/

#pragma once

#include <array>

#include <vof/data/geom/Vector_fwd.h>


namespace vof {
template <int DIM>
class Point
{
public:
	/// Default Constructor
	Point();

	/// 1D Constructor	
	Point(
		const double& x);

	/// 2D Constructor
	template<int D = DIM, typename = typename std::enable_if<(D >= 2)>::type >
	Point(
		const double& x,
		const double& y)
	{
		mPoints[0] = x;
		mPoints[1] = y;
	}

	/// 3D Constructor
	template<int D = DIM, typename = typename std::enable_if<(D == 3)>::type >
	Point(
		const double& x,
		const double& y,
		const double& z)
	{
		mPoints[0] = x;
		mPoints[1] = y;
		mPoints[2] = z;
	}

	/// Return x-component
	double
	x() const;

	/// Return y-component
	template<int D = DIM, typename = typename std::enable_if<(D >= 2)>::type >
	double
	y() const
	{
		return mPoints[1];
	}

	/// Return z-component
	template<int D = DIM, typename = typename std::enable_if<(D == 3)>::type >
	double
	z() const
	{
		return mPoints[2];
	}

	/// Return i-th component
	double
	operator[](
		const int& i) const;

	/// Vector Addition
	Point<DIM>
	operator+(
			const Vector<DIM>& addedVector) const;

	/// Vector Subtraction
	Point<DIM>
	operator-(
			const Vector<DIM>& subractedVector) const;
	
	/// Return the distance between two points
	Vector<DIM>
	operator-(
			const Point<DIM>& secondPoint) const;

	/// Distance to origin
	Vector<DIM>
	origin() const;
private:
	std::array<double, DIM> mPoints;
};
}
