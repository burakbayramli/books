/*
Author: Rohan Ramasamy
Data: 31/03/16

Comments: This class is used to represent vectors in 1-3 dimenstions.
*/

#include <array>

#include <vof/data/geom/Point_fwd.h>


namespace vof {
template<int DIM>
class Vector
{
public:
	friend class Point<DIM>;

	/// Default Constructor
	Vector();

	/// 1D Constructor
	Vector(
		const double& x);

	/// 2D Constructor
	template<int D = DIM, typename = typename std::enable_if<D == 2>::type >
	Vector(
		const double& x,
		const double& y)
	{
		mPoints[0] = x;
		mPoints[1] = y;
	}

	/// 3D Constructor
	template<int D = DIM, typename = typename std::enable_if<D == 3>::type >
	Vector(
		const double& x,
		const double& y,
		const double& z)
	{
		mPoints[0] = x;
		mPoints[1] = y;
		mPoints[2] = z;
	}

	/// Point Constructor
	Vector(
		const Point<DIM>& point
		);
		
	/// Point Constructor
	Vector(
		const Vector<DIM>& vector
		);

	/// Returns x-component
	double
	x() const;

	/// Returns y-component
	template<int D = DIM, typename = typename std::enable_if<D >= 2>::type >
	double
	y() const
	{
		return mPoints[1];
	}

	/// Returns z-component
	template<int D = DIM, typename = typename std::enable_if<D == 3>::type >
	double
	z() const
	{
		return mPoints[2];
	}

	/// Returns component in indexed direction
	double
	operator[](
		const int& i) const;

	/// Vector Addition
	Vector<DIM>
	operator+(
		const Vector<DIM>& addedVector) const;

	/// Vector Subtraction
	Vector<DIM>
	operator-(
		const Vector<DIM>& subtractedVector) const;

	/// Righthand multiplication by real number
	Vector<DIM>
	operator*(
		const double& multiplier) const;

	/// Vector dot product
	double 
	dot(
		const Vector<DIM>& dottedVector) const;
	
	template<int D = DIM, typename = typename std::enable_if<D == 2>::type >
	double
	cross(
		const Vector<DIM> crossedVector) const
	{
		return mPoints[0] * crossedVector.mPoints[1] - mPoints[1] * crossedVector.mPoints[0];
	}

	template<int D = DIM, typename = typename std::enable_if<D == 3>::type >
	Vector<DIM> 
	cross(
		const Vector<DIM> crossedVector) const
	{
		double x = mPoints[1] * crossedVector.mPoints[2] - mPoints[2] * crossedVector.mPoints[1];
		double y = mPoints[2] * crossedVector.mPoints[0] - mPoints[0] * crossedVector.mPoints[2];
		double z = mPoints[0] * crossedVector.mPoints[1] - mPoints[1] * crossedVector.mPoints[0];
		Vector<3> returnedVector(x, y, z);

		return returnedVector;
	}

private:
	std::array<double, DIM> mPoints;
};

/// Lefthand multiplication by real number
template<int DIM>
Vector<DIM>
operator*(
	const double& multiplier,
	const Vector<DIM>& multipliedVector)
{
	return multipliedVector * multiplier;
}
} // namespace vof
