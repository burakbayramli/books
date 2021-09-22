/*
Author: Rohan Ramasamy
Data: 31/03/16
*/

#include <gtest/gtest.h>

#include <vof/data/geom/Vector.h>


namespace vof {
	
class VectorTest : public ::testing::Test {};

TEST_F(VectorTest, DefaultConstructor) {
	Vector<3> defaultVector;


	EXPECT_EQ(0.0, defaultVector.x());
	EXPECT_EQ(0.0, defaultVector.y()); 
	EXPECT_EQ(0.0, defaultVector.z());
}

TEST_F(VectorTest, 1DConstructorAndIndexing) {
  Vector<1> oneDVector(1.0);
  EXPECT_EQ(1.0, oneDVector.x());
  EXPECT_EQ(1.0, oneDVector[0]);

  // Failed Indexing
  EXPECT_ANY_THROW(oneDVector[1]);
  EXPECT_ANY_THROW(oneDVector[2]);
  EXPECT_ANY_THROW(oneDVector[-1]);
}

TEST_F(VectorTest, 2DConstructorsAndIndexing) {

  	  Vector<2> twoDVector(1.0, 2.0);
	  EXPECT_EQ(1.0, twoDVector.x());
	  EXPECT_EQ(2.0, twoDVector.y()); 
	  EXPECT_EQ(1.0, twoDVector[0]);
	  EXPECT_EQ(2.0, twoDVector[1]); 


	  // Failed Indexing
	  EXPECT_ANY_THROW(twoDVector[2]);
	  EXPECT_ANY_THROW(twoDVector[-1]);
}

TEST_F(VectorTest, 3DConstructorsAndIndexing) {
  	  Vector<3> threeDVector(1.0, 2.0, 3.0);
	  EXPECT_EQ(1.0, threeDVector.x());
	  EXPECT_EQ(2.0, threeDVector.y()); 
	  EXPECT_EQ(3.0, threeDVector.z());
	  EXPECT_EQ(1.0, threeDVector[0]);
	  EXPECT_EQ(2.0, threeDVector[1]); 
	  EXPECT_EQ(3.0, threeDVector[2]);

	  EXPECT_ANY_THROW(threeDVector[3]);
	  EXPECT_ANY_THROW(threeDVector[-1]);
}

TEST_F(VectorTest, ArithmeticOperators) {
  	Vector<3> threeDVector(1.0, 2.0, 3.0);
	Vector<3> vectorTest(1.0);
	Vector<3> addedVector = threeDVector + vectorTest;
	Vector<3> subtractedVector = threeDVector - vectorTest;
	Vector<3> multipliedVector = threeDVector * 2.0; 
	Vector<3> friendMultipliedVector = 2.0 * threeDVector;

	EXPECT_EQ(2.0, addedVector.x());
	EXPECT_EQ(3.0, addedVector.y());
	EXPECT_EQ(4.0, addedVector.z());

	EXPECT_EQ(0.0, subtractedVector.x());
	EXPECT_EQ(1.0, subtractedVector.y());
	EXPECT_EQ(2.0, subtractedVector.z());

	EXPECT_EQ(2.0, multipliedVector.x());
	EXPECT_EQ(4.0, multipliedVector.y());
	EXPECT_EQ(6.0, multipliedVector.z());

	EXPECT_EQ(2.0, friendMultipliedVector.x());
	EXPECT_EQ(4.0, friendMultipliedVector.y());
	EXPECT_EQ(6.0, friendMultipliedVector.z());
}

TEST_F(VectorTest, dotProduct) {
	Vector<1> oneDVector(1.0);
  	Vector<2> twoDVector(1.0, 2.0);
  	Vector<3> threeDVector(1.0, 2.0, 3.0);
	
	Vector<1> vectorTest1D(1.0);
	Vector<2> vectorTest2D(2.0);
	Vector<3> vectorTest3D(3.0); 

	double dot1D = oneDVector.dot(vectorTest1D);
	double dot2D = vectorTest2D.dot(twoDVector);
	double dot3D = threeDVector.dot(vectorTest3D);

	EXPECT_EQ(1.0, dot1D);
	EXPECT_EQ(6.0, dot2D);
	EXPECT_EQ(18.0, dot3D) << vectorTest3D.x() << " " << vectorTest3D.y() << " " << vectorTest3D.z() <<
							threeDVector.x() << " " << threeDVector.y() << " " << threeDVector.z();
}

TEST_F(VectorTest, 2DcrossProduct) {
	Vector<2> xAxis(1.0, 0.0);
	Vector<2> yAxis(0.0, 1.0);
	Vector<2> zeroVector(0.0, 0.0);

	double crossProduct = xAxis.cross(yAxis);
	double secondCrossProduct = yAxis.cross(xAxis);
	double zeroCrossProduct = zeroVector.cross(xAxis);

	EXPECT_EQ(1.0, crossProduct);
	EXPECT_EQ(-1.0, secondCrossProduct);
	EXPECT_EQ(0.0, zeroCrossProduct);
}

TEST_F(VectorTest, 3DcrossProduct) {
	Vector<3> xAxis(1.0, 0.0, 0.0);
	Vector<3> yAxis(0.0, 1.0, 0.0);
	Vector<3> zAxis(0.0, 0.0, 1.0);

	Vector<3> crossProduct = xAxis.cross(yAxis);
	Vector<3> secondCrossProduct = yAxis.cross(xAxis);

	EXPECT_EQ(0.0, crossProduct.x());
	EXPECT_EQ(0.0, crossProduct.y());
	EXPECT_EQ(1.0, crossProduct.z());
	EXPECT_EQ(0.0, secondCrossProduct.x());
	EXPECT_EQ(0.0, secondCrossProduct.y());
	EXPECT_EQ(-1.0, secondCrossProduct.z());
}

} // namespace
