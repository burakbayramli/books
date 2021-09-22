/*
Author: Rohan Ramasamy
Data: 31/03/16
*/

#include <gtest/gtest.h>

#include <vof/data/geom/Point.h>
#include <vof/data/geom/Vector.h>


namespace vof {

// The fixture for testing class Foo.
class PointTest : public ::testing::Test {
 protected:
  // You can remove any or all of the following functions if its body
  // is empty.

  PointTest() {

  }

  virtual ~PointTest() {
    // You can do clean-up work that doesn't throw exceptions here.
  }

  // If the constructor and destructor are not enough for setting up
  // and cleaning up each test, you can define the following methods:

  virtual void SetUp() {
    // Code here will be called immediately after the constructor (right
    // before each test).
  }

  virtual void TearDown() {
    // Code here will be called immediately after each test (right
    // before the destructor).
  }

};

TEST_F(PointTest, DefaultConstructor) {
	Point<3> defaultPoint;


	EXPECT_EQ(0.0, defaultPoint.x());
	EXPECT_EQ(0.0, defaultPoint.y()); 
	EXPECT_EQ(0.0, defaultPoint.z());
}

TEST_F(PointTest, 1DConstructorAndIndexing) {
  Point<1> oneDPoint(1.0);
  EXPECT_EQ(1.0, oneDPoint.x());
  EXPECT_EQ(1.0, oneDPoint[0]);

  // Failed Indexing
  EXPECT_ANY_THROW(oneDPoint[1]);
  EXPECT_ANY_THROW(oneDPoint[2]);
  EXPECT_ANY_THROW(oneDPoint[-1]);
}

TEST_F(PointTest, 2DConstructorsAndIndexing) {

  	  Point<2> twoDPoint(1.0, 2.0);
	  EXPECT_EQ(1.0, twoDPoint.x());
	  EXPECT_EQ(2.0, twoDPoint.y()); 
	  EXPECT_EQ(1.0, twoDPoint[0]);
	  EXPECT_EQ(2.0, twoDPoint[1]); 


	  // Failed Indexing
	  EXPECT_ANY_THROW(twoDPoint[2]);
	  EXPECT_ANY_THROW(twoDPoint[-1]);
}

TEST_F(PointTest, 3DConstructorsAndIndexing) {
  	  Point<3> threeDPoint(1.0, 2.0, 3.0);
	  EXPECT_EQ(1.0, threeDPoint.x());
	  EXPECT_EQ(2.0, threeDPoint.y()); 
	  EXPECT_EQ(3.0, threeDPoint.z());
	  EXPECT_EQ(1.0, threeDPoint[0]);
	  EXPECT_EQ(2.0, threeDPoint[1]); 
	  EXPECT_EQ(3.0, threeDPoint[2]);

	  EXPECT_ANY_THROW(threeDPoint[3]);
	  EXPECT_ANY_THROW(threeDPoint[-1]);
}

TEST_F(PointTest, ArithmeticOperators) {
  	Point<3> threeDPoint(1.0, 2.0, 3.0);
	Vector<3> vectorTest(1.0);
	Point<3> pointTest(1.0);

	Point<3> addedPoint = threeDPoint + vectorTest;
	Point<3> subtractedPoint = threeDPoint - vectorTest;
	Vector<3> subtractedVector = threeDPoint - pointTest;

	EXPECT_EQ(2.0, addedPoint.x());
	EXPECT_EQ(3.0, addedPoint.y());
	EXPECT_EQ(4.0, addedPoint.z());

	EXPECT_EQ(0.0, subtractedPoint.x());
	EXPECT_EQ(1.0, subtractedPoint.y());
	EXPECT_EQ(2.0, subtractedPoint.z());

	EXPECT_EQ(0.0, subtractedVector.x());
	EXPECT_EQ(1.0, subtractedVector.y());
	EXPECT_EQ(2.0, subtractedVector.z());	
}

TEST_F(PointTest, Origin) {
	Point<3> PointOne(1.0, 2.0, 4.0);
	Vector<3> VectorOne = PointOne.origin();

	EXPECT_EQ(1.0, VectorOne.x());
	EXPECT_EQ(2.0, VectorOne.y());
	EXPECT_EQ(4.0, VectorOne.z());
}

} // namespace
