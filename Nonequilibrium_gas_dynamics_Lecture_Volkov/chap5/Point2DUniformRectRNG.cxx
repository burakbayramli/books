////////////////////////////////////////////////////////////////////////////////////////////////////
// Standard generator of 2D discrete random point with uniform distrubution in a rectangle
//--------------------------------------------------------------------------------------------------
// ME 491/591 Non-equilibrium gas dynamics, Spring 2017
// Alexey N. Volkov, Univesity of Alabama, avolkov1@ua.edu
////////////////////////////////////////////////////////////////////////////////////////////////////

	void v2rand_uniform_rect ( double &X, double &Y, double a, double b, double c, double d ) //
        // (X,Y) are Cartesian coordinates of a point inside the rectangle [a,b]x[c,d]
	{ //////////////////////////////////////////////////////////////////////////////////////////	
                X = a + ( b - a ) * brng ();
                Y = c + ( d - c ) * brng ();
	} //////////////////////////////////////////////////////////////////////////////////////////
