////////////////////////////////////////////////////////////////////////////////////////////////////
// Standard generator of 2D discrete random point with uniform distrubution in a circle
//--------------------------------------------------------------------------------------------------
// ME 491/591 Non-equilibrium gas dynamics, Spring 2017
// Alexey N. Volkov, Univesity of Alabama, avolkov1@ua.edu
////////////////////////////////////////////////////////////////////////////////////////////////////

        void v2rand_circle ( double &X, double &Y, double a, double b, double R ) //////////////////
        // (X,Y) are Cartesian coordinates of a point inside the circle of radius R with the centre 
        // in point (a,b)
	{ //////////////////////////////////////////////////////////////////////////////////////////	
        double 	R1 = R * sqrt ( brng () );
        double 	E1 = 2.0 * M_PI * brng ();
	        X = a + R1 * cos ( E1 );
	        Y = b + R1 * sin ( E1 );
        } //////////////////////////////////////////////////////////////////////////////////////////
