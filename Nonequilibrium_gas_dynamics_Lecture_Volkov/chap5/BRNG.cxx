////////////////////////////////////////////////////////////////////////////////////////////////////
// Pseudo-random nunmber generator based on the MWC (Multiply With Carry) algorithm proposed by 
// George Marsaglia.
//--------------------------------------------------------------------------------------------------
// ME 491/591 Non-equilibrium gas dynamics, Spring 2017
// Alexey N. Volkov, Univesity of Alabama, avolkov1@ua.edu
////////////////////////////////////////////////////////////////////////////////////////////////////

        // Global variables containing current state of the generator
        // Values here correspond to the default state

        unsigned int m_u = 521288629, m_v = 362436069;

        void SetSeed ( unsigned int u, unsigned int v ) ////////////////////////////////////////////
        { 
	        m_u = u; 
	        m_v = v; 
        } //////////////////////////////////////////////////////////////////////////////////////////

        unsigned int IntUniform ( unsigned int& u, unsigned int& v ) ///////////////////////////////
        {
	        v = 36969*(v & 65535) + (v >> 16);
                u = 18000*(u & 65535) + (u >> 16);
                return (v << 16) + u;
        } //////////////////////////////////////////////////////////////////////////////////////////
        
        double brng () /////////////////////////////////////////////////////////////////////////////
        {
        unsigned int z = IntUniform ( m_u, m_v );
	        return z*2.328306435996595e-10; 
        } ///////////////////////////////////////////////////////////////////////////////////////////