/*----------------------------------------------------------------------------

2D Physics Test Program - a cheesy test harness for 2D physics

by Chris Hecker for my Game Developer Magazine articles.  See my homepage
for more information.

NOTE: This is a hacked test program, not a nice example of Windows programming.
physics.cpp the only part of this you should look at!!!

This material is Copyright 1997 Chris Hecker, All Rights Reserved.
It's for you to read and learn from, not to put in your own articles
or books or on your website, etc.  Thank you.

Chris Hecker
checker@d6.com
http://www.d6.com/users/checker

*/

/*----------------------------------------------------------------------------

Interface functions between physics code and the dumb OS interface.

*/

// physics -> app

void Line( int X0, int Y0, int X1, int Y1 );
float GetTime( void );

extern int WorldWidth, WorldHeight;

// app -> physics

void Run( void );

void ToggleWorldSpring( void );
void ToggleBodySpring( void );
void ToggleGravity( void );
void ToggleDamping( void );

