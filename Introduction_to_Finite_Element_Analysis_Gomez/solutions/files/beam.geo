// Input .geo for beam under uniform load
// Author: Juan Gomez
c = 4.0;
l = 10.0;
//+
Point(1) = {-l/2 , -c , 0 , c/5};
Point(2) = { l/2 , -c , 0 , c/5};
Point(3) = { l/2 ,  c , 0 , c/5};
Point(4) = {-l/2 ,  c , 0 , c/5};
//
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line Loop(1) = {1, 2, 3, 4};
Plane Surface(1) = {1};
Physical Surface(100) = {1};
