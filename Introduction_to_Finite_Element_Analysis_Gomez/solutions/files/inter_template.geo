// Parameters
L = 2.0;
H = 1.0;
h_1 = H/2;
h_2 = H/2;
lado_elem = 0.1;


// Points
Point(1) = {0.0, 0.0, 0, lado_elem};
Point(2) = {L, 0.0, 0, lado_elem};
Point(3) = {L, h_2, 0, lado_elem};
Point(4) = {L, H, 0, lado_elem};
Point(5) = {0, H, 0, lado_elem};
Point(6) = {0, h_2, 0, lado_elem};

// Lines
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 5};
Line(5) = {5, 6};
Line(6) = {6, 1};
Line(7) = {3, 6};

// Surfaces
Line Loop(1) = {1, 2, 7, 6};
Line Loop(2) = {-7, 5, 4, 3};

Plane Surface(1) = {1};
Plane Surface(2) = {2};

// Physical groups
Physical Surface(100) = {1};  // Material superior
Physical Surface(200) = {2};  // Material inferior
