function PSI = ascent_bcs_tf(Y0,Yf,tf)
%
% Boundary Condition Function for the Flat-Moon Optimal Ascent Problem
%
%
% pass in values of boundary conditions as global variables
global x0 y0 Vx0 Vy0 yf Vxf Vyf
% Create a column vector with the 7 boundary conditions on the state &
% costate variables (the 8th boundary condition, t0 = 0, is not used).
PSI = [ Y0(1) - x0
	Y0(2) - y0
	Y0(3) - Vx0
	Y0(4) - Vy0
	Yf(2) - yf
	Yf(3) - Vxf
	Yf(4) - Vyf
];
return


