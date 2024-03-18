% Function to calculate induced velocity of a unit vortex strength (\Gamma = 1) trailing vertex segment
% Anthony Ricciardi
%
% Inputs
% A = [3,1] or [1,3] segment start point
% C = [3,1] or [1,3] point where induced velocity is calculated
%
% Outputs
% V = [3,1] induced velocity
%
function V = trailingVortexSegment(A,C)
r = [0;C(3)-A(3);C(2)-A(2)];
n_r_2 = r.'*r;
if n_r_2 < 1e-8
	V = [0;0;0];
else
	V = (.25/pi)*(r./n_r_2)*( 1 + ( C(1)-A(1) )/sqrt( (C(1)-A(1))^2 + (C(2)-A(2))^2 + (C(3)-A(3))^2) );
end
end