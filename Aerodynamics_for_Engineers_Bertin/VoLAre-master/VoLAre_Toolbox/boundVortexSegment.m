% Function to calculate induced velocity of a unit vortex strength (\Gamma = 1) bound vertex segment
% Anthony Ricciardi
%
% Inputs
% A = [3,1] or [1,3] segment point one
% B = [3,1] or [1,3] segment point two
% C = [3,1] or [1,3] point where induced velocity is calculated
%
% Outputs
% V = [3,1] induced velocity
%
function V = boundVortexSegment(A,B,C)
r0 = [B(1)-A(1);B(2)-A(2);B(3)-A(3)];
r1 = [C(1)-A(1);C(2)-A(2);C(3)-A(3)];
r2 = [C(1)-B(1);C(2)-B(2);C(3)-B(3)];
r1Xr2 = [r1(2)*r2(3); r1(3)*r2(1); r1(1)*r2(2)]-[r1(3)*r2(2); r1(1)*r2(3); r1(2)*r2(1)]; %% cheaper 3x3 Cross product
n_r1Xr2_2 = r1Xr2.'*r1Xr2;
if sqrt(n_r1Xr2_2) < 1e-8
	V = [0;0;0];
else
	V = (.25/pi)* (r1Xr2./n_r1Xr2_2)* (r0'*(r1./sqrt(r1.'*r1))-r0'*(r2./sqrt(r2.'*r2)));
end
end