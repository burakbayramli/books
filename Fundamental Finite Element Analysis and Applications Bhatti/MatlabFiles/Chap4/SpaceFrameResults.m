function [f, bmr, bms, bmt, Vr, Vs] = SpaceFrameResults(e, G, Ir, Is, J, A, qr, ...
    qs, coord, dn)
% [f, bmr, bms, bmt, Vr, Vs] = SpaceFrameResults(e, G, Ir, Is, J, A, qr, ...
%   qs, coord, dn)
% Computes results for a space frame element
% e = modulus of elasticity
% G = shear modulus
% Ir, Is = moment of inertias about element r and s axes
% J = torsional rigity
% A = area of cross-section
% qr, qs = distributed loads along the element r and s axes
% coord = coordinates at the element ends
% dn = nodal solution
% The output variables are
% f = axial force, bmr, bms = bending moments about r and s axes, 
%   bmt = twisting moment, Vr, Vs = shear forces about r and s axes.

EIr=e*Ir; EIs=e*Is; GJ=G*J; EA = e*A;
n1=coord(1,1:3); n2=coord(2,1:3); n3=coord(3,1:3);
L=sqrt(dot((n2-n1),(n2-n1)));
ex = (n2 - n1)/L;
eyy = cross(n3 - n1, n2 - n1);
ey = eyy/sqrt(dot(eyy,eyy));
ez = cross(ex, ey);
H = [ex; ey; ez];
T = zeros(12);
T([1, 2, 3], [1, 2, 3]) = H; 
T([4,5,6], [4,5,6]) = H;
T([7,8,9], [7,8,9]) = H;
T([10,11,12], [10,11,12]) = H;
TT = T';

dl = T*dn;
u = dl([1,7]); tw=dl([4,10]);
v = dl([2, 6, 8, 12]); w = dl([3, 5, 9, 11]);
f=[]; bmr=[]; bms=[]; bmt=[]; Vr=[]; Vs=[];
% Change increment to get results at more points
for s=0:L/2:L
    x = n1(1) + s*H(1,1); y = n1(2)+ s*H(1,2); z = n1(3)+ s*H(1,3);
    f = [f; [x,y,z, EA*(-u(1)+u(2))/L]];
    bmt = [bmt; [x,y,z, GJ*(-tw(1)+tw(2))/L]];
    dnv2 = [(12*s)/L^3 - 6/L^2, (6*s)/L^2 - 4/L, 6/L^2 - ...
            (12*s)/L^3, (6*s)/L^2 - 2/L];
    dnw2=[dnv2(1), -dnv2(2), dnv2(3), -dnv2(4)];
    bmr = [bmr; [x, y, z, EIr*dnv2*v+(qs*(L^2 - 6*s*L + ...
                6*s^2))/(12)]];
    bms = [bms; [x, y, z, -EIs*dnw2*w-(qr*(L^2 - 6*s*L + ...
                6*s^2))/(12)]];
    dnv3 = [12/L^3, 6/L^2, -(12/L^3), 6/L^2];
    Vs = [Vs; [x, y,z, EIr*dnv3*v+((qs*(12*s - 6*L))/(12))]];
    dnw3=[dnv3(1), -dnv3(2), dnv3(3), -dnv3(4)];
    Vr = [Vr; [x, y, z, EIs*dnw3*w+((qr*(12*s - 6*L))/(12))]];
end