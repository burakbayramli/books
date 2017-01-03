% [Sol, H] = homography2Motion(p, q)

% Homography estimation between views of a plane
% decompose it into rotation and translation and plane normal
% p - retinal (calibrated) coordinates of the first view
% q - retinal (calibrated) coordinate in the second view
% Sol = [rotation, translation*1/d, plane normal] four possible solutions
% H - two view homography
%
% Algorith, 5.2 in Chapter 5, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
%
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003
%
% Last modified 5/5/2003

function [Sol, H]  = homography2Motion(p, q);

NPOINTS = size(p,2);
B = [];
for i = 1:NPOINTS
     B = [B; makeRows(p(:,i), q(:,i))];
end;

[u,s,v] = svd(B);
h = v(:,9);
Hest = transpose(reshape(h,[3,3]));

 % decomposition of H into motion and structure in case of calibration case
 [u,s,v] = svd(Hest);
 H = Hest/s(2,2);
 [u,s,v] = svd(H'*H);
 
 if det(u) < 0 u = -u; end;
 
 s1 = s(1,1); s2 = s(2,2); s3 = s(3,3);
 v1 = u(:,1); v2 = u(:,2); v3 = u(:,3);
 u1 = (v1*sqrt(1-s3) + v3*sqrt(s1 -1))/sqrt(s1 - s3);
 u2 = (v1*sqrt(1-s3) - v3*sqrt(s1 -1))/sqrt(s1 - s3);
 
 
 U1 = [v2, u1, skew(v2)*u1];
 U2 = [v2, u2, skew(v2)*u2];
 W1 = [H*v2, H*u1, skew(H*v2)*H*u1];
 W2 = [H*v2, H*u2, skew(H*v2)*H*u2];
 
 N1 = skew(v2)*u1;
 N2 = skew(v2)*u2;
 
 Sol(:,:,1) = [W1*U1', (H - W1*U1')*N1, N1];
 Sol(:,:,2) = [W2*U2', (H - W2*U2')*N2, N2];
 Sol(:,:,3) = [W1*U1', -(H - W1*U1')*N1, -N1];
 Sol(:,:,4) = [W2*U2', -(H - W2*U2')*N2, -N2];