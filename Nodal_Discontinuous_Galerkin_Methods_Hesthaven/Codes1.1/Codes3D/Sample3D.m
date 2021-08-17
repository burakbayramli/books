function [sampleweights,sampletet] = Sample3D(xout, yout, zout)
  
% function [sampleweights,sampletet] = Sample3D(xout, yout, zout)
% purpose: input = coordinates of output data point
%          output = number of containing tet and interpolation weights

Globals3D;

% find containing tet
[sampletet,tetbary] = tsearchn([VX', VY', VZ'], EToV, [xout,yout,zout]);

% Matlab barycentric coordinates -> biunit triangle coordinates
tout = 2*tetbary(:,4)-1; sout = 2*tetbary(:,3)-1; rout = 2*tetbary(:,2)-1;

% build generalized Vandermonde for the sample point
Vout = Vandermonde3D(N, rout, sout, tout);
     
% build interpolation matrix for the sample point
sampleweights = Vout*invV;
return;
