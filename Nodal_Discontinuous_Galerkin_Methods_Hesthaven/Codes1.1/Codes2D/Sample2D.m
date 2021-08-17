
function [sampleweights,sampletri] = Sample2D(xout, yout)
  
% function [sampleweights,sampletri] = Sample2D(xout, yout)
% purpose: input = coordinates of output data point
%          output = number of containing tri and interpolation weights
% [ only works for straight sided triangles ]

Globals2D;

% find containing tri
[sampletri,tribary] = ...
    tsearchn([VX', VY'], EToV, [xout,yout]);

% Matlab barycentric coordinates -> biunit triangle coordinates
sout = 2*tribary(:,3)-1; rout = 2*tribary(:,2)-1;

% build generalized Vandermonde for the sample point
Vout = Vandermonde2D(N, rout, sout);
     
% build interpolation matrix for the sample point
sampleweights = Vout*invV;
return;
