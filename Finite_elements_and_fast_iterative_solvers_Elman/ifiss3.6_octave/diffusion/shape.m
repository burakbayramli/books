      function [phi,dphids,dphidt] = shape(s,t)
%SHAPE evaluates bilinear shape functions 
%   [phi,dphids,dphidt] = shape(s,t);
%   input
%          s         x coordinate   
%          t         y coordinate
%   output
%          phi        shape function
%          dphids     x derivative of phi
%          dphidt     y derivative of phi
%
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      one = 1.0e0;
%
      phi(1) = 0.25 * (s - one) * (t - one);
      phi(2) = -.25 * (s + one) * (t - one);
      phi(3) = 0.25 * (s + one) * (t + one);
      phi(4) = -.25 * (s - one) * (t + one);
      dphids(1) = 0.25 * (t - one);
      dphids(2) = -.25 * (t - one);
      dphids(3) = 0.25 * (t + one);
      dphids(4) = -.25 * (t + one);
      dphidt(1) = 0.25 * (s - one);
      dphidt(2) = -.25 * (s + one);
      dphidt(3) = 0.25 * (s + one);
      dphidt(4) = -.25 * (s - one);
      return
