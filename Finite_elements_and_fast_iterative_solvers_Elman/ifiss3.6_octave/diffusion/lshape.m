      function [chi,dchids,dchidt] = lshape(s,t)
%LSHAPE evaluates linear shape functions 
%   [chi,dchids,dchidt] = lshape(s,t);
%   input
%          s         x coordinate   
%          t         y coordinate
%   output
%          chi        shape function
%          dchids     x derivative of chi
%          dchidt     y derivative of chi
%
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      one = 1.0e0; zero=0.0e0;
% 
      chi(1) = one;
      chi(2) = s;
	  chi(3) = t;
      dchids(1) = zero;
      dchids(2) = one;
      dchids(3) = zero;
      dchidt(1) = zero;
      dchidt(2) = zero;
      dchidt(3) = one;
      return
