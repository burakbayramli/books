function [x, y, z] = enu2xyz(phi,lambda,e,n,u)
%ENU2XYZ  Transformation of [e;n;u] vector from local to geocentric
%   	    system. The local system has origin at (phi, lambda)

%Kai Borre 03-31-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

dtr = pi/180;
cl = cos(lambda*dtr);  sl = sin(lambda*dtr);
cb = cos(phi*dtr);	  sb = sin(phi*dtr);
F = [-sl -sb*cl cb*cl;
      cl -sb*sl cb*sl;
      0	  cb      sb];
global_vector = F*[e; n; u];
x = global_vector(1);
y = global_vector(2);
z = global_vector(3);
%%%%%%%%% end enu2xyz.m %%%%%%%%%
