function [E, N, U] = topo_enu(X,dx)
%TOPO_ENU  Transformation of vector dx into topocentric coordinate
%          system with origin at X.
%          Both parameters are 3 by 1 vectors.
%          Output: E    Easting
%                  N    Northing
%                  U    Upping

%Kai Borre 11-15-02
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2002/11/15  $

dtr = pi/180;
[phi,lambda,h] = togeod(6378137,298.257223563,X(1),X(2),X(3));
cl = cos(lambda*dtr); sl = sin(lambda*dtr);
cb = cos(phi*dtr); sb = sin(phi*dtr);
F = [-sl -sb*cl cb*cl;
      cl -sb*sl cb*sl;
       0    cb   sb];
local_vector = F'*dx;
E = local_vector(1);
N = local_vector(2);
U = local_vector(3);

%%%%%%%%% end topo_enu.m %%%%%%%%%
