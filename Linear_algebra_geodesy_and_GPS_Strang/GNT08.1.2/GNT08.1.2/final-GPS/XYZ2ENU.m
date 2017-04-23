 % convert ECEF coordinates to local East, North, Up 
%****************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/21/2008                *
% Email: moeinmehrtash@yahoo.com                                            *
%****************************************************************************
% Reference:"GPS Theory and application",edited by B.Parkinson,J.Spilker,   *
%****************************************************************************           
function ENU= XYZ2ENU(A,Phi,Lambda) 
  XYZ2ENU=[-sin(Lambda) cos(Lambda) 0;
           -sin(Phi)*cos(Lambda) -sin(Phi)*sin(Lambda) cos(Phi);
           cos(Phi)*cos(Lambda) cos(Phi)*sin(Lambda)  sin(Phi)];
 
  ENU=XYZ2ENU*A';       

  