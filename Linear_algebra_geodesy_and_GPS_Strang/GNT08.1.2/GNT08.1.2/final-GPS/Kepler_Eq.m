%Kepler Eq => M=E-e*sinE
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/28/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
function f=Kepler_Eq(x,M,e)
f=x-e*sin(x)-M;