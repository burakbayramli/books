function yp = lo3(t,y)
%
% Linear o.d.e. system in 3D
%
%   See also GO3

global Ao bo

yp = Ao * [y(1);y(2);y(3)] - bo;

