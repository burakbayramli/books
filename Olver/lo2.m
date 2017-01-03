function yp = lo2(t,y)
%
% Linear o.d.e. system in 2D
%
%   See also GO2

global Ao bo

yp = Ao * [y(1);y(2)] - bo;

