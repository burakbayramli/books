function x=geornd(p)
% generates from Geom(p) distribution using exponential r.v.
% implements Algorithm 4.8;
% the parameter p can be a scalar or an array of any size;

x=ceil(log(rand(size(p)))./log(1-p));