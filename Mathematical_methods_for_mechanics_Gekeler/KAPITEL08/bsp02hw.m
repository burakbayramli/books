function [q,g,h,r] = bsp02hw(p,e,u,time)
% Randbedingungen I fuer w
N = size(e,2);  q = zeros(1,N);
g = zeros(1,N); h = ones(1,2*N);
r = -4*ones(1,2*N);
