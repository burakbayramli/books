function [q,g,h,r] = bsp01hw(p,e,u,time)
% Randbedingungen fuer w
N = size(e,2);  q = zeros(1,N);
g = zeros(1,N); h = ones(1,2*N);
r = 0*ones(1,2*N);
