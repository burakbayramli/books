function [z,d] = newtonstep(Q,B,u,y,n,n1);
% function newton fuer cont

I = 1:n;
C = B(I,:);
D = C.';
v = D\y;
v = [v;0];
v = Q*v;
z = u - v;
d = norm(v);
