function [q,g,h,r] = bsp03hv(p,e,w,time)
% Randbedingung r fuer Teilgleichung - Delta v = f
global nu, global t, global y1, global y2
N = size(e,2);  q = zeros(1,N);
g = zeros(1,N); h = ones(1,2*N);
r = zeros(1,2*N);
w_n  = normalen(p,e,w);
aux1 = w_n + y1; aux2 = [w_n(2:N),w_n(1)] + y2;
r    = (nu - 1)*[aux1,aux2];

% Testen Kruemmung Null
%r    = (nu - 1)*[y1,y2];

