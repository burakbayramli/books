function [q,g,h,r] = bsp03hw(p,e,u,time)
% Randbedingung fuer w

N  = size(e,2);  q  = zeros(1,N);
g  = zeros(1,N); h  = ones(1,2*N);
r  = zeros(1,2*N);
w1 = zeros(1,N); w2 = zeros(1,N);
for I = 1:N
   w1(I) = -cos(2*atan2(p(2,e(1,I)),p(1,e(1,I))))/2;
   w2(I) = -cos(2*atan2(p(2,e(2,I)),p(1,e(2,I))))/2;
end
r = [w1, w2];
