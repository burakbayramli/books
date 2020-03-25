function [V1,V2]=rand_ellipsoid
global a b c
V1=rand*2*pi;
V2=acos(1-2*rand);
C=4*pi*max([a,b,c])^2;
while rand>ellipsoid(V1,V2)/(C*sin(V2)/4*pi)
    V1=rand*2*pi;
    V2=acos(1-2*rand);
end
     
