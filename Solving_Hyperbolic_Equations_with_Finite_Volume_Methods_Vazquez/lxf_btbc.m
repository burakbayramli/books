function [wn] = lxf_btbc(wa,dtdx,m)
%
% Lax Friedrichs scheme for the Burgers equation
%
wam=zeros(1,m+1);
wan=zeros(1,m+1);
%
wam(1:m)=wa(2:m+1);
% Transmissive boundary conditions
wam(m+1)=wa(m);
%
wan(2:m+1)=wa(1:m);
% Transmissive boundary conditions
wan(1)=wa(2);
%
wn(1:m+1)=0.5*(wam(1:m+1)+wan(1:m+1))-...
dtdx*0.25*(wam(1:m+1).^2-wan(1:m+1).^2);
end

