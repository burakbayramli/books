function [wn] = qscheme_btbc(wa,dtdx,m)
%
% Q-scheme scheme for the Burgers equation
%
wam=zeros(1,m+1);
wan=zeros(1,m+1);
dm=zeros(1,m+1);
dn=zeros(1,m+1);
%
wam(1:m)=wa(2:m+1);
% Transmissive boundary conditions
wam(m+1)=wa(m);
%
wan(2:m+1)=wa(1:m);
% Transmissive boundary conditions
wan(1)=wa(2);
%
% Central flux
%
cf(1:m+1)=0.25*dtdx*(wam(1:m+1).^2-wan(1:m+1).^2);
%
% Numerical viscosity Q-scheme
%
dm(1:m+1)=(0.25*dtdx)*abs(wa(1:m+1)+wam(1:m+1)).*(wam(1:m+1)-wa(1:m+1));
dn(1:m+1)=(0.25*dtdx)*abs(wa(1:m+1)+wan(1:m+1)).*(wa(1:m+1)-wan(1:m+1));
%
wn(1:m+1)=wa(1:m+1)-cf(1:m+1)+dm(1:m+1)-dn(1:m+1);
end

