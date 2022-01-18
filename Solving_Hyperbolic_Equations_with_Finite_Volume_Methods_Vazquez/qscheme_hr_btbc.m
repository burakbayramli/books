function [wn] = qscheme_hr_btbc(wa,dtdx,m,epshr)
%
% Q-scheme for the Burgers equation
% Harten regularization
%
wam=zeros(1,m+1);
wan=zeros(1,m+1);
dm=zeros(1,m+1);
dn=zeros(1,m+1);
epsr=zeros(1,m+1);
ablanm=zeros(1,m+1);
regm=zeros(1,m+1);
ablanm=zeros(1,m+1);
regn=zeros(1,m+1);
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
% Numerical viscosity Q-scheme with Harten regularization
%
epsr=epshr*ones(1,m+1);
%
ablanm=0.5*abs(wa(1:m+1)+wam(1:m+1));
regm=ablanm+0.5*(1+sign(epsr-ablanm)).*...
((ablanm.^2+epsr.^2)./(2*epsr)-ablanm);
%
dm(1:m+1)=(0.5*dtdx)*regm.*(wam(1:m+1)-wa(1:m+1));
%
ablann=0.5*abs(wa(1:m+1)+wan(1:m+1));
regn=ablann+0.5*(1+sign(epsr-ablann)).*...
((ablann.^2+epsr.^2)./(2*epsr)-ablann);
%
dn(1:m+1)=(0.5*dtdx)*regn.*(wa(1:m+1)-wan(1:m+1));
%
wn(1:m+1)=wa(1:m+1)-cf(1:m+1)+dm(1:m+1)-dn(1:m+1);
end

