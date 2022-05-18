%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% exact solution (wave decomposition)
% for the heat equation
%============================
function y=PDE_heat_uex(x,t,ks,as)

  y=zeros(1,length(x));
for k=1:length(ks)
   y=y+as(k)*exp(-ks(k)^2*pi^2*t)*sin(ks(k)*pi*x);
end

