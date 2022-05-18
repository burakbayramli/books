%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% initial condition u(x,0)
% for the wave equation
% finite  length vibrating string 
%============================
function y=PDE_wave_fstring_in(x,ks,as)
  
  y=zeros(1,length(x));
for k=1:length(ks)
   y=y+as(k)*sin(ks(k)*pi*x);
end