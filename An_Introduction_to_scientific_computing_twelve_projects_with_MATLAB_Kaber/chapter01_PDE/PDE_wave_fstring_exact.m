%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% exact solution
% for the wave equation
% finite length vibrating string 
%============================
function y=PDE_wave_fstring_exact(x,t,c,ks,as)

  y=zeros(1,length(x));
for k=1:length(ks)
   y=y+as(k)*cos(ks(k)*pi*c*t)*sin(ks(k)*pi*x);
end