%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==========================================
% Visualization of the borders of the domain
%==========================================

function Show_border(xb,yb,nfig)

figure(nfig);
nb=length(xb);
for i=1:2:nb
   plot(xb(i:i+1),yb(i:i+1),'k*','LineWidth',2);hold on;
end
plot([-1 1 1 -1 -1],[-1 -1 1 1 -1],'k','LineWidth',2);
axis equal; axis([-1 1 -1 1 ]) 

