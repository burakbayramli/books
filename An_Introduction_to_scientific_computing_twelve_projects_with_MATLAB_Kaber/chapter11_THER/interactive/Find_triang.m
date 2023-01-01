%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%===============================================
% Finds the triangle containing the given point P
% by computing the barycentric coordinates
%===============================================

function [ifind]=Find_triang(P)

global Nt XYs I123

bbar=[P(1);P(2);1]; ifind=0;
for i=1:Nt
   Abar=[XYs(I123(i,:)',:)'; 1 1 1];
   lamb=Abar\bbar;
   if( lamb(1) >= 0 & lamb(2) >= 0 & lamb(3) >= 0 ) 
      %disp(['Point P(' num2str(P(1)) ',' num2str(P(2)) ') is in the triangle ' num2str(i)]);
      %plot(P(1),P(2),'ro');hold on
      %Iplot=[I123(i,:)';I123(i,1)];
      %plot(XYs(Iplot,1),XYs(Iplot,2),'r');
      ifind=i;
      break
   end
end

if(ifind == 0)
   disp(['Point P(' num2str(P(1)) ',' num2str(P(2)) ') is not in the domain']);
end
