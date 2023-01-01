%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=========================================
% Indentifies distinct values 
% of an integer vector
%=========================================
function [Idiff]=Find_dif(Iin)

ncount=1;
Idiff(1)=Iin(1); 
for i=2:length(Iin)
   ifound=1;
   for j=1:ncount
      if(Iin(i) == Idiff(j))
         ifound=0;break;
      end
   end
   if(ifound)
      ncount=ncount+1;
      Idiff(ncount)=Iin(i);
   end
end     