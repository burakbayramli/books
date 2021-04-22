function[F] = Assem_Elem_loads(F , fg, g)
%
% This function assemble the global force vector
%
global eldof
%
% This function assembles the global force vector
%
for idof=1:eldof
   if (g(idof))~= 0
       F(g(idof))= F(g(idof))+  fg(idof);
   end
end
%
% end function Assem_Elem_loads