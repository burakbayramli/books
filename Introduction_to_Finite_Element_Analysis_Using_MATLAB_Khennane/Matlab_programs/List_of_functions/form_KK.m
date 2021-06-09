function[kk]=form_kk(kk,kg, g)
%
% This function assembles the global stiffness matrix
%
global nodof nne eldof
%
% This function assembles the global stiffness matrix
%
for i=1:eldof
   if g(i) ~= 0
      for j=1: eldof
          if g(j) ~= 0
             kk(g(i),g(j))= kk(g(i),g(j)) + kg(i,j);
          end
       end
   end
end
%
% end function form_kk
