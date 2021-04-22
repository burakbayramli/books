function[ff]=form_ff(ff,fg, g)
%
% This function assemble the global force vector
%
global nodof nne eldof
%
% This function assembles the global force vector
%
for idof=1:eldof
   if (g(idof))~= 0
       ff(g(idof))= ff(g(idof))+  fg(idof);
   end
end
%
% end function form_ff