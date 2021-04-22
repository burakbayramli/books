% scale and plot the deformed configuration 
function displacements(d);
include_flags;


if strcmpi(plot_disp,'yes')==1;  
   
displacement = d(ID)*fact;

% Deformed coordinates
 j = 1;
 for i = 1:ndof:nnp*ndof
     xnew(j) = x(j) + displacement(i);
     ynew(j) = y(j) + displacement(i+1);
     j = j + 1;
 end
% plot deformed shape over the initial configuration
 for e = 1:nel
    XXnew = [xnew(IEN(1,e)) xnew(IEN(2,e)) xnew(IEN(3,e)) xnew(IEN(4,e)) xnew(IEN(1,e))];
    YYnew = [ynew(IEN(1,e)) ynew(IEN(2,e)) ynew(IEN(3,e)) ynew(IEN(4,e)) ynew(IEN(1,e))];
    plot(XXnew,YYnew,'k');hold on;
 end
title('Initial and deformed structure'); xlabel('X'); ylabel('Y');
end

