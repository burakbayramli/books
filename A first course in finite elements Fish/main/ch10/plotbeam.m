function plotbar;
include_flags;

% check if user requested to plot the beam 
if strcmpi(plot_beam,'yes')==1;  
    for i = 1:nel
        XX = [x(IEN(1,i)) x(IEN(2,i))  x(IEN(1,i)) ];
        YY = [y(IEN(1,i)) y(IEN(2,i))  y(IEN(1,i)) ];        
        line(XX,YY);hold on;       
         line(XX,-YY);hold on;
        plot(XX,YY , '+r');  
        
        % check if user requested to plot the global node numbering 
        if strcmpi(plot_nod,'yes')==1;   
            text(XX(1),YY(1),sprintf('%0.5g',IEN(1,i)));
            text(XX(2),YY(2),sprintf('%0.5g',IEN(2,i)));
           
        end
    end
title('Bar Plot');
end

% print mesh parameters
fprintf(1,'  Beam Parameters \n');
fprintf(1,'No. of Elements  %d \n',nel);
fprintf(1,'No. of Nodes     %d \n',nnp);
fprintf(1,'No. of Equations %d \n\n',neq);