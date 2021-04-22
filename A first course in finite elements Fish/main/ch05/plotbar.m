function plotbar;
include_flags;

% check if user defined the bar plot
if strcmpi(plot_bar,'yes')==1;  
    for i = 1:nel
        XX = [x(IEN(1,i)) x(IEN(2,i)) x(IEN(3,i)) x(IEN(1,i)) ];
        YY = [y(IEN(1,i)) y(IEN(2,i)) y(IEN(3,i)) y(IEN(1,i)) ];        
        line(XX,YY);hold on;       
        line(XX,-YY);hold on;
        plot(XX, [0 0 0 0], '+r');  
        
        % check if user defined the plots of the global node numbering 
        if strcmpi(plot_nod,'yes')==1;   
            text(XX(1),-1,sprintf('%0.5g',IEN(1,i)));
            text(XX(2),-1,sprintf('%0.5g',IEN(2,i)));
            text(XX(3),-1,sprintf('%0.5g',IEN(3,i)));
        end
    end
title('Bar Plot');
end

% print some mesh parameters
fprintf(1,'  Bar Params \n');
fprintf(1,'No. of Elements  %d \n',nel);
fprintf(1,'No. of Nodes     %d \n',nnp);
fprintf(1,'No. of Equations %d \n\n',neq);

