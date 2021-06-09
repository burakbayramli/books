function plottruss;
include_flags;

% check if truss plot is requested
if strcmpi(plot_truss,'yes')==1;  
    for i = 1:nel
        XX = [x(IEN(1,i)) x(IEN(2,i)) x(IEN(1,i)) ];
        YY = [y(IEN(1,i)) y(IEN(2,i)) y(IEN(1,i)) ];
        line(XX,YY);hold on;

        % check if node numbering is requested
        if strcmpi(plot_nod,'yes')==1;   
            text(XX(1),YY(1),sprintf('%0.5g',IEN(1,i)));
            text(XX(2),YY(2),sprintf('%0.5g',IEN(2,i)));
        end
    end
    title('Truss Plot');
end

% print mesh parameters
fprintf(1,'\tTruss Params \n');
fprintf(1,'No. of Elements  %d \n',nel);
fprintf(1,'No. of Nodes     %d \n',nnp);
fprintf(1,'No. of Equations %d \n\n',neq);



