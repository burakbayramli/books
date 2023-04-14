%% Determine Internal Forces (for the members/elements)
%Date:      10th August, 2014
%Author:    Dr. Michael I. Okereke

%% Iterate through elements/members that make up the structure
dispLocal = cell(1);    memForces = cell(1); memStresses = cell(1);
for e = 1:numberElements;
    %Determine element connectivities
    indice          =   elementNodes(e,:);
    elementDof      =   [indice(1)*2-1 indice(1)*2 indice(2)*2-1 indice(2)*2];
    xa              =   xx(indice(2)) - xx(indice(1));
    ya              =   yy(indice(2)) - yy(indice(1));
    length_element  =   sqrt(xa*xa+ya*ya);
    C               =   xa/length_element;
    S               =   ya/length_element;
    
%Determine the Displacement transformation Matrix
    Td              =   [   C       S       0       0;  
                           -S       C       0       0;
                            0       0       C       S;
                            0       0      -S       C   ];
                           
 %Isolate the local (member-specific) local displacemnt variables
    dispLocal{e}  	=   Td*displacement(elementDof);
%--------------------------------------------------------------------------
if detailedDisplay == 0
    disp('--------------------------------------------------------------------')
    disp(' >>>      INTERNAL FORCES and STRESSES RESULTS         <<<          ')
    disp('--------------------------------------------------------------------')

    %Print out the member local displacement values
           disp(['For Member >> ',num2str(e),' << node [',num2str(indice(1)),'-',num2str(indice(2)),'], Local Nodal Displacement is :'])
           disp(dispLocal{e});
end

    %% Calculate the Forces
        justDisplacement = dispLocal{e};
        memForces{e} =  (EA/length_element)*(justDisplacement(3)-justDisplacement(1));%Focus on x-axis displacements

       %Display member forces
if detailedDisplay == 0
           disp(['For Member >> ',num2str(e),' << node [',num2str(indice(1)),'-',num2str(indice(2)),'], Internal Force is :'])
           disp([num2str(memForces{e}), ' N']);
end

    %% Calculate the Stresses
        memStresses{e} =  memForces{e}/A;   %Expression forcalculating member stresses
       %Display member stresses
if detailedDisplay == 0
           disp(['For Member >> ',num2str(e),' << node [',num2str(indice(1)),'-',num2str(indice(2)),'], Internal Stress is :'])
           disp([num2str(memStresses{e}/1e6), ' MPa']);
           disp('---------------------------------------------------------------------------------')
end
end
