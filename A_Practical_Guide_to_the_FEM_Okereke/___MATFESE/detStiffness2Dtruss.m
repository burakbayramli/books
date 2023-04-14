%% detStiffness2DTruss:  DETERMINE THE GLOBAL STIFFNESS MATRIX OF A 2D TRUSS SYSTEM
%Date:      5th August, 2014
%Author:    Dr. Michael I. Okereke
%About:     This Matlab script is the Simulation Engine of the FE Solver.
%           It is designed to determine the 2D Global Stiffness Matrix for 
%           the structure under investigation. 
%% -------------------------------------------------------------------------
function [stiffness, k1] = detStiffness2Dtruss(GDof, numberElements,...
                                elementNodes,xx, yy, EA, detailedDisplay )
%   Initialize parameters
stiffness           =   zeros(GDof);
memk1               =   cell(1);
memStiffness        =   cell(1);
structStiffness     =   cell(1);

%computation of the system stiffness matrix
for e = 1:numberElements;
    %elementDof:    element degrees of freedom (Dof)
    indice          =   elementNodes(e,:);
    elementDof      =   [indice(1)*2-1 indice(1)*2 indice(2)*2-1 indice(2)*2];
    xa              =   xx(indice(2)) - xx(indice(1));
    ya              =   yy(indice(2)) - yy(indice(1));
    length_element  =   sqrt(xa*xa+ya*ya);
    C               =   xa/length_element;
    S               =   ya/length_element;
    
    k1              =   EA/length_element*...
                                [   C*C     C*S     -C*C    -C*S        ;  
                                    C*S     S*S     -C*S    -S*S        ;
                                   -C*C    -C*S      C*C     C*S        ; 
                                   -C*S    -S*S      C*S     S*S        ];
   stiffness(elementDof, elementDof) = stiffness(elementDof, elementDof)+k1;
   
   memk1{e}             =   EA/length_element;
   memStiffness{e}      =   k1;
   
   %Print out the member stiffness matrix
   if detailedDisplay == 0
       disp('---------------------------------------------------------------------------------')
       disp(['For Member >> ',num2str(e),' <<, Stiffness Constant [', num2str(EA/length_element), ' N/m], nodes [',num2str(indice(1)),'-',num2str(indice(2)),']the member stiffness matrix  (k/[EA/L]) is :'])
       disp(k1/(EA/length_element));
       disp(['For Member >> ',num2str(e),' <<, node [',num2str(indice(1)),'-',num2str(indice(2)),']the (structure-format) member stiffness matrix is :'])
       iniStiffness = zeros(GDof);
       iniStiffness(elementDof,elementDof) = iniStiffness(elementDof, elementDof)+k1;
       disp(iniStiffness)
       disp(['For Member >> ',num2str(e),' <<, node [',num2str(indice(1)),'-',num2str(indice(2)),']the current updated (structure-format) member stiffness matrix is :'])
       stiffness(elementDof, elementDof) = stiffness(elementDof, elementDof)+k1;
       disp(stiffness)
       disp('---------------------------------------------------------------------------------')
   end
        iniStiffness = zeros(GDof);
        iniStiffness(elementDof,elementDof) = iniStiffness(elementDof, elementDof)+k1;
        structStiffness{e}   =   iniStiffness;
end
%Save all acquired data sets
 save memStiffnessData stiffness memStiffness memk1 structStiffness
end
%% ------------------------------------------------------------------------
