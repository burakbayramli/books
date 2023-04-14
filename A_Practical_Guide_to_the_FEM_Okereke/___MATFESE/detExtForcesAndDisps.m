%% Determine Internal Forces (for the members/elements)
%Author:    Dr. Michael I. Okereke
%Date:      11th August, 2014
%About:     A script for determining the external forces and nodal
%           displacements, based on a given structural analysis using the DSM
%--------------------------------------------------------------------------
%% Create Modified Stiffness Matrix
    kMod = stiffness;
 %Remove the zero displacement rows
     kMod(zeroDispNodesDof,:)       =   [];
     kMod(:,zeroDispNodesDof)       =   [];
     kForce                         =   force;
     kForce(zeroDispNodesDof,:)     =   [];
     
     %If required, display the modified (reduced) Global Structure
     %Stiffness Matrix
     if detailedDisplay == 0
        disp('---------------------------------------------------------------------------------')
        disp('The modified (reduced) Global Structure Stiffness Matrix, Kmod is >>>:   ')
        disp(kMod)
        disp('---------------------------------------------------------------------------------')
     end
     
 %Augmented Matrix
    Q  =    [kMod kForce];
    
     %If required, display the augmented Global Structure Stiffness Matrix
     if detailedDisplay == 0
        disp('---------------------------------------------------------------------------------')
        disp('The augmented Global Structure Stiffness Matrix, Q is >>>:   ')
        disp(Q)
        disp('---------------------------------------------------------------------------------')
     end
    
 %Determine the reduced row echelon format of Q
    R  =    rref(Q);
    
    %If required, display the reduced row echelon format of Q
     if detailedDisplay == 0
        disp('---------------------------------------------------------------------------------')
        disp('The reduced row echelon format of Q is >>>:   ')
        disp(R)
        disp('---------------------------------------------------------------------------------')
     end
    
 %Displacement Results
    dispNumbers                 =   1:length(displacement);
    rezNumbers                  =   setdiff(dispNumbers, zeroDispNodesDof);
    rezDisplacement             = 	R(:,end);
    displacement(rezNumbers)    =   rezDisplacement;
 
 %Force Results
    rezForce                    = 	stiffness*displacement;
    
 %Display Results
 if detailedDisplay == 0
    disp('--------------------------------------------------------------------')
    disp(' >>>      EXTERNAL FORCES AND DISPLACEMENT RESULTS         <<<      ')
    disp('--------------------------------------------------------------------')
    disp('The nodal displacements are   >>>  ')
    disp(displacement)
    disp('---------------------------------------------------------------------------------')
%     disp('---------------------------------------------------------------------------------')
    disp('The nodal forces are          >>>  ')
    disp(rezForce);
    disp('---------------------------------------------------------------------------------')
%     disp('---------------------------------------------------------------------------------')
 end