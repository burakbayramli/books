%% MATFESE:   MATLAB Finite Element Simulation Engine
%Date:      14th July, 2016
%Author:    Dr. Michael I. Okereke
%About:     This MATLAB script is the front end of a set of scripts
%           dedicated to determining Finite-Element style solutions for 
%           2D Truss problems. This Solver outputs the following: 
%           [a] Structural Stiffness Matrix; 
%           [b] External [Nodal] Forces and [Nodal] Displacements; 
%           [c] Internal Forces and 
%           [d] Internal Stresses
%Note:      This Solver requires a *.dat file to run. This file is the 
%           Keyword File which determines the FE solution to be undertaken.

%% Read keyword/input data specification file (*.dat)
    readKeywordFile

%% Evaluate model data obtained from the *.DAT files
for evaluateData = 1:1
    numberElements          =   length(elementNodes);
    numberNodes             =   length(nodeCoordinates);
    xx                      =   nodeCoordinates(:,1);
    yy                      =   nodeCoordinates(:,2);
    GDof                    =   2*numberNodes;
    force                   =   zeros(GDof,1);
    displacement            =   zeros(GDof,1);
    force(loadNodesDof,:)   =   loadValues;
    A                       =   pi*0.25*d^2;          % unit:  m^2
    EA                      =   E*A;                  % unit:  N;
end

%% Computation of system stiffness matrix
    [stiffness, k1] = detStiffness2Dtruss(GDof, numberElements,...
                       elementNodes,xx, yy, EA, detailedDisplay );
%Display Structural Stiffness Results
if detailedDisplay == 0
     disp('---------------------------------------------------------')
     display('>>>>>> Global Structural Stiffness Matrix <<<<<<<  ')
     display(stiffness)
     disp('---------------------------------------------------------')
end

%% Determine Reaction Forces and Nodal Displacements
    detExtForcesAndDisps;
 
 %% Determine Internal Forces  and Stresses
     detIntParameters;

 %% Write Output/Results log file
    outputLog
    
 %% Draw the deformed profile
 if deformedStructure == 0
     detProfile
 end

 

    
    
   
   