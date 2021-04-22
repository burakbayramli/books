%                             beam.m
%
%  LINEAR STATIC ANALYSIS OF A CONTINUOUS BEAM
%
clc               % Clear screen
clear             % Clear all variables in memory
%
% Make these variables global so they can be shared
% by other functions
%
global nnd nel nne nodof eldof n geom connec F ...
       prop nf Element_loads Joint_loads force Hinge
%
disp('Executing beam.m');
%
% Open file for output of results
%
%
% ALTER NEXT LINES TO CHOOSE OUTPUT FILES
%
fid =fopen('ASSIGNMET4.txt','w');
disp('Results printed to file '); 

ASSIGNMENT4              % Load the input file
%
KK =zeros(n) ;            % Initialise global stiffness 
                          % matrix to zero
%                        
F=zeros(n,1);             % Intiliase global force vector to zero
F = form_beam_F(F);       % Form global force vector
%
print_beam_model               % Print model data
%
for i=1:nel
    kl=beam_k(i)       % Form element matrix 
%    
    g=beam_g(i) ;        % Retrieve the element steering
                          % vector
%
    KK =form_KK(KK, kl, g);    % assemble global stiffness
                               % matrix
%
end
%
%
KK
F
%
%%%%%%%%%%%%  End of assembly  %%%%%%%%%%%
%
%
delta = KK\F ;       % solve for unknown displacements
%
% Extract nodal displacements
%
for i=1:nnd
    for j=1:nodof
        node_disp(i,j) = 0;
        if nf(i,j)~= 0;
        node_disp(i,j) = delta(nf(i,j)) ;
        end
    end
end
%
% Calculate the forces acting on each element 
% in local coordinates, and store them in the 
% vector force().
%
force =zeros(nel,4);
 for i=1:nel
    kl=beam_k(i);      % Form element matrix 
%   
    g=beam_g(i) ;       % Retrieve the element steering vector 
    for j=1:eldof
        if g(j)== 0
            ed(j)=0.;  % displacement = 0. for restrained freedom 
        else
            ed(j) = delta(g(j));
        end
    end
    fl = kl*ed'        % Element force vector in global XY 
    f0 = Element_loads(i,:)
    force(i,:) = fl - f0';
end
%
print_beam_results;
%
fclose(fid);