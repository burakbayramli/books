%                             truss.m
%
%  LINEAR STATIC ANALYSIS OF A TRUSS STRUCTURE
%
clc               % Clear screen
clear             % Clear all variables in memory
%
% Make these variables global so they can be shared
% by other functions
%
global nnd nel nne nodof eldof n
global geom connec prop nf load
%
disp('Executing truss.m');
disp('Results printed in file : truss_problem_2_results.txt '); 
%
% Open file for output of results
%
fid = fopen('truss_problem_2_results.txt','w');
%
truss_problem_2_data      % Load the input file
%
print_truss_model         % Print model data
%
KK =zeros(n) ;            % Initialise global stiffness 
                          % matrix to zero
%                        
F=zeros(n,1);             % Intiliase global force  
                          % vector to zero
%
for i=1:nel
    kl=truss_kl(i);       % Form element matrix in local xy 
%    
    C = truss_C(i);       % Form transformation matrix
%
    kg=C*kl*C' ;          % Transform the element matrix from 
                          % local to global coordinates
%                           
    g=truss_g(i) ;        % Retrieve the element steering
                          % vector
%
    KK =form_KK(KK, kg, g);    % assemble global stiffness
                               % matrix
%
end
%
%
F = form_truss_F(F);             % Form global force vector
%
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
 for i=1:nel
    kl=truss_kl(i);      % Form element matrix in local xy 
    C = truss_C(i);      % Form transformation matrix
    kg=C*kl*C' ;         % Transform the element matrix from 
                         % local to global coordinates
    g=truss_g(i) ;       % Retrieve the element steering vector 
    for j=1:eldof
        if g(j)== 0
            edg(j)=0.;  % displacement = 0. for restrained freedom 
        else
            edg(j) = delta(g(j));
        end
    end
    fg = kg*edg';        % Element force vector in global XY 
    fl=C'*fg ;           % Element force vector in local  xy 
    force(i) = fl(3);
end
%
print_truss_results;
%
fclose(fid);