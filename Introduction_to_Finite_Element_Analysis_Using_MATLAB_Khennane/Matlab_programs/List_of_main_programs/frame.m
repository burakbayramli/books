%  PROGRAM frame.m
%
%   LINEAR STATIC ANALYSIS OF A RIGID JOINTED FRAME
%
% Make these variables global so they can be shared by other functions
%
clc
clear all
format long e
%
global nnd nel nne nodof eldof n geom connec F
global  prop nf Element_loads Joint_loads force Hinge
%
%
disp('Executing frame.m');
%
% Open file for output of results
%
% ALTER NEXT LINES TO CHOOSE OUTPUT FILES
%
fid =fopen('frame_problem2_results.txt','w');
disp('Results printed to file'); 
%
%%%%%%%%%%%%  Beginning of data input    %%%%%%%%%%%%%%%%%%%%% 
%
frame_problem2_data;          % Load the input file
%
F = zeros(n,1);    % Intialiase global force vector to zero
%
F = Assem_Joint_Loads(F);    % Assemble joint loads to global force vector
%
KK = zeros(n, n);  % Initialise the global stffness matrix to zero
%
for i=1:nel
    kl=beam_column_k(i)     % Form element matrix in local xy 
    C = beam_column_C(i)    % Form transformation matrix
    kg=C*kl*C'              % Transform the element matrix from local 
                             % to global coordinates
    fl= Element_loads(i,:) ; % Retrieve element equivalent nodal forces
                             % in local xy 
    fg=C*fl'  ;               % Transform the element force vector from local 
                             % to global coordinates
    g=beam_column_g(i) ;     % Retrieve the element degrees of freedom 
    KK =form_kk(KK , kg, g)   % assemble global stiffness matrix
    F = Assem_Elem_loads(F , fg, g)   % assemble global force vector
end
%
%%%%%%%%%%%%%%%%    End of assembly       %%%%%%%%%%%%
%
%
delta = KK\F;              	% solve for unknown displacements
%
% %
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
%
 for i=1:nel
    kl=beam_column_k(i);     % Form element matrix in local xy 
    C = beam_column_C(i);   % Form transformation matrix
    kg=C*kl*C' ;             % Transform the element matrix from local 
                             % to global coordinates
    g=beam_column_g(i) ;     % Retrieve the element degrees of freedom 
    for j=1:eldof
        if g(j)== 0
            edg(j)=0.;  % displacement = 0. for restrained freedom 
        else
            edg(j) = delta(g(j));
        end
    end
    
    fg = kg*edg';            % Element force vector in global XY 
    fl = C'*fg ;             % Element force vector in local  xy 
    f0 = Element_loads(i,:); % Equivalent nodal loads
    force_l(i,:) = fl-f0';
    force_g(i,:) = C*(fl-f0');
end
%
print_frame_model;            % Print model data
print_frame_results;
%
fclose(fid);






