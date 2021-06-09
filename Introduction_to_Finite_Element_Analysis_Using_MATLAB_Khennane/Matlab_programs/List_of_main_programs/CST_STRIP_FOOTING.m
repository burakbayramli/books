% THIS PROGRAM USES AN 3-NODE LINEAR TRIANGULAR ELEMENT FOR THE 
% LINEAR ELASTIC STATIC ANALYSIS OF A TWO DIMENSIONAL PROBLEM
% IT INCLUDES AN AUTOMATIC MESH GENERATION 
%
% Make these variables global so they can be shared by other functions
%
clear all
clc
global nnd nel nne  nodof eldof  n 
global geom  dee nf Nodal_loads
global Length Width NXE NYE X_origin Y_origin
%
 format long g
%
% 
% To change the size of the problem or change elastic properties
%    supply another input file 
%
Length = 6.; % Length of the model
Width =5.;    % Width
NXE = 12;      % Number of rows in the x direction
NYE = 10;      % Number of rows in the y direction
dhx = Length/NXE; % Element size in the x direction
dhy = Width/NYE;  % Element size in the x direction
X_origin = 0. ;  % X origin of the global coordinate system
Y_origin = 0. ;   % Y origin of the global coordinate system
%
nne = 3;
nodof = 2;
eldof = nne*nodof;
%
T3_mesh ;         % Generate the mesh 
%
% Material 
%
E = 100000.;     % Elastic modulus in MPa
vu = 0.3;       % Poisson's ratio 
thick = 1.;      % Beam thickness in mm
%
% Form the elastic matrix for plane strain 
%
dee = formdeps(E,vu);
%
%
% Boundary conditions
%
nf = ones(nnd, nodof);    % Initialise the matrix nf to 1
%
% Restrain in the x-direction the nodes situated @ 
% (x = 0  or x = Length)
%
for i=1:nnd
    if geom(i,1) == 0.  | geom(i,1) == Length;
        nf(i,:) = [0 1];
    end
end
%
% Restrain in all directions the nodes situated @ 
% (y = 0)
%
for i=1:nnd
    if geom(i,2) == 0. ;
        nf(i,:) = [0 0];
    end
end
%
% Counting of the free degrees of freedom
%
n=0;
for i=1:nnd
    for j=1:nodof
        if nf(i,j) ~= 0 
            n=n+1;
           nf(i,j)=n;
        end
    end
end
%
% loading
%
Nodal_loads= zeros(nnd, 2);   % Initialise the matrix of odal loads to 0
%
% Apply equivalent concentrated loads on nodes 11, 22, and 33 in the
% y-direction.
%
   Nodal_loads(11,:) = [0.  -1.25]; 
   Nodal_loads(22,:) = [0.  -2.50]; 
   Nodal_loads(33,:) = [0.  -1.25]; 
%
%%%%%%%%%%%%%%%%%%%%%%%%%% End of input%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%
% Assemble the global force vector
%
fg=zeros(n,1);
for i=1: nnd
    if nf(i,1) ~= 0   
        fg(nf(i,1))= Nodal_loads(i,1);
    end
    if nf(i,2) ~= 0   
        fg(nf(i,2))= Nodal_loads(i,2);
    end
end 
%
% Assembly of the global stiffness matrix
%
%  initialise the global stffness matrix to zero
%
kk = zeros(n, n);
%
for i=1:nel
    [bee,g,A] = elem_T3(i);     % Form strain matrix, and stering vector
    ke=thick*A*bee'*dee*bee;    % Compute stiffness matrix
    kk=form_kk(kk,ke, g);       % assemble global stiffness matrix
end
%
%
%%%%%%%%%%%%%%%%%%%%%%%  End of assembly %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
delta = kk\fg ;            % solve for unknown displacements
%
for i=1: nnd                                %
    if nf(i,1) == 0                         %
        x_disp =0.;                         %
    else
        x_disp = delta(nf(i,1));            %
    end
%    
    if nf(i,2) == 0                         %
        y_disp = 0.;                        %
    else
        y_disp = delta(nf(i,2));            %
    end
    node_disp(i,:) =[x_disp  y_disp];
end
%
%
% Retrieve the y_disp of the nodes located on centre line beneath 
% the footing
%
k = 0;
vertical_disp=zeros(1,NYE+1);
for i=1:nnd;
    
    if geom(i,1)== 0.
        k=k+1;
        y_coord(k) = geom(i,2);
        vertical_disp(k)=node_disp(i,2);
    end
end
%
for i=1:nel
    [bee,g,A] = elem_T3(i);    % Form strain matrix, and stering vector 
    eld=zeros(eldof,1);        % Initialise element displacement to zero
    for m=1:eldof              
        if g(m)==0             
         eld(m)=0.;         
        else                        %
         eld(m)=delta(g(m));     % Retrieve element displacement 
        end
    end
%
   eps=bee*eld;                 % Compute strains
   EPS(i,:)=eps ;               % Store strains for all elements
   sigma=dee*eps;               % Compute stresses
   SIGMA(i,:)=sigma ;           % Store stresses for all elements
end
%
% Calculate the principal stresses 
%
SIG1=zeros(nel,1); SIG2=zeros(nel,1); 
for i = 1:nel
SIG1(i) = (SIGMA(i,1)+SIGMA(i,2))/2 + sqrt(((SIGMA(i,1)+SIGMA(i,2))/2)^2 +SIGMA(i,3)^2);
SIG2(i) = (SIGMA(i,1)+SIGMA(i,2))/2 - sqrt(((SIGMA(i,1)+SIGMA(i,2))/2)^2 +SIGMA(i,3)^2);
end
cmin = min(SIG2);
cmax = max(SIG2);
caxis([cmin cmax]);
patch('Faces', connec, 'Vertices', geom, 'FaceVertexCData',SIG2, 'Facecolor','flat','Marker','o');
colorbar;
%
plottools;

