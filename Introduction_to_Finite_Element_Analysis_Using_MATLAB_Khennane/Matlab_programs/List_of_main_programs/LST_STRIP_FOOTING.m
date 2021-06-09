% THIS PROGRAM USES A 6-NODE LINEAR TRIANGULAR ELEMENT FOR THE 
% LINEAR ELASTIC STATIC ANALYSIS OF A TWO DIMENSIONAL PROBLEM
% IT INCLUDES AN AUTOMATIC MESH GENERATION 
%
% Make these variables global so they can be shared by other functions
%
clear all
clc
global nnd nel nne  nodof eldof  n 
global connec geom  dee nf Nodal_loads XIG YIG
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
XIG = zeros(2*NXE+1,1); YIG=zeros(2*NYE+1,1); % Vectors holding grid coordinates
dhx = Length/NXE; % Element size in the x direction
dhy = Width/NYE;  % Element size in the x direction
X_origin = 0. ;  % X origin of the global coordinate system
Y_origin = 0. ;   % Y origin of the global coordinate system
%
nne = 6;
nodof = 2;
eldof = nne*nodof;
%
T6_mesh ;         % Generate the mesh 
%
% Material 
%
E = 100000.;     % Elastic modulus in MPa
vu = 0.3;       % Poisson's ratio 
thick = 1.;      % Beam thickness in mm
nhp = 3;        % Number of sampling points
%
% Form the elastic matrix for plane stress 
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
%
% Apply equivalent concentrated loads on nodes 21, 42, 63, 84 and 105 in the
% y-direction.
%
   Nodal_loads(21,:) = [0.  -0.416]; 
   Nodal_loads(42,:) = [0.  -1.666]; 
   Nodal_loads(63,:) = [0.  -0.833]; 
   Nodal_loads(84,:) = [0.  -1.666];
   Nodal_loads(105,:) = [0.  -0.416];
%
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
%
%  Form the matrix containing the abscissas and the weights of Hammer points
%
samp=hammer(nhp);
%
%  initialise the global stffness matrix to zero
%
kk = zeros(n, n);
%
for i=1:nel
    [coord,g] = elem_T6(i);  % Form strain matrix, and stering vector
    ke=zeros(eldof,eldof) ;  % Initialise the element stiffness matrix to zero
     for ig = 1:nhp
        wi = samp(ig,3);
        [der,fun] = fmT6_quad(samp, ig);   
        jac = der*coord;
        d = det(jac);
        jac1=inv(jac);      % Compute inverse of the Jacobian
        deriv=jac1*der;     % Derivative of shape functions in global coordinates
        bee=formbee(deriv,nne,eldof);   %  Form matrix [B]
        ke=ke + d*thick*wi*bee'*dee*bee; % Integrate stiffness matrix
     end
    kk=form_kk(kk,ke, g);                % assemble global stiffness matrix
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
% Retrieve the x_coord and y_disp of the nodes located on the neutral axis
%
k = 0;
for i=1:nnd;
    
    if geom(i,1)== 0.
        k=k+1;
        y_coord(k) = geom(i,2);
        vertical_disp(k)=node_disp(i,2);
    end
end
%
nhp = 1;   % Calculate stresses at the centroid of the element
samp=hammer(nhp);
%
for i=1:nel
    [coord,g] = elem_T6(i);         % Retrieve coordinates and stering vector
    eld=zeros(eldof,1);             % Initialise element displacement to zero
    for m=1:eldof                   %
        if g(m)==0                  %
            eld(m)=0.;              %
        else                        %
            eld(m)=delta(g(m));     % Retrieve element displacement from 
                                    % the global displacement vector
        end
    end
%
    for ig=1: nhp
       [der,fun] = fmT6_quad(samp,ig); % Derivative of shape functions in local coordinates
        jac=der*coord;                 % Compute Jacobian matrix
        jac1=inv(jac);                 % Compute inverse of the Jacobian
        deriv=jac1*der;                % Derivative of shape functions in global coordinates
        bee=formbee(deriv,nne,eldof);  %  Form matrix [B]
        eps=bee*eld;                   % Compute strains
        sigma=dee*eps ;                % Compute stresses
    end         
   SIGMA(i,:)=sigma ;           % Store stresses for all elements
end
%
% Prepare stresses for ploting
%
[ZX, ZY, ZT, Z1, Z2]=prepare_contour_data(SIGMA);
%
%  Plot mesh using patches
%
% patch('Faces',connec,'Vertices',geom,'FaceVertexCData',hsv(nel), ...
%         'Facecolor','none','Marker','o');
%
% Plot stresses in the x_direction 
%
[C,h]= contourf(XIG,YIG,Z2,40);
%clabel(C,h);  
colorbar
plottools;

