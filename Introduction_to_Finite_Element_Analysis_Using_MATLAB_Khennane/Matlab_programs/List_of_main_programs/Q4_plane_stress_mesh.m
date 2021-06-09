% THIS PROGRAM USES AN 4-NODED QUADRILATERAL ELEMENT FOR THE LINEAR ELASTIC 
% STATIC ANALYSIS OF A TWO DIMENSIONAL PROBLEM
%
% Make these variables global so they can be shared by other functions
%
clc
clear all
global nnd nel nne nodof eldof n ngp
global geom connec dee nf Nodal_loads
global Length Width NXE NYE X_origin Y_origin dhx dhy 
%
 format long g
%
% To change the size of the mesh, alter the next statements
%    
Length = 60.; % Length of the model
Width =20.;    % Width
NXE = 30;      % Number of rows in the x direction
NYE = 10;      % Number of rows in the y direction
dhx = Length/NXE; % Element size in the x direction
dhy = Width/NYE;  % Element size in the x direction
X_origin = 0. ;  % X origin of the global coordinate system
Y_origin = Width/2. ;   % Y origin of the global coordinate system
%
nne = 4;
nodof = 2;
eldof = nne*nodof;
%
Q4_mesh     % Generate the mesh 
%
E = 200000.;     % Elastic modulus in MPa
vu = 0.3;       % Poisson's ratio 
thick = 5.;      % Beam thickness in mm
%
% Form the elastic matrix for plane stress 
%
dee = formdsig(E,vu);
%
%
% Boundary conditions
%
nf = ones(nnd, nodof);    % Initialise the matrix nf to 1
%
% Restrain in all directions the nodes situated @ 
% (x = Length)
%
for i=1:nnd
    if geom(i,1) == Length;
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
% Apply a concentrated at the node having x = 0, and y = 0.
%
Force = 1000.;  % N
%
for i=1:nnd
    if geom(i,1) == 0. && geom(i,2) == 0.
        Nodal_loads(i,:) = [0.  -Force]; 
    end
end
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
%  Form the matrix containing the abscissas and the weights of Gauss points
%
ngp = 2;
samp=gauss(ngp);
%
% Numerical integration and assembly of the global stiffness matrix
%
%  initialise the global stffness matrix to zero
kk = zeros(n, n);
%
for i=1:nel
    [coord,g] = elem_q4(i) ;       % coordinates of the nodes of element i,
                                   %  and its steering vector 
    ke=zeros(eldof,eldof) ;        % Initialise the element stiffness 
                                   %  matrix to zero
    for ig=1: ngp
        wi = samp(ig,2);
    for jg=1: ngp
        wj=samp(jg,2);
        [der,fun] = fmlin(samp, ig,jg);  % Derivative of shape functions 
                                         % in local coordinates
        jac=der*coord;                   % Compute Jacobian matrix
        d=det(jac);                      % Compute determinant of Jacobian
                                         %  matrix
        jac1=inv(jac);                   % Compute inverse of the Jacobian
        deriv=jac1*der;                  % Derivative of shape functions in
                                         % global coordinates
        bee=formbee(deriv,nne,eldof);    %  Form matrix [B]
        ke=ke + d*thick*wi*wj*bee'*dee*bee; % Integrate stiffness matrix
    end
    end
    kk=form_kk(kk,ke, g);               % assemble global stiffness matrix
end
%
%
%%%%%%%%%%%%%%%%%%%%%%%  End of assembly %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
delta = kk\fg ;                         % solve for unknown displacements

disp('node       x_disp       y_disp ')     %
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
disp([i  x_disp  y_disp])                   % Display displacements of each node
DISP(i,:) = [x_disp  y_disp]
end

%
%
ngp=1;                              % Calculate stresses and strains at 
                                    %the centre of each element
samp=gauss(ngp);
%
for i=1:nel
    [coord,g] = elem_q4(i);      % coordinates of the nodes of element i, 
                                 % and its steering vector 
    eld=zeros(eldof,1);          % Initialise element displacement to zero
    for m=1:eldof                %
        if g(m)==0               %
            eld(m)=0.;           %
        else                     %
            eld(m)=delta(g(m));  % Retrieve element displacement from the 
                                 % global displacement vector
        end
    end
%
    for ig=1: ngp
        wi = samp(ig,2);
    for jg=1: ngp
        wj=samp(jg,2);
        [der,fun] = fmlin(samp, ig,jg); % Derivative of shape functions in
                                        % local coordinates
        jac=der*coord;                  % Compute Jacobian matrix
        jac1=inv(jac);                  % Compute inverse of the Jacobian
        deriv=jac1*der;                 % Derivative of shape functions in 
                                        % global coordinates
        bee=formbee(deriv,nne,eldof);   % Form matrix [B]
        eps=bee*eld                     % Compute strains
        sigma=dee*eps                   % Compute stresses
    end
    end
   SIGMA(i,:)=sigma ;           % Store stresses for all elements
end
%
% Average stresses at nodes
%
[ZX, ZY, ZT, Z1, Z2]=stresses_at_nodes_Q4(SIGMA);
%
%
% Plot stresses in the x_direction 
%
U2 = DISP(:,2);
cmin = min(ZX);
cmax = max(ZX);
caxis([cmin cmax]);
patch('Faces', connec, 'Vertices', geom, 'FaceVertexCData',ZX, ...
      'Facecolor','interp','Marker','.');
 colorbar;
