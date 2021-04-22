% THIS PROGRAM USES AN 8-NODED QUADRILATERAL ELEMENT FOR THE LINEAR ELASTIC 
% STATIC ANALYSIS OF AN AXISYMMETRIC PROBLEM. IT CONTAINS AN AUTOMATIC
% MESH GENERATION MODULE Q8_mesh.m
%
% Make these variables global so they can be shared by other functions
%
clc
clear all
global nnd nel nne  nodof eldof  n ngp
global geom connec dee nf Nodal_loads
global Length Width NXE NYE X_origin Y_origin dhx dhy 
%
 format long g
%
% To change the size of the problem or change elastic properties
%    ALTER the q8_input_module.m
%
Length = 7.; % Length of the model
Width =9.;    % Width
NXE = 14;      % Number of rows in the x direction
NYE = 18;      % Number of rows in the y direction
dhx = Length/NXE; % Element size in the r direction
dhy = Width/NYE;  % Element size in the z direction
X_origin = 0. ;  % r origin of the global coordinate system
Y_origin = 0. ;   % z origin of the global coordinate system
%
nne = 8;
nodof = 2;
eldof = nne*nodof;
ngp = 3;
%
Q8_mesh     % Generate the mesh 
%
E = 100000.;     % Elastic modulus in kN/m2
vu = 0.35;       % Poisson's ratio 
%
% Form the elastic matrix for plane stress 
%
dee = formdax(E,vu);
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
    if geom(i,1) == 0 || geom(i,1) == Length
    nf(i,:) = [0  1]; % Restrain in direction r the nodes situated @ 
                     % (x = 0) and (x = Length)
    end
%     
    if geom(i,2) == 0;
    nf(i,:) = [0 0]; % Restrain in all directions the nodes situated @ 
                     % (y = 0) Rock substratutm
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
% Apply an equivalent nodal load to the nodes located at
% (r = 0, z = 9.), (r = 0.25, z = 9.), and (r = 0.5, z = 9.)
% (r = .75, z = 9.), (r = 1., z = 9.)
%
pressure = 63.662 ; % kN/m^2
%
for i=1:nnd
    if geom(i,1) == 0. && geom(i,2) == 9.
        Nodal_loads(i,:) = pressure*[0.  0.]; 
    elseif geom(i,1) == 0.25 && geom(i,2) == 9.
        Nodal_loads(i,:) = pressure*[0.  -0.0833]; 
    elseif geom(i,1) == 0.5 && geom(i,2) == 9.
        Nodal_loads(i,:) = pressure*[0.  (-0.0833-0.0833)]; 
    elseif geom(i,1) == 0.75 && geom(i,2) == 9.
        Nodal_loads(i,:) = pressure*[0.  -0.25]; 
    elseif geom(i,1) == 1. && geom(i,2) == 9.
        Nodal_loads(i,:) = pressure*[0.  -0.0833]; 
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
samp=gauss(ngp);
%
% Numerical integration and assembly of the global stiffness matrix
%
%  initialise the global stffness matrix to zero
kk = zeros(n, n);
%
for i=1:nel
  [coord,g] = elem_q8(i) ;       % coordinates of the nodes of element i,
                                 % and its steering vector 
  ke=zeros(eldof,eldof) ;        % Initialise the element stiffness 
                                 % matrix to zero
  for ig=1: ngp
      wi = samp(ig,2);
  for jg=1: ngp
   wj=samp(jg,2);
   [der,fun] = fmquad(samp, ig,jg);  % Derivative of shape functions
                                     % in local coordinates
   jac=der*coord;            % Compute Jacobian matrix
   d=det(jac);               % Compute determinant of Jacobian matrix
   jac1=inv(jac);            % Compute inverse of the Jacobian
   deriv=jac1*der;           % Derivative of shape functions in 
                             % global coordinates
   [bee,radius]=formbee_axi(deriv,nne,fun, coord,eldof); % Form matrix [B]
   ke=ke + d*wi*wj*bee'*dee*bee*radius; % Integrate stiffness matrix
  end
  end
  kk=form_kk(kk,ke, g);           % assemble global stiffness matrix
end
%
%
%%%%%%%%%%%%%%%%%%%%%%%  End of assembly %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
delta = kk\fg ;                          % solve for unknown displacements
disp('node       x_disp       y_disp ')  %
for i=1: nnd                             %
    if nf(i,1) == 0                      %
        x_disp =0.;                      %
    else
        x_disp = delta(nf(i,1));         %
    end
%    
    if nf(i,2) == 0                      %
        y_disp = 0.;                     %
    else
        y_disp = delta(nf(i,2));        %
    end
disp([i  x_disp  y_disp]) ;            % Display displacements of each node
DISP(i,:) = [x_disp  y_disp];
end
%
%
ngp=1;                                  % Calculate stresses and strains at
                                        % the centre of each element
samp=gauss(ngp);
%
for i=1:nel
  [coord,g] = elem_q8(i);      % coordinates of the nodes of element i, 
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
      [der,fun] = fmquad(samp, ig,jg); % Derivative of shape functions in 
                                       % local coordinates
      jac=der*coord;                   % Compute Jacobian matrix
      jac1=inv(jac);                   % Compute inverse of the Jacobian
      deriv=jac1*der;                  % Derivative of shape functions 
                                       % in global coordinates
      [bee,radius]=formbee_axi(deriv,nne,fun, coord,eldof);%Form matrix [B]
      eps=bee*eld                      % Compute strains
      sigma=dee*eps                    % Compute stresses
  end
  end
  SIGMA(i,:)=sigma ;           % Store stresses for all elements
end
%
%
[ZX, ZY,  Z_THETA, ZT] = stresses_at_nodes_axi(SIGMA);
%
%
% Plot stresses in the x_direction 
%
U2 = DISP(:,2);
cmin = min(ZT);
cmax = max(ZT);
caxis([cmin cmax]);
patch('Faces', connec, 'Vertices', geom, 'FaceVertexCData',ZT,...
                             'Facecolor','interp','Marker','.');
colorbar;