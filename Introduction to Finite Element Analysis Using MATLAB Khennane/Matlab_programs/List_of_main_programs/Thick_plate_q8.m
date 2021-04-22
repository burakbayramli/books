% THIS PROGRAM USES AN 8-NODED QUADRILATERAL ELEMENT FOR THE LINEAR ELASTIC 
% STATIC ANALYSIS OF A THICK PLATE IN BENDING
%
% Make these variables global so they can be shared by other functions
%
clc
clear all
%
global nnd nel nne nodof eldof n ngpb ngps
global geom connec deeb dees nf load dim
%
 format long g
%
% To cchange the size of the problem or change the elastic properties
%    ALTER the PlateQ8_input_module.m
%
dim = 2;
nne = 8;
nodof = 3;
eldof = nne*nodof;
%
% Plate_Q8_input_module
Length = 18.;    % Length of the in x-direction
Width =  18.;    % Width of the model in y-direction
NXE = 9;         % Number of rows in the x direction
NYE = 9;         % Number of rows in the y direction
dhx = Length/NXE; % Element size in the x direction
dhy = Width/NYE;  % Element size in the y direction
X_origin = 0. ;    % x origin of the global coordinate system
Y_origin = 0. ;    % y origin of the global coordinate system
%
thick = 0.25;  % Thickness of plate
ngpb = 3;          % number of Gauss points bending
ngps = 2;          % number of Gauss points for shear
%
Q8_mesh     % Generate the mesh 
%
E = 30.e+6;     % Elastic modulus in kN/m2
vu = 0.3;       % Poisson's ratio 
%
% Form the matrix of elastic properties
%
deeb=formdeeb(E,vu,thick); % Matrix of elastic properties for plate bending
dees=formdees(E,vu,thick); % Matrix of elastic properties for plate shear
%
% Boundary conditions
%
nf = ones(nnd, nodof);   % Initialise the matrix nf to 1
%
for i=1:nnd
if geom(i,1) == 0 
    nf(i,1) = 0 ;   % Restrain in direction w 
    nf(i,3) = 0 ;   % Restrain rotation theta_y (around x)
elseif geom(i,2) == 0 
    nf(i,1) = 0. ;   % Restrain displacement  w 
    nf(i,2) = 0. ;   % Restrain rotation theta_x (around y)  
elseif geom(i,1) == Length  
    nf(i,2) = 0. ;   % Restrain rotation theta_x (around y)  
elseif geom(i,2) == Width 
    nf(i,3) = 0. ;   % Restrain rotation theta_y (around x) 
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
disp ('Nodal freedom')
nf
disp ('Total number of active degrees of freedom')
n
%
% loading
%
load = zeros(nnd, 3);
%
for i=1:nnd
    if geom(i,1) == Length && geom(i,2) == Width
    load(i,1) = - 1000/4; % Vertical load of 250 lb on the centre node  
    end
end
%    
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%% End of input%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%
% Assemble the global force vector
%
fg=zeros(n,1);
for i=1: nnd
    for j=1:nodof
    if nf(i,j) ~= 0   
        fg(nf(i,j))= load(i,j);
    end
    end
end 
%
%  Form the matrix containing the abscissas and the weights of Gauss points
%
sampb=gauss(ngpb);
samps=gauss(ngps);
%
% Numerical integration and assembly of the global stiffness matrix
%
%  initialise the global stffness matrix to zero
kk = zeros(n, n);
%
for i=1:nel
    [coord,g] = platelem_q8(i) ;   % coordinates of the nodes of element i,
                                   % and its steering vector 
    keb=zeros(eldof,eldof) ;       % Initialise the element bending 
                                   % stiffness matrix to zero
    kes=zeros(eldof,eldof) ;       % Initialise the element Shear 
                                   %  stiffness matrix to zero
    %
    % Integrate element bending stiffness and assemble it in global matrix
    %
    for ig=1: ngpb
        wi = sampb(ig,2);
    for jg=1: ngpb
        wj=sampb(jg,2);
        [der,fun] = fmquad(sampb, ig,jg);  % Derivative of shape functions
                                           %  in local coordinates
        jac=der*coord;                     % Compute Jacobian matrix
        d=det(jac);                        % Compute the determinant of 
                                           % Jacobian matrix
        jac1=inv(jac);                     % Compute inverse of the Jacobian
        deriv=jac1*der;                    % Derivative of shape functions 
                                           % in global coordinates
        beeb=formbeeb(deriv,nne,eldof);    %  Form matrix [B]
        keb=keb + d*wi*wj*beeb'*deeb*beeb; % Integrate stiffness matrix
    end
    end
    kk=form_kk(kk,keb, g);             % assemble global stiffness matrix
    %
    % Integrate element Shear stiffness and assemble it in global matrix
    %
    for ig=1: ngps
        wi = samps(ig,2);
    for jg=1: ngps
        wj=samps(jg,2);
        [der,fun] = fmquad(samps, ig,jg);  % Derivative of shape functions
                                           % in local coordinates
        jac=der*coord;                     % Compute Jacobian matrix
        d=det(jac);                        % Compute determinant of 
                                           % Jacobian matrix
        jac1=inv(jac);                     % Compute inverse of the
                                           % Jacobian
        deriv=jac1*der;                    % Derivative of shape functions 
                                           % in global coordinates
        bees=formbees(deriv,fun,nne,eldof);          %  Form matrix [B]
        kes=kes + (5/6)*d*wi*wj*bees'*dees*bees; % Integrate stiffness matrix
    end
    end
    kk=form_kk(kk,kes, g);              % assemble global stiffness matrix
end
%
%
%%%%%%%%%%%%%%%%%%%%%%%  End of assembly %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
delta = kk\fg           % solve for unknown displacements

format short e
disp('node         w_disp       x_slope         y_slope ')     %
for i=1: nnd                                %
    if nf(i,1) == 0                         %
        w_disp =0.;                         %
    else
        w_disp = delta(nf(i,1));            %
    end
%    
    if nf(i,2) == 0                         %
        x_slope = 0.;                        %
    else
        x_slope = delta(nf(i,2));            %
    end
 %
    if nf(i,3) == 0                         %
        y_slope = 0.;                        %
    else
        y_slope = delta(nf(i,3));            %
    end   
    
disp([i  w_disp   x_slope  y_slope])  % Display displacements of each node
DISP(i,:) = [ w_disp   x_slope  y_slope];
end
%
%
%
ngp=1;                             % Calculate moments and shear forces
                                   % the centre of each element
samp=gauss(ngp);
%
for i=1:nel
  [coord,g] =  platelem_q8(i);  % coordinates of the nodes of element i, 
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
        [der,fun] = fmquad(samp, ig,jg);  % Derivative of shape functions
                                           %  in local coordinates
        jac=der*coord;                     % Compute Jacobian matrix
        d=det(jac);                        % Compute the determinant of 
                                           % Jacobian matrix
        jac1=inv(jac);                     % Compute inverse of the Jacobian
        deriv=jac1*der;                    % Derivative of shape functions 
                                           % in global coordinates
                                           %
        beeb=formbeeb(deriv,nne,eldof);    %  Form matrix [B_b]
        chi_b = beeb*eld  ;                % compute bending curvatures
        Moment = deeb*chi_b ;              % Compute moments
        bees=formbees(deriv,fun,nne,eldof); %  Form matrix [B_s]
        chi_s = bees*eld  ;                % compute shear curvatures
        Shear = dees*chi_s ;               % Compute shera forces
    end 
  end
  Element_Forces(i,:)=[Moment' Shear'];
end
%
W = DISP(:,1);
[MX, MY,  MXY, QX, QY] = Forces_at_nodes_plate(Element_Forces);
%
cmin = min(W);
cmax = max(W);
caxis([cmin cmax]);
patch('Faces', connec, 'Vertices', geom, 'FaceVertexCData',W,...
                             'Facecolor','interp','Marker','.');
colorbar;

