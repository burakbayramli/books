function [Vel Pressure] = mechanical2d_std(ELEM2NODE, Phases, GCOORD, ...
    parameters, Bc_ind, Bc_val, nip)
%
% simplified version
%


%MECHANICAL2D Two dimensional finite element mechanical problem solver of MILAMIN

%   Part of MILAMIN: MATLAB-based FEM solver for large problems, Version 1.0
%   Copyright (C) 2007, M. Dabrowski, M. Krotkiewski, D.W. Schmid
%   University of Oslo, Physics of Geological Processes
%   http://milamin.org
%   See License file for terms of use.

%==========================================================================
% MODEL INFO
%==========================================================================
nnod        = size(GCOORD,2); % number of nodes
nel         = size(ELEM2NODE,2); % number elements

%==========================================================================
% CONSTANTS
%==========================================================================
ndim        = 2; % number of dimensions
nnodel      = size(ELEM2NODE,1); % number of nodes per element
nedof       = nnodel*ndim; % number of elemental degrees of freedom 
sdof        = 2*nnod; % global nodal number of degrees of freedom
%
np          = 3; % nodes that enter into the pressure formulation 
% (linear, discontinuous)

% this is the material matrix 
DEV   = [ 4/3 -2/3 0;...
         -2/3  4/3 0;...
            0    0 1];

%
%
%
PF = 1e3*max(parameters.Eta);  % fake compressibility

%
% constants
%
C1 = 4/3;
C2 = 2/3;

nelblo      = 400;
%==========================================================================
% BLOCKING PARAMETERS (nelblo must be < nel)
%==========================================================================
nelblo          = min(nel, nelblo);
nblo            = ceil(nel/nelblo);

%==========================================================================
% i) PREPARE INTEGRATION POINTS & DERIVATIVES wrt LOCAL COORDINATES
%==========================================================================
[IP_X, IP_w]    = ip_triangle(nip);                   
% get shape functions and shape function derivatives, evaluated at
% the element-local coordinates for the Gauss points and all of the nodes of the element
[   N, dNdu]    = shp_deriv_triangle(IP_X, nnodel);   

%==========================================================================
% DECLARE VARIABLES (ALLOCATE MEMORY)
%==========================================================================
A_all       = zeros(nedof*(nedof+1)/2,nel); % upper triangle for K_bar
Q_all       = zeros(nedof*np,nel); % integral over divergence from flow shape function times pressure shape function
invM_all    = zeros(np*np,nel); % inverse of M, the pressure shape function inner product
Rhs_all     = zeros(nedof,nel); % solution
%==========================================================================
% INDICES EXTRACTING LOWER PART
%==========================================================================
indx_l = tril(ones(nedof)); indx_l = indx_l(:); indx_l = indx_l==1;


EG        = [ 0 parameters.Gz ]'; % element gravity


%==================================================================
% DECLARE VARIABLES (ALLOCATE MEMORY)
%==================================================================
A_elem      = zeros(nedof,nedof);
Q_elem      = zeros(nedof,np);
M_elem      = zeros(np,np);
Rhs_elem    = zeros(ndim,nnodel);

B           = zeros(nedof,ndim*(ndim+1)/2);
P           = ones(np);
Pb          = ones(np,1);

%==================================================================
% i) ELEMENT LOOP - MATRIX COMPUTATION
%==================================================================
fprintf(1, 'MATRIX COMPUTATION: '); tic;
for iel = 1:nel
  %==============================================================
  % ii) FETCH DATA OF ELEMENT
  %==============================================================
  ECOORD_X  = GCOORD(:,ELEM2NODE(:,iel));
  EEta      = parameters.Eta(Phases(iel));% element viscosity
  ERho      = parameters.Rho(Phases(iel)); % element density

  %==============================================================
  % iii) INTEGRATION LOOP
  %==============================================================
  A_elem(:) = 0;
  Q_elem(:) = 0;
  M_elem(:) = 0;
  Rhs_elem(:) = 0;

  P(2:3,:) = ECOORD_X(:,1:3);% node coordinates at pressure points
  for ip=1:nip % loop over integration points
    %==========================================================
    % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
    %==========================================================
    Ni       =       N{ip}; % shape function for each of the lement nodes at integration point
    dNdui       =    dNdu{ip}; % derivatives 
    
    Pb(2:3)     = ECOORD_X*Ni; % center of element
    Pi          =   P\Pb;

    %==========================================================
    % v) CALCULATE JACOBIAN, ITS DETERMINANT AND INVERSE
    %==========================================================
    J           = ECOORD_X*dNdui;
    detJ        = det(J);
    invJ        = inv(J);

    %==========================================================
    % vi) DERIVATIVES wrt GLOBAL COORDINATES
    %==========================================================
    dNdX        = dNdui*invJ;

    %==========================================================
    % vii) NUMERICAL INTEGRATION OF ELEMENT MATRICES
    %==========================================================                
    weight       = IP_w(ip)*detJ;
    B(1:2:end,1) = dNdX(:,1);
    B(2:2:end,2) = dNdX(:,2);
    B(1:2:end,3) = dNdX(:,2);
    B(2:2:end,3) = dNdX(:,1);
    Bvol         = dNdX';
    
    A_elem       = A_elem + weight*EEta*(B*DEV*B');
    Q_elem       = Q_elem - weight*Bvol(:)*Pi';
    M_elem       = M_elem + weight*Pi*Pi';
    Rhs_elem     = Rhs_elem + weight*ERho*EG*Ni';
  end
%==============================================================
% viii) STATIC CONDENSATION
%==============================================================
invM_elem = inv(M_elem);
A_elem    = A_elem + PF*Q_elem*invM_elem*Q_elem';

%==============================================================
% ix) WRITE DATA INTO GLOBAL STORAGE
%==============================================================
A_all(:, iel)      = A_elem(indx_l);
Q_all(:, iel)      = Q_elem(:);
invM_all(:,iel)    = invM_elem(:);
Rhs_all(:,iel)     = Rhs_elem(:);
end
fprintf(1, [num2str(toc),'\n']);

        


%==========================================================================
% ix) CREATE TRIPLET FORMAT INDICES
%==========================================================================
tic; fprintf(1, 'TRIPLET INDICES:    ');
%A matrix
ELEM_DOF = zeros(nedof, nel);
ELEM_DOF(1:ndim:end,:) = ndim*(ELEM2NODE-1)+1;
ELEM_DOF(2:ndim:end,:) = ndim*(ELEM2NODE-1)+2;
indx_j = repmat(1:nedof,nedof,1); indx_i = indx_j';
indx_i = tril(indx_i); indx_i = indx_i(:); indx_i = indx_i(indx_i>0);
indx_j = tril(indx_j); indx_j = indx_j(:); indx_j = indx_j(indx_j>0);

A_i = ELEM_DOF(indx_i(:),:);
A_j = ELEM_DOF(indx_j(:),:);

indx       = A_i < A_j;
tmp        = A_j(indx);
A_j(indx)  = A_i(indx);
A_i(indx)  = tmp;

%Q matrix
Q_i = repmat(1:nel*np,nedof,1);
Q_j = repmat(ELEM_DOF,np,1);

%invM matrix
indx_j = repmat(1:np,np,1); indx_i = indx_j';
invM_i = reshape(1:nel*np,np, nel);
invM_j = invM_i(indx_i,:);
invM_i = invM_i(indx_j,:);

fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% x) CONVERT TRIPLET DATA TO SPARSE MATRIX
%==========================================================================
fprintf(1, 'SPARSIFICATION:     '); tic
A    = sparse(A_i(:)   ,    A_j(:),    A_all(:));
Q    = sparse(Q_i(:)   ,    Q_j(:),    Q_all(:));
invM = sparse(invM_i(:), invM_j(:), invM_all(:));
Rhs  = accumarray(ELEM_DOF(:), Rhs_all(:));
clear ELEM_DOF A_i A_j A_all Q_i Q_j Q_all invM_i invM_j invM_all Rhs_all;
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% BOUNDARY CONDITIONS
%==========================================================================
fprintf(1, 'BDRY CONDITIONS:    '); tic;
Free        = 1:sdof;
Free(Bc_ind)= [];
TMP         = A(:,Bc_ind) + transpose(A(Bc_ind,:));
Rhs         = Rhs - TMP*Bc_val';  
A           = A(Free,Free);
fprintf(1, [num2str(toc),'\n']);

%
% uses a highly inefficient solver
%
% fill in upper triangular part
A = A + triu(A',1);

%==========================================================================
% POWELL-HESTENES ITERATIONS
%==========================================================================
tic
div_max_uz  = 1e-10; div_max     = realmax;
uz_iter     =     0; uz_iter_max =       3;

Pressure    = zeros(nel*np, 1);
Vel         = zeros(sdof  , 1);
Vel(Bc_ind) = Bc_val;

while (div_max>div_max_uz  && uz_iter<uz_iter_max)
    uz_iter         = uz_iter + 1;
    
    Vel(Free) = A\Rhs(Free);
    Div             = invM*(Q*Vel);                            % COMPUTE QUASI-DIVERGENCE    
    Rhs             = Rhs - PF*(Q'*Div);                       % UPDATE RHS 
    Pressure        = Pressure + PF*Div;                       % UPDATE TOTAL PRESSURE (negative sign convention)
    div_max         = max(abs(Div(:)))                         % CHECK INCOMPRESSIBILITY
    disp([' PH_ITER: ', num2str(uz_iter), ' ', num2str(div_max)]); 
end
Pressure = reshape(Pressure,np, nel);
fprintf(1, 'P-H ITERATIONS:     '); 
fprintf(1, [num2str(toc,'%8.6f'),'\n']);





