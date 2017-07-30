function T = thermal2d_std(ELEM2NODE, Phases, GCOORD, D, H, ...
        Bc_ind, Bc_val, nip)
%
%
% this is the simplified version of thermal2d, without the better
% solver approaches
%
%

% THERMAL2D Two dimensional finite element thermal problem solver of MILAMIN


% elem2node: element connectivity
% Phases: element material type
% gcoord: global nodal coordinates
%
% D: material property array (dimensions: phases)
% H: source array (dimensions: phases)
% Bc_ind: nodal bc codes
% Bc_val: nodel bc values
% nip: number of integration points

%   Part of MILAMIN: MATLAB-based FEM solver for large problems, Version 1.0
%   Copyright (C) 2007, M. Dabrowski, M. Krotkiewski, D.W. Schmid
%   University of Oslo, Physics of Geological Processes
%   http://milamin.org
%   See License file for terms of use.

%==========================================================================
% MODEL INFO
%==========================================================================
nnod         = size(GCOORD,2);		% number of nodes
nnodel       = size(ELEM2NODE,1); % number of nodes per element
nel          = size(ELEM2NODE,2);	% number of elements

%==========================================================================
% CONSTANTS
%==========================================================================
ndim         =   2;			% number of dimensions

%==========================================================================
% PREPARE INTEGRATION POINTS & DERIVATIVES wrt LOCAL COORDINATES
%==========================================================================

% get integration node locations and weights
[IP_X, IP_w] = ip_triangle(nip);       
% get shape functions and shape function derivatives, evaluated at
% the element-local coordinates for all of the nodes of the element
[N dNdu]     = shp_deriv_triangle(IP_X, nnodel);   

%==========================================================================
% DECLARE VARIABLES (ALLOCATE MEMORY)
%==========================================================================
K_all        = zeros(nnodel*(nnodel+1)/2,nel); % (number of upper triangle nnodel*nnodel x nel)

%==========================================================================
% INDICES EXTRACTING LOWER PART
%==========================================================================
indx_l       = tril(ones(nnodel));	% lower triangular part of nnodel x nnodel matrix
                                        % all entries above the
                                        % diagonal are set to zero
indx_l = indx_l(:); % turn matrix into a vector 
indx_l = indx_l==1;			% turn into logical array, true for 
                                        % all entries below diagonal in matrix

%
% load vector
%
Rhs          = zeros(nnod,1);		% number of nodes where temperatures are to be obtained

%==================================================================
% DECLARE VARIABLES (ALLOCATE MEMORY)
%==================================================================
K_elem      = zeros(nnodel,nnodel);   
F_elem      = zeros(nnodel,1);

%==================================================================
% i) ELEMENT LOOP - MATRIX COMPUTATION
%==================================================================
fprintf(1, 'MATRIX ASSEMBLY:    '); tic;
for iel = 1:nel
  %==============================================================
  % ii) FETCH DATA OF ELEMENT
  %==============================================================
  ECOORD_X = GCOORD(:,ELEM2NODE(:,iel)); % dim x nnodel array of node locations
  ED       = D(Phases(iel));	% coductivity of element
  EH       = H(Phases(iel)); % constant heat production within each element
  
  
  %==============================================================
  % iii) INTEGRATION LOOP
  %==============================================================
  K_elem(:) = 0;
  F_elem(:) = 0;
  for ip=1:nip		% loop through Gauss points
    %==========================================================
    % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
    %==========================================================
    dNdui       = dNdu{ip};
    Ni          = N{ip}; %shape function
    
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
    K_elem      = K_elem + IP_w(ip)*detJ*ED*(dNdX*dNdX');
    F_elem      = F_elem + IP_w(ip)*detJ*(        EH*Ni);
  end
  
  %==============================================================
  % ix) WRITE DATA INTO GLOBAL STORAGE
  %==============================================================
  K_all(:,iel)    = K_elem(indx_l);
  Rhs(ELEM2NODE(:,iel)) = Rhs(ELEM2NODE(:,iel)) + F_elem;
end
fprintf(1, [num2str(toc),'\n']);
%==========================================================================
% ix) CREATE TRIPLET FORMAT INDICES
%==========================================================================
fprintf(1, 'TRIPLET INDICES:    '); tic
indx_j = repmat(1:nnodel,nnodel,1);	% matrix with colum (j) index in all rows
indx_i = indx_j';			% matrix with row (i) index in all columns
indx_i = tril(indx_i); % upper triag replaced with zeroes
indx_i = indx_i(:); % vector form
indx_i = indx_i(indx_i>0);		% only non-zero entries
indx_j = tril(indx_j); indx_j = indx_j(:); indx_j = indx_j(indx_j>0);

% together, those indices address 11, 21, 31, 22, 32, 33 combinations
% of the element nodes, for 3 nodes per elemen, for example. 
K_i = ELEM2NODE(indx_i,:); K_i = K_i(:); 
K_j = ELEM2NODE(indx_j,:); K_j = K_j(:);

% swap indices referring to upper triangle
indx       = K_i < K_j;
tmp        = K_j(indx);
K_j(indx)  = K_i(indx);
K_i(indx)  = tmp;
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% x) CONVERT TRIPLET DATA TO SPARSE MATRIX
%==========================================================================
fprintf(1, 'SPARSIFICATION:     '); tic
K_all  = K_all(:);
K      = sparse(K_i, K_j, K_all);	% sparse representation of the K(nnod,nnod) matrix 
clear K_i K_j K_all;
fprintf(1, [num2str(toc),'\n']);


%==========================================================================
% BOUNDARY CONDITIONS
%==========================================================================
fprintf(1, 'BDRY CONDITIONS:    '); tic;
Free        = 1:nnod;
Free(Bc_ind)= [];			% delete the nodes with prescribed boundary value 
tmp         = K(:,Bc_ind) + K(Bc_ind,:)';
Rhs     = Rhs -  tmp*Bc_val';	% reduce load vector by solution
K          = K(Free,Free);
fprintf(1, [num2str(toc),'\n']);



fprintf(1, 'REGULAR SOLVER:      '); tic;
% regular solver for testing purpooses
T             = zeros(nnod,1);
T(Bc_ind)     = Bc_val;

% fill in upper triangular part
K = K + triu(K',1);
T(Free) = K\Rhs(Free);

fprintf(1, [num2str(toc,'%8.6f'),'\n']);



