function [ Disp ] = elastic2d_std(ELEM2NODE, GCOORD, Phases, ...
        material, Bc_ind, Bc_val,nip)

    %
    %
    % linear elastic (compressible) solver, derived from mechanical2d
    %
    % simplified version
    
   
    
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
K_all       = zeros(nedof*(nedof+1)/2,nel); % upper triangle 
Rhs_all     = zeros(nedof,nel); % solution
%==========================================================================
% INDICES EXTRACTING LOWER PART
%==========================================================================
indx_l = tril(ones(nedof)); indx_l = indx_l(:); indx_l = indx_l==1;


EG        = [ 0 -1 ]'; % element gravity


        %==================================================================
        % DECLARE VARIABLES (ALLOCATE MEMORY)
        %==================================================================
        K_elem      = zeros(nedof,nedof);
        Rhs_elem    = zeros(ndim,nnodel);
        B           = zeros(nedof,ndim*(ndim+1)/2);
                                
        %==================================================================
        % i) ELEMENT LOOP - MATRIX COMPUTATION
        %==================================================================
        fprintf(1, 'MATRIX COMPUTATION: '); tic;
        for iel = 1:nel
            %==============================================================
            % ii) FETCH DATA OF ELEMENT
            %==============================================================
            ECOORD_X  = GCOORD(:,ELEM2NODE(:,iel));
            ERho      = material.Rho(Phases(iel)); % element density
            [ D Dn ] = calc_el_D(material.Mu(Phases(iel)),material.nu(Phases(iel)),...
                material.plane_strain);

            %==============================================================
            % iii) INTEGRATION LOOP
            %==============================================================
            K_elem(:) = 0;
            Rhs_elem(:) = 0;

            for ip=1:nip % loop over integration points
                %==========================================================
                % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
                %==========================================================
                Ni       =       N{ip}; % shape function for each of the lement nodes at integration point
                dNdui       =    dNdu{ip}; % derivatives 
                
                %==========================================================
                % v) CALCULATE JACOBIAN, ITS DETERMINANT AND INVERSE
                %==========================================================
                J           = ECOORD_X*dNdui;
                detJ        = det2D(J);
                invJ        = inv2D(J,detJ);

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
                                
                K_elem       = K_elem + weight*Dn*(B*D*B');
                Rhs_elem     = Rhs_elem + weight*ERho*EG*Ni';
            end
            %==============================================================
            % ix) WRITE DATA INTO GLOBAL STORAGE
            %==============================================================
            K_all(:, iel)      = K_elem(indx_l);
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

K_i = ELEM_DOF(indx_i(:),:);
K_j = ELEM_DOF(indx_j(:),:);

indx       = K_i < K_j;
tmp        = K_j(indx);
K_j(indx)  = K_i(indx);
K_i(indx)  = tmp;

fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% x) CONVERT TRIPLET DATA TO SPARSE MATRIX
%==========================================================================
fprintf(1, 'SPARSIFICATION:     '); tic
K    = sparse(K_i(:)   ,    K_j(:),    K_all(:));
Rhs  = accumarray(ELEM_DOF(:), Rhs_all(:));
clear ELEM_DOF K_i K_j K_all Rhs_all;
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% BOUNDARY CONDITIONS
%==========================================================================
fprintf(1, 'BDRY CONDITIONS:    '); tic;
Free        = 1:sdof;
Free(Bc_ind)= [];
TMP         = K(:,Bc_ind) + transpose(K(Bc_ind,:));
Rhs         = Rhs - TMP*Bc_val';  
K           = K(Free,Free);
fprintf(1, [num2str(toc),'\n']);


%
% simplified solver (highly inefficient)
%
% add symmetric part

%==========================================================================
fprintf(1, 'SOLVING:      '); tic;
% fill in upper triangular part
K = K + triu(K',1);
% fixed values

Disp         = zeros(sdof  , 1);
Disp(Bc_ind) = Bc_val;
Disp(Free) = K\Rhs(Free);
fprintf(1, [num2str(toc,'%8.6f'),'\n']);


end





