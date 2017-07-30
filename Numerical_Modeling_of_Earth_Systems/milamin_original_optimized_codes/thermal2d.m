function T = thermal2d(ELEM2NODE, Phases, GCOORD, D, ...
        Bc_ind, Bc_val, nip, reorder, method)
    

% THERMAL2D Two dimensional finite element thermal problem solver of MILAMIN


%z
% elem2node: element connectivity
% Phases: element material type
% gcoord: global nodal coordinates
% D: material property array
% Bc_ind: nodal bc codes
% Bc_val: nodel bc values
% nip: number of integration points
% reorder: reordering method
% method: solver method

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
nelblo       = 760;			% blocking size for partitioned solver

%==========================================================================
% BLOCKING PARAMETERS (nelblo must be < nel)
%==========================================================================
nelblo       = min(nel, nelblo);
nblo         = ceil(nel/nelblo);

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
Rhs          = zeros(nnod,1);		% number of nodes where temperatures are to be obtained

%==========================================================================
% INDICES EXTRACTING LOWER PART
%==========================================================================
indx_l       = tril(ones(nnodel));	% lower triangular part of nel x nel matrix
                                        % all entries above the
                                        % diagonal are set to zero
indx_l = indx_l(:); % turn matrix into a vector 
indx_l = indx_l==1;			% turn into logical array, true for 
                                        % all entries below diagonal in matrix


switch method
    %======================================================================
    % STANDARD VERSION
    %======================================================================        
    case 'std'        
        %==================================================================
        % DECLARE VARIABLES (ALLOCATE MEMORY)
        %==================================================================
        K_elem      = zeros(nnodel,nnodel);   
        
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

            %==============================================================
            % iii) INTEGRATION LOOP
            %==============================================================
            K_elem(:) = 0;
            for ip=1:nip		% loop through Gauss points
                %==========================================================
                % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
                %==========================================================
                dNdui       = dNdu{ip};

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
            end

            %==============================================================
            % ix) WRITE DATA INTO GLOBAL STORAGE
            %==============================================================
            K_all(:,iel)    = K_elem(indx_l);
        end
        fprintf(1, [num2str(toc),'\n']);
                
    %======================================================================
    % OPTIMIZED VERSION
    %======================================================================    
    case 'opt'        
        %==================================================================
        % DECLARE VARIABLES (ALLOCATE MEMORY)
        %==================================================================        
        K_block     = zeros(nelblo,nnodel*(nnodel+1)/2);
        invJx       = zeros(nelblo, ndim);
        invJy       = zeros(nelblo, ndim);
        il          = 1;
        iu          = nelblo;

        %==================================================================
        % i) BLOCK LOOP - MATRIX COMPUTATION
        %==================================================================
        fprintf(1, 'MATRIX ASSEMBLY:    '); tic;
        for ib = 1:nblo
            %==============================================================
            % ii) FETCH DATA OF ELEMENTS IN BLOCK
            %==============================================================
            ECOORD_x = reshape( GCOORD(1,ELEM2NODE(:,il:iu)), nnodel, nelblo);
            ECOORD_y = reshape( GCOORD(2,ELEM2NODE(:,il:iu)), nnodel, nelblo);
            ED       = reshape(D(Phases(il:iu)),nelblo,1);

            %==============================================================
            % iii) INTEGRATION LOOP
            %==============================================================
            K_block(:)  = 0;
            for ip=1:nip
                %==========================================================
                % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
                %==========================================================
                dNdui       = dNdu{ip};

                %==========================================================
                % v) CALCULATE JACOBIAN, ITS DETERMINANT AND INVERSE
                %==========================================================
                Jx          = ECOORD_x'*dNdui;
                Jy          = ECOORD_y'*dNdui;
                detJ        = Jx(:,1).*Jy(:,2) - Jx(:,2).*Jy(:,1);

                invdetJ     = 1.0./detJ;
                invJx(:,1)  = +Jy(:,2).*invdetJ;
                invJx(:,2)  = -Jy(:,1).*invdetJ;
                invJy(:,1)  = -Jx(:,2).*invdetJ;
                invJy(:,2)  = +Jx(:,1).*invdetJ;

                %==========================================================
                % vi) DERIVATIVES wrt GLOBAL COORDINATES
                %==========================================================
                dNdx        = invJx*dNdui';
                dNdy        = invJy*dNdui';

                %==========================================================
                % vii) NUMERICAL INTEGRATION OF ELEMENT MATRICES 
                %==========================================================
                weight      = IP_w(ip)*detJ.*ED;

                indx = 1;
                for i = 1:nnodel
                    for j = i:nnodel
                        K_block(:,indx)  =   K_block(:,indx) + ...
                            (dNdx(:,i).*dNdx(:,j)+ dNdy(:,i).*dNdy(:,j)).*weight;
                        indx = indx + 1;
                    end
                end
            end
            %==============================================================
            % ix) WRITE DATA INTO GLOBAL STORAGE
            %==============================================================
            K_all(:,il:iu)	= K_block';
            
            %==============================================================
            % READJUST START, END AND SIZE OF BLOCK. REALLOCATE MEMORY
            %==============================================================
            il  = il+nelblo;
            if(ib==nblo-1)
                nelblo 	= nel-iu;
                K_block	= zeros(nelblo, nnodel*(nnodel+1)/2);
                invJx   = zeros(nelblo, ndim);
                invJy   = zeros(nelblo, ndim);
            end
            iu  = iu+nelblo;
        end
        fprintf(1, [num2str(toc),'\n']);                
end

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
K      = sparse2(K_i, K_j, K_all);	% sparse representation of the K(nnod,nnod) matrix 
clear K_i K_j K_all;
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% BOUNDARY CONDITIONS
%==========================================================================
fprintf(1, 'BDRY CONDITIONS:    '); tic;
Free        = 1:nnod;
Free(Bc_ind)= [];			% delete the nodes with prescribed boundary value 
tmp         = K(:,Bc_ind) + cs_transpose(K(Bc_ind,:));
Rhs         = Rhs -  tmp*Bc_val';	% reduce load vector by solution
K          = K(Free,Free);
fprintf(1, [num2str(toc),'\n']);

use_cholesky=0;
switch use_cholesky
    case 1
    %    cholesky using cholmod

    %==========================================================================
    % REORDERING
    %==========================================================================
        fprintf(1, 'CHOLMOD SOLVER REORDERING:         '); tic;
        switch reorder
            case 'metis'
                perm = metis(K);
            case 'amd'
                perm = amd(K);
            otherwise
                error('Unknown reordering')
        end
        fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% FACTORIZATION - ideally L = lchol(K, perm)
%==========================================================================
        fprintf(1, 'FACTORIZATION:      '); %tic;
        K = cs_transpose(K); % convert to upper triag
        K = cs_symperm(K,perm); % get the permutation only accessing upper triag
        K = cs_transpose(K); % flip back
        L = lchol(K); % compute cholesky factorization
        fprintf(1, [num2str(toc,'%8.6f'),'\n']);

%==========================================================================
% SOLVE
%==========================================================================
        fprintf(1, 'SOLVE:              '); %tic;
        T             = zeros(nnod,1);
        T(Bc_ind)     = Bc_val;
        T(Free(perm)) = cs_ltsolve(L,cs_lsolve(L,Rhs(Free(perm))));
        fprintf(1, [num2str(toc,'%8.6f'),'\n']);
    case 0
        fprintf(1, 'REGULAR SOLVER:      '); tic;
        % regular solver for testing purpooses
        T             = zeros(nnod,1);
        T(Bc_ind)     = Bc_val;
            
       % fill in upper triangular part
        K = K + triu(K',1);
        T(Free) = K\Rhs(Free);

        fprintf(1, [num2str(toc,'%8.6f'),'\n']);
    case -1
        % this is in general not a good idea !
        fprintf(1, 'CG SOLVER:      '); tic;
        T             = zeros(nnod,1);
        T(Bc_ind)     = Bc_val;
        K = K + triu(K',1);
        T(Free) = cgs(K,Rhs(Free),1e-5,1000);
%        T(Free) = lsqr(K,Rhs(Free),1e-5,1000);

        fprintf(1, [num2str(toc,'%8.6f'),'\n']);

    otherwise
            error('unknown cholesky solver mode')

end
