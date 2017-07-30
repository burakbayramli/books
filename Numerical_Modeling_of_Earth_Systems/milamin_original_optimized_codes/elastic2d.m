function [ Disp ] = elastic2d(ELEM2NODE, GCOORD, Phases, ...
        material, Bc_ind, Bc_val, ...,
        nip, reorder, method)

    %
    %
    % linear elastic (compressible) solver, derived from mechanical2d
    %
    
   
    
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

nelblo      = 400;
%==========================================================================
% BLOCKING PARAMETERS (nelblo must be < nel)
%========================%A_==================================================
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
K_all       = zeros(nedof*(nedof+1)/2,nel); % upper triangle 
Rhs_all     = zeros(nedof,nel); % solution
%==========================================================================
% INDICES EXTRACTING LOWER PART
%==========================================================================
indx_l = tril(ones(nedof)); indx_l = indx_l(:); indx_l = indx_l==1;


EG        = [ 0 -1 ]'; % element gravity


switch method
    %======================================================================
    % STANDARD VERSION
    %======================================================================
    case 'std'
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

        
    %======================================================================
    % OPTIMIZED VERSION
    %======================================================================
    case 'opt'
        %==================================================================
        % DECLARE VARIABLES (ALLOCATE MEMORY)
        %==================================================================
        K_block     = zeros(nelblo, nedof*(nedof+1)/2);
        Rhs_block   = zeros(nelblo, nedof);

        K_all       = zeros(nedof*(nedof+1)/2,nel);
        Rhs_all     = zeros(nedof, nel);
   
        il          = 1;
        iu          = nelblo;
        %==================================================================
        % i) BLOCK LOOP - MATRIX COMPUTATION
        %==================================================================

        fprintf(1, 'MATRIX COMPUTATION: '); tic;
        for ib = 1:nblo
            %==============================================================
            % ii) FETCH DATA OF ELEMENTS IN BLOCK
            %==============================================================
            ECOORD_x = reshape( GCOORD(1,ELEM2NODE(:,il:iu)), nnodel, nelblo);
            ECOORD_y = reshape( GCOORD(2,ELEM2NODE(:,il:iu)), nnodel, nelblo);
           
            [ DM ED ] = calc_el_D(material.Mu(Phases(il:iu)),...
                material.nu(Phases(il:iu)),material.plane_strain);
            C1 = DM(:,1,1);
            C2 = DM(:,1,2);
            
            ERho     = material.Rho(Phases(il:iu));
            %==============================================================
            % iii) INTEGRATION LOOP
            %==============================================================
            K_block(:)      = 0;
            Rhs_block(:)    = 0;

            a23   = ECOORD_x(2,:).*ECOORD_y(3,:) - ECOORD_x(3,:).*ECOORD_y(2,:);
            a31   = ECOORD_x(3,:).*ECOORD_y(1,:) - ECOORD_x(1,:).*ECOORD_y(3,:);
            a12   = ECOORD_x(1,:).*ECOORD_y(2,:) - ECOORD_x(2,:).*ECOORD_y(1,:);
            area  = a23 + a31 + a12;

            for ip=1:nip
                %==========================================================
                % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
                %==========================================================
                Ni      =        N{ip};
                dNdui   =     dNdu{ip};
                GIP_x   = Ni'*ECOORD_x;
                GIP_y   = Ni'*ECOORD_y;

                tmp   = ECOORD_x(3,:).*GIP_y - GIP_x.*ECOORD_y(3,:);
                eta1  = a23 + tmp + ECOORD_y(2,:).*GIP_x - GIP_y.*ECOORD_x(2,:);
                eta2  = a31 - tmp + ECOORD_x(1,:).*GIP_y - GIP_x.*ECOORD_y(1,:);

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
                weight      = IP_w(ip)*detJ;
                weightD     =    weight.*ED';

                % ------------------------A matrix-------------------------
                indx  = 1;
                for i = 1:nnodel
                    % x-velocity equation
                    for j = i:nnodel
                        K_block(:,indx) = K_block(:,indx) + ( C1.*dNdx(:,i).*dNdx(:,j) + dNdy(:,i).*dNdy(:,j)).*weightD;
                        indx = indx+1;
                        K_block(:,indx) = K_block(:,indx) + ( C2.*dNdx(:,i).*dNdy(:,j) + dNdy(:,i).*dNdx(:,j)).*weightD;
                        indx = indx+1;
                    end
                    % y-velocity equation
                    for j = i:nnodel
                        if(j>i)
                            K_block(:,indx) = K_block(:,indx) + ( C2.*dNdy(:,i).*dNdx(:,j) + dNdx(:,i).*dNdy(:,j)).*weightD ;
                            indx = indx+1;
                        end
                        K_block(:,indx) = K_block(:,indx) + ( C1.*dNdy(:,i).*dNdy(:,j) + dNdx(:,i).*dNdx(:,j)).*weightD;
                        indx = indx+1;
                    end
                end

                % -----------------------Rhs vector------------------------
                Rhs_block(:,1:2:nedof) = Rhs_block(:,1:2:nedof) + EG(1)*(ERho.*weight)*Ni';
                Rhs_block(:,2:2:nedof) = Rhs_block(:,2:2:nedof) + EG(2)*(ERho.*weight)*Ni';

            end


            %==============================================================
            % ix) WRITE DATA INTO GLOBAL STORAGE
            %==============================================================
            K_all(:,il:iu)  = K_block';
            Rhs_all(:,il:iu)  = Rhs_block';

            %==============================================================
            % READJUST START, END AND SIZE OF BLOCK. REALLOCATE MEMORY
            %==============================================================
            il  = il+nelblo;
            if(ib==nblo-1)
                nelblo 	   = nel-iu;
                K_block = zeros(nelblo, nedof*(nedof+1)/2);
                Rhs_block = zeros(nelblo, nedof);
                invJx      = zeros(nelblo, ndim);
                invJy      = zeros(nelblo, ndim);
            end
            iu  = iu+nelblo;
        end        
        fprintf(1, [num2str(toc),'\n']);

end

%==========================================================================
% ix) CREATE TRIPLET FORMAT INDICES
%==========================================================================
tic; fprintf(1, 'TRIPLET INDICES:    ');
%A matrix
ELEM_DOF = zeros(nedof, nel,'int32');
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
K    = sparse2(K_i(:)   ,    K_j(:),    K_all(:));
Rhs  = accumarray(ELEM_DOF(:), Rhs_all(:));
clear ELEM_DOF K_i K_j K_all Rhs_all;
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% BOUNDARY CONDITIONS
%==========================================================================
fprintf(1, 'BDRY CONDITIONS:    '); tic;
Free        = 1:sdof;
Free(Bc_ind)= [];
TMP         = K(:,Bc_ind) + cs_transpose(K(Bc_ind,:));
Rhs         = Rhs - TMP*Bc_val';  
K           = K(Free,Free);
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% REORDERING
%==========================================================================
fprintf(1, 'REORDERING:         '); tic;
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
%========================================================================
fprintf(1, 'FACTORIZATION:      '); tic;
K = cs_transpose(K);
K = cs_symperm(K,perm);
K = cs_transpose(K);
L = lchol(K);
fprintf(1, [num2str(toc,'%8.6f'),'\n']);

fprintf(1, 'SOLVING:      '); tic;

Disp         = zeros(sdof  , 1);
Disp(Bc_ind) = Bc_val;
Disp(Free(perm)) = cs_ltsolve(L,cs_lsolve(L,Rhs(Free(perm))));          %BACK & FORWARD SUBS   
fprintf(1, [num2str(toc,'%8.6f'),'\n']);


end





