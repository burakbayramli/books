function [Vel Pressure] = mechanical2d(ELEM2NODE, Phases, GCOORD, ...
        parameters, Bc_ind, Bc_val, ...,
        nip, reorder, method)
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
np          = 3; % nodes that enter into the pressure formulation 
                 % (linear, discontinuous)

                 % this is the material matrix 
DEV   = [ 4/3 -2/3 0;...
         -2/3  4/3 0;...
            0    0 1];

PF = 1e3*max(parameters.Mu);  % fake compressibility

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


EG        = [ 0 parameters.Gy ]'; % element gravity


switch method
    %======================================================================
    % STANDARD VERSION
    %======================================================================
    case 'std'
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
            EMu       = parameters.Mu(Phases(iel));% element viscosity
            % depth dependent viscosity 
            ymean = mean(ECOORD_X(2,:));
            if(ymean > 0.9)
                EMu = EMu * parameters.Muz(1);
            elseif(ymean > 0.8)
                EMu = EMu * parameters.Muz(2);
            else
                EMu = EMu * parameters.Muz(3);
            end
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
                                
                A_elem       = A_elem + weight*EMu*(B*DEV*B');
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

        
    %======================================================================
    % OPTIMIZED VERSION
    %======================================================================
    case 'opt'
        %==================================================================
        % DECLARE VARIABLES (ALLOCATE MEMORY)
        %==================================================================
        A_block     = zeros(nelblo, nedof*(nedof+1)/2);
        Q_block     = zeros(nelblo, np*nedof);
        M_block     = zeros(nelblo, np*(np+1)/2);
        invM_block  = zeros(nelblo, np*np);
        invMQ_block = zeros(nelblo, np*nedof);
        Pi_block    = zeros(nelblo, np);
        Rhs_block   = zeros(nelblo, nedof);

        A_all       = zeros(nedof*(nedof+1)/2,nel);
        Q_all       = zeros(nedof*np, nel);
        invM_all    = zeros(np*np, nel);
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
            ED       = parameters.Mu(Phases(il:iu));
            
            ymean = mean(ECOORD_y);
            ind=find(ymean>0.9);ED(ind)=ED(ind)*parameters.Muz(1);
            ind=find(ymean<=0.9 & ymean >0.8);ED(ind)=ED(ind)*parameters.Muz(1);
%            ind=find(ymean<=0.9);ind2=find(ymean(ind)>0.8);ind=ind(ind2);ED(ind)=ED(ind)*parameters.Muz(2);
            ind=find(ymean<=0.8);ED(ind)=ED(ind)*parameters.Muz(3);
            
            ERho     = parameters.Rho(Phases(il:iu));
            %==============================================================
            % iii) INTEGRATION LOOP
            %==============================================================
            A_block(:)      = 0;
            Q_block(:)      = 0;
            M_block(:)      = 0;
            invM_block(:)   = 0;
            invMQ_block(:)  = 0;
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

                Pi_block(:,1) = eta1./area;
                Pi_block(:,2) = eta2./area;
                Pi_block(:,3) = 1 - Pi_block(:,1) - Pi_block(:,2);

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
                weightD     =    weight.*ED;

                % ------------------------A matrix-------------------------
                indx  = 1;
                for i = 1:nnodel
                    % x-velocity equation
                    for j = i:nnodel
                        A_block(:,indx) = A_block(:,indx) + ( C1.*dNdx(:,i).*dNdx(:,j) + dNdy(:,i).*dNdy(:,j)).*weightD;
                        indx = indx+1;
                        A_block(:,indx) = A_block(:,indx) + (-C2.*dNdx(:,i).*dNdy(:,j) + dNdy(:,i).*dNdx(:,j)).*weightD;
                        indx = indx+1;
                    end
                    % y-velocity equation
                    for j = i:nnodel
                        if(j>i)
                            A_block(:,indx) = A_block(:,indx) + (-C2.*dNdy(:,i).*dNdx(:,j) + dNdx(:,i).*dNdy(:,j)).*weightD;
                            indx = indx+1;
                        end
                        A_block(:,indx) = A_block(:,indx) + ( C1.*dNdy(:,i).*dNdy(:,j) + dNdx(:,i).*dNdx(:,j)).*weightD;
                        indx = indx+1;
                    end
                end

                % ------------------------Q matrix-------------------------
                for i=1:np
                    TMP1 = weight.*Pi_block(:,i);
                    TMP2 = TMP1(:,ones(1,nnodel));
                    Q_block(:,(i-1)*nedof + (1:2:nedof)) =  Q_block(:,(i-1)*nedof + (1:2:nedof)) - TMP2.*dNdx;
                    Q_block(:,(i-1)*nedof + (2:2:nedof)) =  Q_block(:,(i-1)*nedof + (2:2:nedof)) - TMP2.*dNdy;
                end

                % ------------------------M matrix-------------------------
                indx = 1;
                for i = 1:np
                    for j = i:np
                        M_block(:,indx) = M_block(:,indx) + weight.*Pi_block(:,i).*Pi_block(:,j);
                        indx = indx + 1;
                    end
                end

                % -----------------------Rhs vector------------------------
                Rhs_block(:,1:2:nedof) = Rhs_block(:,1:2:nedof) + EG(1)*(ERho.*weight)*Ni';
                Rhs_block(:,2:2:nedof) = Rhs_block(:,2:2:nedof) + EG(2)*(ERho.*weight)*Ni';

            end

            %==============================================================
            % viii) STATIC CONDENSATION
            %==============================================================

            % --------------------------invM-------------------------------
            TMP     = 1./area';
            M_block = M_block.*TMP(:,ones(1,np*(np+1)/2));

            detM_block = M_block(:,1).*(M_block(:,4).*M_block(:,6) - M_block(:,5).*M_block(:,5)) + ...
                M_block(:,2).*(M_block(:,5).*M_block(:,3) - M_block(:,2).*M_block(:,6)) + ...
                M_block(:,3).*(M_block(:,2).*M_block(:,5) - M_block(:,4).*M_block(:,3));

            detM_block = detM_block./TMP;
            invM_block(:,1) = (M_block(:,4).*M_block(:,6) - M_block(:,5).*M_block(:,5))./detM_block;
            invM_block(:,2) = (M_block(:,5).*M_block(:,3) - M_block(:,2).*M_block(:,6))./detM_block;
            invM_block(:,3) = (M_block(:,2).*M_block(:,5) - M_block(:,4).*M_block(:,3))./detM_block;
            invM_block(:,4) = invM_block(:,2);
            invM_block(:,5) = (M_block(:,1).*M_block(:,6) - M_block(:,3).*M_block(:,3))./detM_block;
            invM_block(:,6) = (M_block(:,2).*M_block(:,3) - M_block(:,1).*M_block(:,5))./detM_block;
            invM_block(:,7) = invM_block(:,3);
            invM_block(:,8) = invM_block(:,6);
            invM_block(:,9) = (M_block(:,1).*M_block(:,4) - M_block(:,5).*M_block(:,5))./detM_block;

            % --------------------------invM*Q'----------------------------
            for i=1:np
                for j=1:nedof
                    for k=1:np
                        invMQ_block(:,(i-1)*nedof+j) = invMQ_block(:,(i-1)*nedof+j) + invM_block(:,(i-1)*np+k).*Q_block(:,(k-1)*nedof+j);
                    end
                end
            end

            % -------------------A = A + PF*Q'*invM*Q'---------------------
            indx = 1;
            for i=1:nedof
                for j=i:nedof
                    for k=1:np
                        A_block(:,indx) = A_block(:,indx) + PF*Q_block(:,(k-1)*nedof+i).*invMQ_block(:,(k-1)*nedof+j);
                    end
                    indx = indx + 1;
                end
            end

            %==============================================================
            % ix) WRITE DATA INTO GLOBAL STORAGE
            %==============================================================
            A_all(:,il:iu)  = A_block';
            Q_all(:,il:iu)  = Q_block';
            invM_all(:,il:iu)  = invM_block';
            Rhs_all(:,il:iu)  = Rhs_block';

            %==============================================================
            % READJUST START, END AND SIZE OF BLOCK. REALLOCATE MEMORY
            %==============================================================
            il  = il+nelblo;
            if(ib==nblo-1)
                nelblo 	   = nel-iu;
                A_block = zeros(nelblo, nedof*(nedof+1)/2);
                Q_block = zeros(nelblo, np*nedof);
                M_block = zeros(nelblo, np*(np+1)/2);
                invM_block = zeros(nelblo, np*np);
                invMQ_block = zeros(nelblo, np*nedof);
                Pi_block  = zeros(nelblo, np);
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

A_i = ELEM_DOF(indx_i(:),:);
A_j = ELEM_DOF(indx_j(:),:);

indx       = A_i < A_j;
tmp        = A_j(indx);
A_j(indx)  = A_i(indx);
A_i(indx)  = tmp;

%Q matrix
Q_i = repmat(int32(1:nel*np),nedof,1);
Q_j = repmat(ELEM_DOF,np,1);

%invM matrix
indx_j = repmat(1:np,np,1); indx_i = indx_j';
invM_i = reshape(int32(1:nel*np),np, nel);
invM_j = invM_i(indx_i,:);
invM_i = invM_i(indx_j,:);

fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% x) CONVERT TRIPLET DATA TO SPARSE MATRIX
%==========================================================================
fprintf(1, 'SPARSIFICATION:     '); tic
A    = sparse2(A_i(:)   ,    A_j(:),    A_all(:));
Q    = sparse2(Q_i(:)   ,    Q_j(:),    Q_all(:));
invM = sparse2(invM_i(:), invM_j(:), invM_all(:));
Rhs  = accumarray(ELEM_DOF(:), Rhs_all(:));
clear ELEM_DOF A_i A_j A_all Q_i Q_j Q_all invM_i invM_j invM_all Rhs_all;
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% BOUNDARY CONDITIONS
%==========================================================================
fprintf(1, 'BDRY CONDITIONS:    '); tic;
Free        = 1:sdof;
Free(Bc_ind)= [];
TMP         = A(:,Bc_ind) + cs_transpose(A(Bc_ind,:));
Rhs         = Rhs - TMP*Bc_val';  
A           = A(Free,Free);
fprintf(1, [num2str(toc),'\n']);

%==========================================================================
% REORDERING
%==========================================================================
fprintf(1, 'REORDERING:         '); tic;
switch reorder
    case 'metis'
        perm = metis(A);
    case 'amd'
        perm = amd(A);
    otherwise
        error('Unknown reordering')
end
fprintf(1, [num2str(toc),'\n']);
 
%==========================================================================
% FACTORIZATION - ideally L = lchol(K, perm)
%========================================================================
fprintf(1, 'FACTORIZATION:      '); tic;
A = cs_transpose(A);
A = cs_symperm(A,perm);
A = cs_transpose(A);
L = lchol(A);
fprintf(1, [num2str(toc,'%8.6f'),'\n']);
  
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
    Vel(Free(perm)) = cs_ltsolve(L,cs_lsolve(L,Rhs(Free(perm))));          %BACK & FORWARD SUBS   
    Div             = invM*(Q*Vel);                                        %COMPUTE QUASI-DIVERGENCE    
    Rhs             = Rhs - PF*(Q'*Div);                                   %UPDATE RHS 
    Pressure        = Pressure + PF*Div;                                   %UPDATE TOTAL PRESSURE (negative sign convention)
    div_max         = max(abs(Div(:)))                                    %CHECK INCOMPRESSIBILITY
    disp([' PH_ITER: ', num2str(uz_iter), ' ', num2str(div_max)]); 
end
Pressure = reshape(Pressure,np, nel);
fprintf(1, 'P-H ITERATIONS:     '); 
fprintf(1, [num2str(toc,'%8.6f'),'\n']);





