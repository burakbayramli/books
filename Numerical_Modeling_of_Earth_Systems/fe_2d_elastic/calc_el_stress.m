function [ ewx sigma ] = calc_el_stress(Disp, ELEM2NODE, GCOORD, Phases, ...
        material,  nip, nip_plot)
    %   
    % calculate stresses at element integration points 
    % for 2-D linear, isotropic elasticity
    % returns the location of the integration points, and three component
    % stress vector
    %
    % output is sxx syy shear=2sxy
    %
    % nip_plot is a vector with a list of which points to evaluates
    
    
    ndim        = 2; % number of dimensions
    nel         = size(ELEM2NODE,2); % number elements
    nnodel      = size(ELEM2NODE,1); % number of nodes per element
    nnodel2 = nnodel*2;
    nedof       = nnodel*ndim; % number of elemental degrees of freedom

    nuse_nip = size(nip_plot,1);
    
    sigma = zeros(nel*nuse_nip,3);
    ewx = zeros(nel*nuse_nip,ndim);

    % get Gaussian weight locations
    [IP_X, IP_w]    = ip_triangle(nip);
    % get shape function and derivatives
    [   N, dNdu]    = shp_deriv_triangle(IP_X, nnodel);
    B           = zeros(nedof,ndim*(ndim+1)/2);

    ind=1;
    for iel = 1:nel
          ECOORD_X  = GCOORD(:,ELEM2NODE(:,iel));
          % displacements at the element nodes
          EDisp(1:2:nnodel2-1) = Disp(ELEM2NODE(:,iel)*2-1);
          EDisp(2:2:nnodel2) = Disp(ELEM2NODE(:,iel)*2);

          [ D Dn ] = calc_el_D(material.Mu(Phases(iel)),material.nu(Phases(iel)),...
                material.plane_strain);
          for ip=nip_plot % loop over integration points
                %==========================================================
                % iv) LOAD SHAPE FUNCTIONS DERIVATIVES FOR INTEGRATION POINT
                %==========================================================
                Ni       =       N{ip}; % shape function for each of the lement nodes at integration point
                dNdui       =    dNdu{ip}; % derivatives 

                J           = ECOORD_X*dNdui;
                dNdX        = dNdui*inv2D(J,det2D(J));

                B(1:2:end,1) = dNdX(:,1);
                B(2:2:end,2) = dNdX(:,2);
                B(1:2:end,3) = dNdX(:,2);
                B(2:2:end,3) = dNdX(:,1);
             
                ewx(ind,:) = ECOORD_X * Ni;
                sigma(ind,:) = Dn * D * (B'*EDisp');
                ind=ind+1;
            end
    end


end