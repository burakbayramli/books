function [ xn sigma ] = calc_nd_stress(Disp, ELEM2NODE, GCOORD, Phases, ...
        material)
    %   
    % calculate stresses at nodes
    % for 2-D (compressible) linear, isotropic elasticity
    % returns the location of the integration points, and three component
    % stress vector
    %
    % output is sxx syy shear=2sxy, compression positive?
    %
    % nip_plot is a vector with a list of which points to evaluates
    
    ndim        = 2; % number of dimensions
    nnod        = size(GCOORD,2); % number of nodes
    nel         = size(ELEM2NODE,2); % number elements
    nnodel      = size(ELEM2NODE,1); % number of nodes per element
    nnodel2 = nnodel*2;
    nedof       = nnodel*ndim; % number of elemental degrees of freedom


    IP_X    = [ [ 0 0]; [1 0]; [0 1 ]]';
    [   N, dNdu]    = shp_deriv_triangle(IP_X, nnodel);
    B           =     zeros(nedof,ndim*(ndim+1)/2);

    index=1;
    for ind = 1:nnod
        ninel =[];
        [ tmp ninel ] = find(ELEM2NODE(1:3,:)==ind);
        nelnd=length(ninel);
        if nelnd > 0
            sigma(index,:) = zeros(1,3);
            xn = GCOORD(:,ind);
            for i = 1:nelnd;
                iel = ninel(i);
                ip  = tmp(i)
                ECOORD_X  = GCOORD(:,ELEM2NODE(:,iel));
                EDisp(1:2:nnodel2-1) = Disp(ELEM2NODE(:,iel)*2-1);
                EDisp(2:2:nnodel2) = Disp(ELEM2NODE(:,iel)*2);
                [ D Dn ] = calc_el_D(material.Mu(Phases(iel)),material.nu(Phases(iel)),...
                    material.plane_strain);
                Ni       =       N{ip}; % shape function at node
                dNdui       =    dNdu{ip}; % derivative
                J           = ECOORD_X*dNdui;
                dNdX        = dNdui*inv2D(J,det2D(J));
                B(1:2:end,1) = dNdX(:,1);
                B(2:2:end,2) = dNdX(:,2);
                B(1:2:end,3) = dNdX(:,2);
                B(2:2:end,3) = dNdX(:,1);
             
                sigma(ind,:) = sigma(ind,:) + (Dn * (D * (B'*EDisp')))';

            end

            sigma(index,:) = sigma(index,:)/nelnd;
            index=index+1;
        end
    end
    sigma = sigma';
end