%
%
% elasticity solver driver, based on milamin code
%
% uses the simplified solver
%
%

%
%   Part of MILAMIN: MATLAB-based FEM solver for large problems, Version 1.0
%   Copyright (C) 2007, M. Dabrowski, M. Krotkiewski, D.W. Schmid
%   University of Oslo, Physics of Geological Processes
%   http://milamin.org
%   See License file for terms of use.

%==========================================================================
% CLEARING AND INITIALIZATION 
%==========================================================================

%CLEAR ENVIRONMENT, BUT NOT BREAKPOINTS
%clc; 
clear variables;


%==========================================================================
% USER INPUT: PHYSICS
%==========================================================================
material.Mu     = [??];                % shear modulus
material.nu     = [??];                % Poisson ratio

material.plane_strain = true; % plane stress or plane strain?

%==========================================================================
% MESH GENERATION: 
%==========================================================================
fprintf(1, 'PREPROCESSING:      '); tic


second_order=??; % we need second order (six node, geometrically) triangles
%
%  nip                - Number of integration points 
%
nip      =       ??;  

% add a 7th, center node within the element and change the shape function?
add_seven_center = false;


% mesh parameters
mesh_par.no_pts_incl = 1;% points on inclusion
mesh_par.radius =     0.5; % 
mesh_par.type   =       0; % 
mesh_par.ellipticity = 0;
mesh_par.qangle = 10;
mesh_par.area_glob = 0.1;
conv_to_int = 0;

%
% generate mesh
%
[GCOORD, ELEM2NODE, Point_id, Phases] = ...
    generate_mesh(mesh_par,second_order,conv_to_int);

nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

if(add_seven_center)
    %%add 7th node
    ELEM2NODE(7,:)  = nnod+1:nnod+nel;
    GCOORD          = [GCOORD, [...
        mean(reshape(GCOORD(1, ELEM2NODE(1:3,:)), 3, nel));...
        mean(reshape(GCOORD(2, ELEM2NODE(1:3,:)), 3, nel))]];
    % update node number
    nnod    = size(GCOORD,2);
end

%
% visualize the initial mesh
%
plot_mesh=0;
switch plot_mesh
    case 1
        figure(2);clf(2);
        % elements themselves
        trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:), GCOORD(2,:), ... 
            zeros(size(GCOORD(1,:))),'EdgeColor','k','FaceColor','w');
        hold on;
        % edge nodes
	% ???
        % hole nodes
	% ???
	% special elements, overplot with different color
        cstring='rgbcmyb';
        for i = 2:max(Phases)
            sele=find(Phases == i);
            trisurf(ELEM2NODE(1:3,sele)', GCOORD(1,:), GCOORD(2,:), ... 
                zeros(size(GCOORD(1,:))),'FaceColor',cstring(i-1));
        end
        view(2);  axis image  
end

fprintf(1, [num2str(toc,'%8.6f'),'\n']);


%
% bondary conditions and gravity
%

bcmode=2;
switch bcmode
    case 1
        %
        % no density effect, shear at top
        %
        utop = ???;
        material.Rho         = [   ??? ];                %Density
        % no slip at bottom, shear on top
        y1 = min(GCOORD(2,:));
        y2=max(GCOORD(2,:));
        bottom = find(GCOORD(2,:)==y1);
        top =    find(GCOORD(2,:)==y2);
        Bc_val  = [zeros(size(bottom)) zeros(size(bottom)) ...
                  utop *ones(size(top)) zeros(size(top))  ];
        Bc_ind  = [2*(bottom-1)+1      2*(bottom-1)+2 ...
                2*(top-1)+1      2*(top-1)+2];
   case 2
        %
        % no density effect, sheat at top and sides
        %
        material.Rho         = [   ???];                %Density
        % no slip at bottom, shear on top and sides
        utop = ??? ;

	
      	y1 = min(GCOORD(2,:));y2=max(GCOORD(2,:));
        bottom = find(GCOORD(2,:)==y1);top = find(GCOORD(2,:)==y2);
        x1=min(GCOORD(1,:));x2=max(GCOORD(1,:));
        leftright = [ find(GCOORD(1,:)==x1) find(GCOORD(1,:)==x2) ];
        leftright(GCOORD(2,leftright)==y1) =[];
        leftright(GCOORD(2,leftright)==y2) =[];
        Bc_val  = [zeros(size(bottom)) zeros(size(bottom)) ...
                utop*ones(size(top)) zeros(size(top))  ...
                utop*GCOORD(2,leftright) zeros(size(leftright)) ];
        Bc_ind  = [2*(bottom-1)+1      2*(bottom-1)+2 ...
                2*(top-1)+1      2*(top-1)+2 ...
                2*(leftright-1)+1      2*(leftright-1)+2 ];
        
    case 3
        %
        % box under own weight, no slip at bottom
        %
        material.Rho = ???              %Density
        
        Bc_val  = [???  ???];
        Bc_ind  = [???  ???];
        
    case 4
        %
        % box under own weight, constrained on sides
        %
        material.Rho  = ???              %Density
        % free slip
        % left and right constrained in x
        % top and bottom constrained in y
        
        Bc_val = [ ???  ??? ];
        % x component for left and right, y component for top and bottom
        Bc_ind = [ ???  ??? ];
        
                            
end
        
        
fprintf(1, [num2str(toc,'%8.6f')]);
fprintf(1, ['\n Number of nodes:   ', num2str(nnod)]);
fprintf(1, ['\n Number of elems:   ', num2str(nel),'\n']);



% calculate displacement
[U] = elastic2d_std(ELEM2NODE, GCOORD, Phases, material,...
        Bc_ind, Bc_val, nip);
%
%  assign solution to u_x and u_z displacements for simplicity
%
ux = U(1:2:nnod*2-1);
uz = U(2:2:nnod*2);



%==========================================================================
% POSTPROCESSING
%==========================================================================
fprintf(1, 'POSTPROCESSING:     '); tic


figure(1);
clf(1);
hold on

plottype=1;
use_new_coord=0; % plot the stresses at the new, deformed coordinates?

if(plottype == 1) % plot stresses by interpolation on regular grid
    
    %
    % calculate stress within elements
    %
    if(use_new_coord) % compute the stress locations for the deformed mesh
        [ ewx sigma ] = calc_el_stress(U, ELEM2NODE, GCOORD+reshape(U,size(GCOORD)),...
            Phases,     material, nip, [1:3:nip]);
    else
        % place the stress output locations on the original mesh
        [ ewx sigma ] = calc_el_stress(U, ELEM2NODE, GCOORD,...
            Phases,     material, nip, [1:1:nip]);
    end
    %
    % compute first and second eigenvalue and angle of largest eigenvector
    %
    [ s1 s2 azi ] = eig2d(sigma);

    % generate a set of basis vectors for gridded representation
    [xi,yi] = meshgrid(0:.005:1,0:.005:1);
    % interpolate a grid of max shear stress
    %zi = griddata(ewx(:,1),ewx(:,2),(s1-s2)/2,xi,yi,'linear');
    %title('max shear stress');

    % mean normal stress
    zi = griddata(ewx(:,1),ewx(:,2),(s1+s2)/2,xi,yi,'linear');
    title('mean normal stress');

    pcolor(xi,yi,zi);
    colorbar;
    % fixed colorbar?
    %caxis([0, .02]);
    shading interp
    %
    % add strain crosses
    %
    plot2d_strain_cross(ewx,s1,s2,azi,0);
    
elseif(plottype==2) % plot displacements
    [xi,yi] = meshgrid(0:.005:1,0:.005:1);
    % absolute displacements
    uabs = sqrt(ux.^2 + uz.^2);
    zi = griddata(GCOORD(1,:)',GCOORD(2,:)',uabs,xi,yi,'linear');
    pcolor(xi,yi,zi), shading interp
    % displacement vectors 
    quiver(GCOORD(1,:)',GCOORD(2,:)', ux,uz,'k');
elseif(plottype ==3) % plot shear stress on each element
    
    % stress at nodes
    sigma = calc_nd_stress(U, ELEM2NODE, GCOORD, Phases, ...
        material, second_order);
    [ s1 s2 azi ] = eig2d(sigma); % compute eigensystem
    % plot
    trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:), GCOORD(2,:), ...
        (s1+s2)/2);
    colorbar
    view(2);  axis image

    
end

axis image; 
hold off

% plot the deformed mesh

????

fprintf(1, [num2str(toc,'%8.6f'),'\n']);
fprintf(1, ['\n']);
