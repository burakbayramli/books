%
%
% elasticity solver, based on milamin code

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

%SET THE DEFAULT ROOT RENDERER TO ZBUFFER,
%set(0, 'DefaultFigureRenderer', 'zbuffer');

%==========================================================================
% USER INPUT: PHYSIX
%==========================================================================
material.Mu     = [0.1  ;   1];                % shear modulus
material.nu     = [.25  ;   .25];                % Poisson ratio
material.plane_strain = true;

%==========================================================================
% MESH GENERATION: 
% no_pts            - number of points on inclusion outline
% radius            - size of inclusion(s)
% (type = 1)        - square box + circular inclusion
% (type = 2)        - square box + circular hole + circular inclusion
% (mode = 'ascii')  - Triangle output written as ASCII files
% (mode = 'binary') - Triangle output written as binary files
%==========================================================================
fprintf(1, 'PREPROCESSING:      '); tic

second_order=1; % we need second order (six node, geometrically) triangles
% mesh parameters
mesh_par.no_pts_incl = 40;% points on inclusion
mesh_par.radius =     0.2; % 
mesh_par.type   =       0; % 
mesh_par.ellipticity = 0;
mesh_par.qangle = 25;
mesh_par.area_glob = 0.001;
conv_to_int = 1;

[GCOORD, ELEM2NODE, Point_id, Phases] = generate_mesh(mesh_par,...
        second_order,conv_to_int);

nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

%%add 7th node
ELEM2NODE(7,:)  = nnod+1:nnod+nel;
GCOORD          = [GCOORD, [...
    mean(reshape(GCOORD(1, ELEM2NODE(1:3,:)), 3, nel));...
    mean(reshape(GCOORD(2, ELEM2NODE(1:3,:)), 3, nel))]];

nnod    = size(GCOORD,2);

plot_mesh=1;
switch plot_mesh
    case 1
        figure(2);clf(2);
        trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:), GCOORD(2,:), ... 
            zeros(size(GCOORD(1,:))),'EdgeColor','k','FaceColor','w');
        hold on;
        % edge nodes
        bnodes=find(Point_id==1);
        plot(GCOORD(1,bnodes),GCOORD(2,bnodes),'ro');
        % hole nodes
        bnodes=find(Point_id==100);
        plot(GCOORD(1,bnodes),GCOORD(2,bnodes),'g^');
        % special elements
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

bcmode=1;
switch bcmode
    case 1
        material.Rho         = [   0;  0];                %Density
        % no slip at bottom, shear on top
        y1 = min(GCOORD(2,:));y2=max(GCOORD(2,:));
        bottom = find(GCOORD(2,:)==y1);
        top = find(GCOORD(2,:)==y2);
        Bc_val  = [zeros(size(bottom)) zeros(size(bottom)) ...
                .05*ones(size(top)) zeros(size(top))  ];
        Bc_ind  = [2*(bottom-1)+1      2*(bottom-1)+2 ...
                2*(top-1)+1      2*(top-1)+2];
   case 2
        material.Rho         = [   0;  0];                %Density
        % no slip at bottom, shear on top and sides
        u = 0.05;
        y1 = min(GCOORD(2,:));y2=max(GCOORD(2,:));
        bottom = find(GCOORD(2,:)==y1);top = find(GCOORD(2,:)==y2);
        x1=min(GCOORD(1,:));x2=max(GCOORD(1,:));
        leftright = [ find(GCOORD(1,:)==x1) find(GCOORD(1,:)==x2) ];
        leftright(find(GCOORD(2,leftright)==y1)) =[];
        leftright(find(GCOORD(2,leftright)==y2)) =[];
        Bc_val  = [zeros(size(bottom)) zeros(size(bottom)) ...
                u*ones(size(top)) zeros(size(top))  ...
                u*GCOORD(2,leftright) zeros(size(leftright)) ];
        Bc_ind  = [2*(bottom-1)+1      2*(bottom-1)+2 ...
                2*(top-1)+1      2*(top-1)+2 ...
                2*(leftright-1)+1      2*(leftright-1)+2 ];
                            
    case 3 
        % no slip at bottom
        material.Rho         = [   .1;  .1];                %Density
        y1 = min(GCOORD(2,:));
        bottom = find(GCOORD(2,:)==y1);
        Bc_val  = [zeros(size(bottom)) zeros(size(bottom)) ];
        Bc_ind  = [2*(bottom-1)+1      2*(bottom-1)+2];
    case 4
        material.Rho         = [   .1;  .1];                %Density
        % free slip
        % left and right constrained in x
        % top and bottom constrained in y
        x1=min(GCOORD(1,:));x2=max(GCOORD(1,:));
        leftright = [ find(GCOORD(1,:)==x1) find(GCOORD(1,:)==x2) ];
        y1=min(GCOORD(2,:));y2=max(GCOORD(2,:));
        topbottom = [ find(GCOORD(2,:)==y1) find(GCOORD(2,:)==y2) ];
        
        Bc_val = [ zeros(size(leftright)) zeros(size(topbottom)) ];
        % x component for left and right, y component for top and bottom
        Bc_ind = [2*(leftright-1)+1  2*(topbottom-1)+2 ];
        clear leftright topbottom;
end
        
        
fprintf(1, [num2str(toc,'%8.6f')]);
fprintf(1, ['\n Number of nodes:   ', num2str(nnod)]);
fprintf(1, ['\n Number of elems:   ', num2str(nel),'\n']);


%==========================================================================
% SOLVER
%  nip                - Number of integration points (6 or higher)
% (reorder = 'amd')   - AMD reordering
% (reorder = 'metis') - METIS reordering
% (method = 'std')    - Standard matrix computation
% (method = 'opt')    - Optimized matrix computation
%==========================================================================
nip      =       6;  
%reorder  =   'amd';
reorder = 'metis';
method   =   'opt'; 
%method = 'std';

% calculate displacement
[U] = elastic2d(ELEM2NODE, GCOORD, Phases, material,...
        Bc_ind, Bc_val, nip, reorder, method);
% calculate stress
use_new_coord=1;
if(use_new_coord)
    [ ewx sigma ] = calc_el_stress(U, ELEM2NODE, GCOORD+reshape(U,size(GCOORD)),...
        Phases,     material, nip, [1:3:nip]);
else
      [ ewx sigma ] = calc_el_stress(U, ELEM2NODE, GCOORD,...
        Phases,     material, nip, [1:1:nip]);
end
[ s1 s2 azi ] = eig2d(sigma);
azi = azi/180*pi;

%==========================================================================
% POSTPROCESSING
%==========================================================================
fprintf(1, 'POSTPROCESSING:     '); tic

hh = figure(1);
clf(1);
hold on


[xi,yi] = meshgrid(0:.005:1,0:.005:1);

%zi = griddata(ewx(:,1),ewx(:,2),(s1-s2)/2,xi,yi,'linear');
%title('max shear stress');
zi = griddata(ewx(:,1),ewx(:,2),(s1+s2)/2,xi,yi,'linear');
title('mean normal stress');

pcolor(xi,yi,zi);
colorbar;
%caxis([0, .02]);
shading interp
plot2d_strain_cross(ewx,s1,s2,azi,0);
%quiver(GCOORD(1,:)',GCOORD(2,:)', U(1:2:nnod*2-1),U(2:2:nnod*2),'k');
%plot(ewx(:,1),ewx(:,2),'o');

axis image; 
hold off

% plot the deformed mesh?
pdisp=1;
if pdisp == 1
    figure(3);clf(3);
    scale = 5
    hold on;
    trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:)+(U(1:2:end-1)')*scale,...
        GCOORD(2,:)+(U(2:2:end)')*scale, ...
        zeros(size(GCOORD(1,:))),'FaceColor','r');
    view(2);
end

fprintf(1, [num2str(toc,'%8.6f'),'\n']);
fprintf(1, ['\n']);
