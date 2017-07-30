%TEST THERMAL2D 
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
% PHYSICS
%==========================================================================
D           = [1; 1];               % Diffusivities
H           = [0;10];               % Internal heating

bc_mode = 3; % 1: linear profile on sides
             % 2: contant zero temperature on sides
             % 3: zero on top, unity on surface


%==========================================================================
% MESH GENERATION: 
%==========================================================================
fprintf(1, 'PREPROCESSING:      '); tic

second_order=0; % use six or three node triangles?

% mesh parameters
mesh_par.no_pts_incl = 100;% points on inclusion
mesh_par.radius =     0.4; % 
mesh_par.type   =       1; % 
mesh_par.ellipticity = 0.8;
mesh_par.qangle = 27;
mesh_par.area_glob = 0.003;
conv_to_int = 0;

[GCOORD, ELEM2NODE, Point_id, Phases] = ...
    generate_mesh(mesh_par,second_order,conv_to_int);
nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

if(bc_mode == 1)
    %==========================================================================
    % BOUNDARY CONDITION: LINEAR TEMPERATURE PROFILE ON BOX, ZERO FLUX ON HOLE
    %==========================================================================
    Bc_ind  = find(Point_id==1);
    Bc_val  = 1-GCOORD(1,Bc_ind);
elseif(bc_mode == 2) % zero temperatures on the outside
    Bc_ind = find(Point_id==1);
    Bc_val  = zeros(size(Bc_ind));
elseif(bc_mode == 3) % zero temperature on top, unity on bottom
    ind1 = find(GCOORD(2,:)==1);
    ind2 = find(GCOORD(2,:)==0);
    Bc_ind = [ ind1 ind2];
    Bc_val  = [ zeros(size(ind1)) ones(size(ind2))];
end


fprintf(1, [num2str(toc,'%8.6f')]);
fprintf(1, ['\n Number of nodes:   ', num2str(nnod)]);
fprintf(1, ['\n Number of elems:   ', num2str(nel),'\n']);

if(second_order == 1)
    nip  = 6; 
else
    nip = 3
end

Temp = ??? % call the thermal2d_std solver here

%==========================================================================
% POSTPROCESSING
%==========================================================================
fprintf(1, 'POSTPROCESSING:     '); tic
figure(1);clf(1);

pmode=1;
switch pmode
    case 1   
        % triangular based on elements
        trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:), GCOORD(2,:), ...
            zeros(size(GCOORD(1,:))),Temp,...  
            'FaceColor', 'interp');
    case 2
        % interpolated, based on nodes
        [xi,yi] = meshgrid(0:.01:1,0:.01:1);
        zi = griddata(GCOORD(1,:), GCOORD(2,:),Temp,xi,yi,'cubic');
        pcolor(xi,yi,zi);
        shading interp
end
title('Temperature');view(2); colorbar; axis image;%, axis off
colormap jet

hold on;  % plot nodes with boundary conditions on top
plot(GCOORD(1,Bc_ind),GCOORD(2,Bc_ind),'o');
 

fprintf(1, [num2str(toc,'%8.6f'),'\n']);
fprintf(1, ['\n']);
