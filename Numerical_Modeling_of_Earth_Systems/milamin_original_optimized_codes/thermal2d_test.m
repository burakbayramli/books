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

%SET THE DEFAULT ROOT RENDERER TO ZBUFFER
%(Nicer plots than OpenGL, no transparency support)
%set(0, 'DefaultFigureRenderer', 'zbuffer');

%==========================================================================
% PHYSIX
%==========================================================================
D           = [1; 5];                %Diffusivities

%==========================================================================
% MESH GENERATION: 
% no_pts            - number of points on inclusion outline
% radius            - size of inclusion(s)
% (type = 1)        - square box + circular inclusion
% (type = 2)        - square box + circular hole + circular inclusion
%==========================================================================
fprintf(1, 'PREPROCESSING:      '); tic

second_order=0; % use six or three node triangles?

no_pts =      100;
radius =     0.2;
type   =       1;

[GCOORD, ELEM2NODE, Point_id, Phases] = generate_mesh(no_pts,radius,...
    type,second_order);
nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

%==========================================================================
% BOUNDARY CONDITION: LINEAR TEMPERATURE PROFILE ON BOX, ZERO FLUX ON HOLE
%==========================================================================
Bc_ind  = find(Point_id==1);
Bc_val  = 1-GCOORD(1,Bc_ind);
fprintf(1, [num2str(toc,'%8.6f')]);
fprintf(1, ['\n Number of nodes:   ', num2str(nnod)]);
fprintf(1, ['\n Number of elems:   ', num2str(nel),'\n']);

%==========================================================================
% SOLVER
%  nip                - Number of integration points (3 or higher)
% (reorder = 'amd')   - AMD reordering
% (reorder = 'metis') - METIS reordering
% (method = 'std')    - Standard matrix computation
% (method = 'opt')    - Optimized matrix computation
%==========================================================================
if(second_order == 1)
    nip  = 6; 
else
    nip = 3
end
reorder   =     'amd';
%method    =     'opt';
method    =     'std';

Temp = thermal2d(ELEM2NODE, Phases, GCOORD, D, Bc_ind, Bc_val, nip, ...
        reorder, method);

%==========================================================================
% POSTPROCESSING
%==========================================================================
fprintf(1, 'POSTPROCESSING:     '); tic
figure(1);clf(1);

pmode=2;
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
hold on; 
%plot(GCOORD(1,Bc_ind),GCOORD(2,Bc_ind),'o');
 

fprintf(1, [num2str(toc,'%8.6f'),'\n']);
fprintf(1, ['\n']);
