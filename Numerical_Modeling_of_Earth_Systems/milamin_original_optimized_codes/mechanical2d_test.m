%TEST FLUID2D_SOLVER
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
parameters.Mu           = [100  ;   1];                %Viscosity
parameters.Rho         = [   0;  1];                %Density
parameters.Gy           = -100;                %Gravity in y direction
parameters.Muz = [ 1, 1, 1];% depth dependent viscosity

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
no_pts =      40;
radius =     0.1;
type   =       1;
[GCOORD, ELEM2NODE, Point_id, Phases] = generate_mesh(no_pts,radius,type,...
        second_order);

nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

%add 7th node
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




bcmode=3;
switch bcmode
    case 1
%==========================================================================
% BOUNDARY CONDITION: PURE SHEAR
%==========================================================================
        Bc_ind  = find(Point_id==1);
        Bc_val  = [GCOORD(1,Bc_ind)  -GCOORD(2,Bc_ind)];
        Bc_ind  = [2*(Bc_ind-1)+1       2*(Bc_ind-1)+2];
    case 2 
        % no slip
        Bc_ind  = find(Point_id==1);
        Bc_val  = [zeros(size(Bc_ind))     zeros(size(Bc_ind))];
        Bc_ind  = [2*(Bc_ind-1)+1      2*(Bc_ind-1)+2];
    case 3
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
%method = 'std'
[V PRESSURE] = mechanical2d(ELEM2NODE, Phases, GCOORD, parameters, ...,
        Bc_ind, Bc_val, nip, reorder, method);

%==========================================================================
% POSTPROCESSING
%==========================================================================
fprintf(1, 'POSTPROCESSING:     '); tic



hh = figure(1);
clf(1)
pmode=2;
if(pmode==1)
    trisurf(reshape(1:3*nel,3, nel)', GCOORD(1,ELEM2NODE(1:3,:)), ...,
        GCOORD(2,ELEM2NODE(1:3,:)), zeros(size(GCOORD(1,ELEM2NODE(1:3,:)))), PRESSURE/D(1));
    title('Pressure');
    view(2);
    shading interp;
else
    [xi,yi] = meshgrid(0:.005:1,0:.005:1);
    zi = griddata(GCOORD(1,:)',GCOORD(2,:)', V(1:2:nnod*2-1),...
        xi,yi,'linear');
    pcolor(xi,yi,abs(zi));
    title('|V_x|');

    shading interp
end

colorbar; axis image; %axis off
hold on
quiver(GCOORD(1,:)',GCOORD(2,:)', V(1:2:nnod*2-1),V(2:2:nnod*2),'k');

fprintf(1, [num2str(toc,'%8.6f'),'\n']);
fprintf(1, ['\n']);
