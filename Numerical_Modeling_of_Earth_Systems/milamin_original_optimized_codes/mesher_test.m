clear all
%
% test the mesher interface to triangle
%
second_order=0; % use six or three node triangles?

%
% set some of the mesh parameters
%
mesh_par.no_pts_incl =      20;% points on inclusion
mesh_par.radius =     0.2; % 
mesh_par.ellipticity = 0.9;
mesh_par.type   =       1; % 
mesh_par.qangle = 33;
mesh_par.area_glob = 0.01;


tic
% make the new grid using triangle
[GCOORD, ELEM2NODE, Point_id, Phases] = ...
    generate_mesh(mesh_par, second_order, 1);

nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

figure(2);
%clf(2);
trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:), GCOORD(2,:), ... 
        zeros(size(GCOORD(1,:))),'EdgeColor','k','FaceColor','w');
hold on;
% edge nodes
bnodes=find(Point_id==1);
plot(GCOORD(1,bnodes),GCOORD(2,bnodes),'ro');

% inclusion nodes
bnodes=find(Point_id==100);
plot(GCOORD(1,bnodes),GCOORD(2,bnodes),'g^');

% hole nodes
bnodes=find(Point_id==200);
plot(GCOORD(1,bnodes),GCOORD(2,bnodes),'b^');

% special elements
cstring='rgbcmyb';
for i = 2:max(Phases)
    sele=find(Phases == i);
    trisurf(ELEM2NODE(1:3,sele)', GCOORD(1,:), GCOORD(2,:), ... 
            zeros(size(GCOORD(1,:))),'FaceColor',cstring(i-1));
end

fprintf(1, [num2str(toc,'%8.6f'),'\n']);
axis image;
view(2); 
hold off; 
