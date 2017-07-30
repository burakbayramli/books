clear all
%
% test the mesher interface to triangle
%
second_order=???; 

%
% set some of the mesh parameters
%
mesh_par.no_pts_incl =     ???;
mesh_par.radius =     ???; 
mesh_par.ellipticity = ???;
mesh_par.type   =      ???;
mesh_par.qangle = ???;
mesh_par.area_glob = ???;



[GCOORD, ELEM2NODE, Point_id, Phases] = ...
    generate_mesh(mesh_par, second_order, 1);

nnod    = size(GCOORD,2);
nel     = size(ELEM2NODE,2);

figure(2);
clf(2);
trisurf(ELEM2NODE(1:3,:)', GCOORD(1,:), GCOORD(2,:), ... 
        zeros(size(GCOORD(1,:))),'EdgeColor','k','FaceColor','w');
hold on;
% edge nodes
bnodes=find(Point_id==1);
plot(GCOORD(???),GCOORD(???),'ro');

% inclusion nodes
bnodes=find(Point_id==100);
plot(GCOORD(???,???,'g^');

% hole nodes
bnodes=find(Point_id==???);
plot(GCOORD(???),GCOORD(???),'b^');

% color only special elements 

????

axis image;
view(2); 
hold off; 
