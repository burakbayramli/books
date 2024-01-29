% next 10 lines: global variables
global edges nodes ray_angles; 
global n_nodes last_node_row n_edges n_max_edges last_edge_row dflag;
global tris tris_data n_tris n_max_tris last_tri_row nd_corners;
global TOL MAX_LENGTH;
global ec_queue ec_head ec_tail;
global sm_queue sm_head sm_tail sm_nodes;
global sw_queue sw_head sw_tail;
global test_node test_edge;
global no_bad_node no_bad_edge;
global angle_test_done edge_test_done;
% next 5 lines: initilization of some of the global variables
dflag=0;                              % a test flag
TOL=1e-5;                             % tolerance
%MAX_LENGTH=500;                      % maxium edge length allowed
angle_test_done=0;                    % an angle test flag
edge_test_done=0;                     % an edge test flag
load shape.dat;                       % load the PSLG file
vertices=shape(shape(1,1)+2:end,:);   % get the vertices of the shape
% next 13 lines: initialization of the data structures
n_nodes=size(vertices,1);             
n_edges=0;                           
last_edge_row=0;                      
n_max_edges=10;
edges=zeros(n_max_edges,10);
nodes=zeros(n_nodes,23);
nodes(:,1:2)=vertices;
nodes(:,3)=(linspace(1,n_nodes,n_nodes))';
ray_angles=zeros(n_nodes,20);
nodes=sortrows(nodes,[1 2]);
bnode_pos=nodes(:,3);
nodes(:,3)=0;
last_node_row=size(nodes,1);
% next 5 lines: main triangulation and refinement functions
[le,re]=BuildDelaunay(1,n_nodes);
bpoints=SetBoundaryEdges(shape, bnode_pos);
DeleteOutsideEdges();
SetupTriangles();
DelaunayRefinement();
last_tri_row
% next 2 lines: plot the mesh
figure(1);
clf; PlotEdges();