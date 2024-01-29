clear all;
sr=2;  % inner radius
er=10; % outer radius
m=5;   % divisions in r direction
n=20;  % divisions in angle direction
stheta=0;  % starting angle
etheta=pi; % ending angle

% next block: create nodes and elements
[nodes,elements,nids]=HalfAnnulusMeshQuad4(sr,er,stheta,etheta,m,n);
n_nodes=size(nodes,1)+1;
n_elements=size(elements,1);
nodes(n_nodes,1:3)=[n_nodes 0 0];

% next block: adding more elements
e=n_elements;
for i=1:n 
  e=e+1;
  elements(e,1:5)=[e n_nodes nids(i,1) nids(i+1,1) n_nodes];
end
n_elements=size(elements,1);  

% following code block: write nodes.dat
fid=fopen('nodes.dat','w+');   % open file "nodes.dat"
for i=1:n_nodes
  fprintf(fid,'%d %.10f %.10f\n',nodes(i,1:3)); 
end
fclose(fid);       % close "nodes.dat"

% following code block: write elements.dat
fid=fopen('elements.dat','w+'); % open file "elements.dat"
for i=1:n_elements
  fprintf(fid,'%d %d %d %d %d \n', elements(i,1:5));
end
fclose(fid);  % close "elements.dat"

plotmesh;

