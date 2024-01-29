% next 3 lines load files
load elements.dat;
load nodes.dat;
load feU.dat;     % results file
scaling_factor=100;  % set scaling factor
nodes=nodes(:,2:4)+feU(:,5:7)*scaling_factor; % deformed nodes

% next 16 lines: create patches of the element faces
p=patch('Vertices',nodes,'Faces',elements(:,2:5));
set(p,'facecolor','[.9 .9 .9]','edgecolor','black');
s1=elements(:,[2 3 7 6]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','[.9 .9 .9]','edgecolor','black');
s1=elements(:,[3 4 8 7]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','[.9 .9 .9]','edgecolor','black');
s1=elements(:,[4 5 9 8]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','[.9 .9 .9]','edgecolor','black');
s1=elements(:,[5 2 6 9]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','[.9 .9 .9]','edgecolor','black');
p=patch('Vertices',nodes,'Faces',elements(:,6:9));
set(p,'facecolor','[.9 .9 .9]','edgecolor','black');

% next 6 lines: set 3-D view
daspect([1 1 0.7]); 
view(150,35); 
grid on;
camlight; lighting gouraud;
alpha(.75)
axis([-60 60 -10 70 -20 20]);