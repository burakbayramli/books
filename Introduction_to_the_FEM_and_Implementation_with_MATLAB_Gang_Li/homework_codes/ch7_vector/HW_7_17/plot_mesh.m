clear();
clf();
load elements.dat;
load nodes.dat;

nodes=nodes(:,2:4);

p=patch('Vertices',nodes,'Faces',elements(:,2:5));
set(p,'facecolor','yellow','edgecolor','black');


s1=elements(:,[2 3 7 6]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','yellow','edgecolor','black');


s1=elements(:,[3 4 8 7]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','yellow','edgecolor','black');


s1=elements(:,[4 5 9 8]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','yellow','edgecolor','black');


s1=elements(:,[5 2 6 9]);
p=patch('Vertices',nodes,'Faces',s1);
set(p,'facecolor','yellow','edgecolor','black');

p=patch('Vertices',nodes,'Faces',elements(:,6:9));
set(p,'facecolor','yellow','edgecolor','black');

daspect([1 1 0.7]);
view(150,45); 
grid on;
camlight; lighting gouraud;

alpha(.75)

axis([-60 60 -10 70 -5 15]);

%print -r200 -djpeg90 mesh.jpg
print -depsc T_beam_mesh2.eps