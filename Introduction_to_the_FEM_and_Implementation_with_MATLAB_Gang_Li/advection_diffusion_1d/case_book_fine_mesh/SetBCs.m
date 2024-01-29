load nodes.dat
n=size(nodes,1);

bcs=zeros(n,3);
bcs(:,1)=nodes(:,1);
bcs(1,2)=1;
bcs(1,3)=0.01;
bcs(n,2)=1;
bcs(n,3)=0;

dlmwrite('bcs.dat',bcs,'delimiter','\t','precision','%.6f');

bfs=zeros(n,2);
bfs(:,1)=nodes(:,1);
bfs(:,2)=4e-10;
dlmwrite('bfs.dat',bfs,'delimiter','\t','precision','%.10f');