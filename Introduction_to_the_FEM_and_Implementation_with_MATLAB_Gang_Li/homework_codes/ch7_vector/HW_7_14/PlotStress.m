
load Sxy.dat;
load nodes.dat;
load elements.dat;
n_nodes=size(nodes,1);

node1=elements(1,2);
node2=elements(1,4);

figure(2)
clf
scatter3(nodes(:,2),nodes(:,3),Sxy(:,5),250,Sxy(:,5),'s','filled');
colorbar;
view(0,90);
colormap jet;
lim = caxis
caxis([.3 1.6]*1e7)
