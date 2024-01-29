
load Sxy.dat;
load nodes.dat;
n_nodes=size(nodes,1);


figure(2)
clf
scatter3(nodes(:,2),nodes(:,3),Sxy(:,2),5000,abs(Sxy(:,2)),'s','filled')
view(0,90);

figure(3)
clf
scatter3(nodes(:,2),nodes(:,3),Sxy(:,3),5000,abs(Sxy(:,3)),'s','filled')
view(0,90);

figure(4)
clf
scatter3(nodes(:,2),nodes(:,3),Sxy(:,4),5000,abs(Sxy(:,4)),'s','filled')
view(0,90);

