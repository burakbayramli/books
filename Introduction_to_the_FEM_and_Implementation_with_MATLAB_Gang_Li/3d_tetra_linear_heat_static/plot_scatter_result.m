% plot results on a set of scattered points
% nodes.dat provides the x, y coordinates of the points
% the results in T are represented by the colors


clear all;
clf;

load nodes.dat;
load T.dat;

figure (1);

scatter3(nodes(:,2), nodes(:,3),30,T(:,2),'filled');
