
load U3.dat;
U=U3;

load nodes.dat;
n_nodes=size(nodes,1);
feU=zeros(n_nodes,5);
for i=1:n_nodes
  feu(i,1)=i;
  feU(i,2)=nodes(i,2);
  feU(i,3)=nodes(i,3);
  feU(i,4)=U(i,2);
  feU(i,5)=U(i,3);
end

save -ascii -double feU.dat feU

plotdeformedmesh;


%plot(Uout, 'LineWidth',2);
%axis([0 2100 -1.2e-3 0]);
%xlabel('Time step');
%ylabel('Displacement (m)');
%set(gca,'fontsize',16);
