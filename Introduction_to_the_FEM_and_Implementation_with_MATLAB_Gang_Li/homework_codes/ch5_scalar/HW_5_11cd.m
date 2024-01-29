clear all;

nxi=40;
neta=40;
nodes(:,:,1)=[7 5; 3 4; 2 2; 4 2];
nodes(:,:,2)=[.5 .5; 0 2; 0 0; 2 0];

[xi,eta]=meshgrid(-1:2/nxi:1,-1:2/neta:1);
detJ=xi;
x=xi;
y=eta;

for k=1:2
  for j=1:nxi+1
    for i=1:neta+1
      [N,Nx,Ny]=CompNDNatPointsQuad4(xi(i,j),eta(i,j));
      x(i,j)=(nodes(:,1,k))'*N(:,1);
      y(i,j)=(nodes(:,2,k))'*N(:,1);
      J=CompJacobian2DatPoint(nodes(:,:,k), Nx(:,1), Ny(:,1));
      detJ(i,j)=det(J);
    end
  end
  figure(k);
  hold off;
  surf(x,y,detJ,'FaceColor',[0.5 0.5 0.5],'EdgeColor','k');
  hold on
  plot3(nodes(:,1,k), nodes(:,2,k), [0 0 0 0]','k-','linewidth',2);
  plot3([nodes(4,1,k) nodes(1,1,k)],[nodes(4,2,k) nodes(1,2,k)], ...
       [0 0],'k-','linewidth',2);
  alpha(.4)
end
