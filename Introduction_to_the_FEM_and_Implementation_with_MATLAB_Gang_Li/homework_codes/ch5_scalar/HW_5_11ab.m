clear all;

lines=[-2/3 -1/3 0 1/3 2/3]';
coord1=[-1:0.05:1]';
coord2=ones(41,1);
nodes(:,:,1)=[7 5; 3 4; 2 2; 4 2];
nodes(:,:,2)=[.5 .5; 0 2; 0 0; 2 0];

for k=1:2
  figure(k);
  hold off;
  plot(nodes(:,1,k), nodes(:,2,k),'k-','linewidth',2);
  hold on;
  plot([nodes(4,1,k) nodes(1,1,k)],[nodes(4,2,k) nodes(1,2,k)], 'k-','linewidth',2);
  for i=1:5
    ln=lines(i);
    [N,Nx,Ny]=CompNDNatPointsQuad4(coord1, coord2*ln);
    xy=(nodes(:,:,k))'*N;
    plot(xy(1,:), xy(2,:),'r:','linewidth',2);
    [N,Nx,Ny]=CompNDNatPointsQuad4(coord2*ln, coord1);
    xy=(nodes(:,:,k))'*N;
    plot(xy(1,:), xy(2,:),'b:','linewidth',2);
  end
end