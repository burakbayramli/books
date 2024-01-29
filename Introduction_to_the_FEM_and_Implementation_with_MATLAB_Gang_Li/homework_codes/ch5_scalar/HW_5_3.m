clear;
clf;

nx=20; % mesh grid parameters
ny=20;

[x,y]=meshgrid(-1:2/nx:1,-1:2/ny:1);

% for-loop: evaluate the shape functions at the grid points
for j=1:nx+1
  for i=1:ny+1
    N=CompShapeTransition(x(i,j),y(i,j));
    for k=1:7
      NT(i,j,k)=N(k,1);    
    end
  end
end

% next block: plotting
for k=1:7
  figure(k);
  NK=NT(:,:,k);
  surf(x,y,NK,'FaceColor',[0.5 0.5 0.5],'EdgeColor','k');
  hold on
  plot3([1 -1 -1 1 1], [1 1 -1 -1 1],[0 0 0 0 0],'k');
  alpha(.4)
  %camlight left; lighting phong
  xlabel('x');
  ylabel('y');
  zlabel('N_i(x,y)');
  hold off
end