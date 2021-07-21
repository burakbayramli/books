clf
clear M

for n = 1:size(U,2)  
  pdesurf(p, t, U(:,n))
  shading interp
  colormap hot
  view(2)  
  M(n) = getframe;
end

movie(M)
