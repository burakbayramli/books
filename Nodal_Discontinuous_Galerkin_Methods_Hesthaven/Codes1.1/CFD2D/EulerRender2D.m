function EulerRender2D(Q, gamma, time, ExactSolution)

% function EulerRender2D(Q, gamma, time, ExactSolution)
% purpose: render solution from Euler 2D

Globals2D;

rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4); 
u = rhou./rho; v = rhov./rho;

p = (gamma-1)*(Ener-0.5*(rhou.*u + rhov.*v));
c = sqrt(gamma*p./rho);
magu = sqrt(u.^2 + v.^2);
   
mach = max(max(magu./c))

if(~isempty(ExactSolution))
  QEX = feval(ExactSolution, x, y, time);
  rhoEX = QEX(:,:,1); rhouEX = QEX(:,:,2); rhovEX = QEX(:,:,3); EnerEX = QEX(:,:,4);     
  
  subplot(2,2,1); PlotField2D(N, x, y, rhou-rhouEX); view(2); axis equal; colorbar
  subplot(2,2,2); PlotField2D(N, x, y, rhov-rhovEX); view(2); axis equal; colorbar
  subplot(2,2,3); PlotField2D(N, x, y, rho-rhoEX);  view(2); axis equal; colorbar;
  subplot(2,2,4); PlotField2D(N, x, y, Ener-EnerEX); view(2); axis equal; colorbar
  
  drawnow; pause(.05)
  
  % compute L2 error in Ener
  MassMatrix = invV'*invV;
  errEner = Ener-EnerEX;
  MMerrEner = MassMatrix*(J.*errEner);
  errL2 = sqrt(errEner(:)'*MMerrEner(:))
else
  
  u = rhou./rho; v = rhov./rho;
  dudy = ry.*(Dr*u) + sy.*(Ds*u); dvdx = rx.*(Dr*v) + sx.*(Ds*v);
  
  figure(1)
  subplot(2,3,1); PlotField2D(N, x, y, rho);  view(2); axis equal; title('density')
  subplot(2,3,2); PlotField2D(N, x, y, rhou); view(2); axis equal; title('x-momentum')
  subplot(2,3,3); PlotField2D(N, x, y, rhov); view(2); axis equal; title('y-momentum')
  subplot(2,3,4); PlotField2D(N, x, y, Ener);  view(2); axis equal; title('total Energy')
  subplot(2,3,5); PlotField2D(N, x, y, magu./c);  view(2); axis equal; title('mach number'); colorbar
  set(gcf, 'Name', sprintf('Time=%7.5f', time))
  drawnow; pause(.02)
end

disp('rendering end');
return;   
