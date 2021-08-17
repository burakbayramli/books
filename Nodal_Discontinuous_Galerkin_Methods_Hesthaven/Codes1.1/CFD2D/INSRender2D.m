function INSRender2D(time, Nout, Ux, Uy, PR)

% function INSRender2D(time, Ux, Uy, PR)
% purpose: render solution from INS 2D

Globals2D;

subplot(2,2,1); PlotField2D(Nout, x, y, Ux); view(2); axis equal; title('x-velocity'); colorbar;
subplot(2,2,2); PlotField2D(Nout, x, y, Uy); view(2); axis equal; title('y-velocity'); colorbar;
subplot(2,2,3); PlotField2D(Nout, x, y, PR); view(2); axis equal; title('Pressure'); colorbar;

[tmp,tmp,vort] = Curl2D(Ux, Uy, []);
subplot(2,2,4); PlotField2D(Nout, x, y, vort); view(2); axis equal; title('Vorticity'); colorbar;
set(gcf, 'Name', sprintf('Time=%7.5f', time))
drawnow; pause(.02)
return;   
