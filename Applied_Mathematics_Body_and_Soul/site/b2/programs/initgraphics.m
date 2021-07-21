function initgraphics()
% Initializes the graphics system to prepare for drawing 3D geometry.

  figure(1)
  set(gcf, 'DoubleBuffer', 'on');
  set(gcf, 'Renderer', 'opengl');
  %set(gcf, 'Renderer', 'zbuffer');
  set(gca, 'XLim', [-2 2], 'YLim', [-2 2], 'ZLim', [-2 2]);
  set(gca, 'xgrid', 'on');
  set(gca, 'ygrid', 'on');
  set(gca, 'zgrid', 'on');
  set(gca, 'Projection', 'perspective');
  xlabel('x');
  ylabel('y');
  zlabel('z');
  title('Session A7: 3D Geometry');
  view(3);
  cameratoolbar('SetMode', 'orbit');
  cameratoolbar('Show');
  cla;
  
