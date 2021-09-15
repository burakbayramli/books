% pde_ex1.m
% This program demonstrates the use of the
% command line interface of the pde toolkit
% for solving BVPs in 2-D.
function iflag = pde_ex1();
iflag = 0;

show_all_figs = 0;

% using geometry function polygon1_geom below,
% show geometry of the computational domain
figure;
pdegplot('polygon1_geom');

% initialize mesh
[P,E,T] = initmesh('polygon1_geom');
% jiggle mesh
P = jigglemesh(P,E,T);
% plot mesh
if(show_all_figs)
    figure;
end
pdemesh(P,E,T);
% do global refinement
[P2,E2,T2] = refinemesh('polygon1_geom',P,E,T);
if(show_all_figs)
    figure;
end
pdemesh(P2,E2,T2);

% demonstrate Delaunay tesselation directly
% for points in initial mesh
x = P(1,:)';  y = P(2,:)';
tri = delaunay(x,y);
if(show_all_figs)
    figure;
end
triplot(tri,x,y);

% form voronoi polyhedra
[vx,vy] = voronoi(x,y,tri);
if(show_all_figs)
    figure;
end
voronoi(x,y,tri);

% retain fine mesh and discard initial one
P = P2; E = E2;  T = T2;
clear P2 E2 T2;

% compute values of f = x^2 + y^2 at node points
x = P(1,:)';  y = P(2,:)';
f = zeros(size(x));
f = (x.^2) + (y.^2);

% plot f(x,y) in various formats
% 3-D surface plot showing mesh
if(show_all_figs)
    figure;
end
pdeplot(P,E,T,'zdata',f);
% produce smoother plot by interpolating to
% standard (x,y) positions and using regular
% plotting routine
if(show_all_figs)
    figure;
end
pdeplot(P,E,T,'xydata',f,'zdata',f);
% switch to more common color scheme and add title
if(show_all_figs)
    figure;
end
pdeplot(P,E,T,'xydata',f,'zdata',f,...
    'colormap','jet','title','f = x^2 + y^2');
xlabel('x');  ylabel('y');
% make 2-D filled contour plot
if(show_all_figs)
    figure;
end
pdeplot(P,E,T,'xydata',f,'zdata',f,...
    'colormap','jet','title','f = x^2 + y^2', ...
    'zstyle','off');
xlabel('x');  ylabel('y');


% Now, we solve Poisson equation with source term f
% using the adaptive mesh solver for elliptic PDEs.
% type doc adaptmesh to see syntax, and doc assempde
% for information on how we specify coefficients of PDE
geom_mfile = 'polygon1_geom';
bound_mfile = 'pde_ex1_bound';
c = 1;  a = 0;  f = 'pde_ex1_f';
[u,P2,E2,T2] = adaptmesh(...
    'polygon1_geom', 'pde_ex1_bound', ... % names of m-files
    c, a, f);  % coefficients in PDE
% plot the solution
figure;
pdeplot(P2,E2,T2,'xydata',u,'zdata',u,...
    'colormap','jet','title','u(x,y)', ...
    'zstyle','off');
xlabel('x');  ylabel('y');

% Finally, make a master plot
figure;
% mesh returned from adaptmesh
subplot(2,2,1);  pdemesh(P2,E2,T2);
title('Mesh with adaptive refinement during solution');
axis([0 3 0 3]);
% source function
subplot(2,2,2);
x = P(1,:)';  y = P(2,:)';
f = (x.^2) + (y.^2);
pdeplot(P,E,T,'xydata',f,'zdata',f,...
    'colormap','jet','title','f = x^2 + y^2', ...
    'zstyle','off');
axis([0 3 0 3]);
% solution, filled contour plot
subplot(2,2,3);
pdeplot(P2,E2,T2,'xydata',u,'zdata',u,...
    'colormap','jet','title','u(x,y)', ...
    'zstyle','off');
axis([0 3 0 3]);
% compute gradient of solution
[ux,uy] = pdegrad(P2,T2,u);
subplot(2,2,4);
pdeplot(P2,E2,T2,'xydata',u,'zdata',u, ...
    'colormap','jet', 'zstyle','off', ...
    'flowdata', [ux; uy]);
axis tight;

% save results to .mat file
save pde_ex1.mat;

iflag = 1;
return;


