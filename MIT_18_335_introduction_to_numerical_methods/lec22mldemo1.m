% MIT 18.335 - Lecture 22 MATLAB Demo 1
% Conjugate Gradient
% Per-Olof Persson, December 3, 2007

% Create 2-by-2 linear system
A=[3,1; 1,2];
x0=[2;-2];
b=A*x0;

% Evaluate and plot contours of phi(x,y)
[xx,yy]=meshgrid(-3:.1:5,-5:.1:3);
phi=0*xx;
for i=1:prod(size(xx))
  x=[xx(i); yy(i)];
  phi(i)=.5*x'*A*x-b'*x;
end
phi=phi-min(phi(:));
levels=(0:.5:sqrt(max(phi(:)))).^2;
contour(xx,yy,phi,levels),axis equal,grid on
pause

% Run steepest descent starting at [-2,1], plot path
x=steep(A,b,[-2;1]);
line(x(1,:),x(2,:),'color','k','linewidth',2)
pause

% Plot eigenvectors
[V,D]=eig(A);
par={'linestyle','--','color','r'};
line([x0(1)-5*V(1,1),x0(1)+5*V(1,1)],[x0(2)-5*V(2,1),x0(2)+5*V(2,1)],par{:});
line([x0(1)-3*V(1,2),x0(1)+3*V(1,2)],[x0(2)-3*V(2,2),x0(2)+3*V(2,2)],par{:});

% Run steepest descent with other starting locations
x=steep(A,b,[-2;-3]);
line(x(1,:),x(2,:),'color','g','linewidth',2)
pause
x=steep(A,b,x0-4*V(:,1));
line(x(1,:),x(2,:),'color','b','linewidth',2)
pause
x=steep(A,b,x0+2*V(:,2));
line(x(1,:),x(2,:),'color','m','linewidth',2)
pause
x=steep(A,b,[-2;2]);
line(x(1,:),x(2,:),'color','r','linewidth',2)
pause

% Clean up, plot only first steepest descent path
contour(xx,yy,phi,levels),axis equal,grid on
line([x0(1)-5*V(1,1),x0(1)+5*V(1,1)],[x0(2)-5*V(2,1),x0(2)+5*V(2,1)],par{:});
line([x0(1)-3*V(1,2),x0(1)+3*V(1,2)],[x0(2)-3*V(2,2),x0(2)+3*V(2,2)],par{:});
x=steep(A,b,[-2;1]);
line(x(1,:),x(2,:),'color','k','linewidth',2)
pause

% Run conjugate directions
x=conjdir(A,b,[-2;1]);
line(x(1,:),x(2,:),'color','b','linewidth',2)
pause

% Run conjugate gradients
x=conjgrad(A,b,[-2;1]);
line(x(1,:),x(2,:),'color','g','linewidth',2)
pause
