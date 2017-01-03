function [E,D] = go3(A,dt,et)
%
%  [E,D] = go3(A,dt,et)
%
%  Phase portrait of 3D linear o.d.e.
%      u' = A * u
%
% Optional arguments:
%    dt - forward time interval
%    et - backward time interval
%
%  E - eigenvectors of A
%  D - eigenvalues
%
% initial data input is either a
%    k by 3 matrix giving k initial points
%    positive scalar indicating a zoom factor
%    negative scalar for default initial data
%    0 meaning to quit
%
%   See also GO2, LO3

colors = ['b','g','r','c','m','y','k'];
ncolors = size(colors,2);
ic = 1;

global Ao bo

Ao = A; bo = [0;0;0];

tso = 25;
switch nargin 
  case 1, tsp = [0 tso]; tsm = -tsp;
  case 2, tsp = [0 dt]; tsm = -tsp;
  otherwise tsp = [0 dt]; tsm = -[0 et];
end

[E,D] = eig(A);
disp('Eigenvalues')
disp(diag(D))
disp(' ')
disp('Eigenvectors')
disp(E)


ax = 10 * [-1 1 -1 1 -1 1];

clf;
axis(ax);
zoom on;
rotate3d on;

stopit = 0;

while stopit == 0

y0 = input('Initial Data : ');

if y0 < 0,
 		y0 = [1 1 1; 1 1 -1; -1 1 1; -1 1 -1];
end

if size(y0,2) == 1
    if y0 == 0
		stopit = 1;
	else
		zoom(y0);
	end
else
  for i=1:size(y0,1)
	[t,y] = ode45('lo3',tsp,y0(i,:));
	hold on; plot3(y(:,1),y(:,2),y(:,3),colors(ic)); hold off;
	[t,y] = ode45('lo3', tsm,y0(i,:));
	hold on; plot3(y(:,1),y(:,2),y(:,3),colors(ic)); hold off;

	ic = ic + 1; if ic > ncolors, ic = 1; end 
  end
end
end
