function [E,D] = go2(A,b)
%
%  [E,D] = go2(A,b)
%
%  Phase portrait of 2D linear o.d.e.
%      u' = A * u + b
%
%    b = 0 if not given
%
%  E - eigenvectors of A
%  D - eigenvalues
%
% Use cursor and mouse button to input initial data
% keyboard for other options:
%
% 'i' - type initial data
% 'e' or 'v' - plot eigendirections
% 'k' - keyboard
% 'p' - print eigenvalues and vectors
% 'q' - quit
% 'r' - reset zoom
% 't' - change time interval
% 'w' - zoom out
% 'z' - zoom in
% all others - print options
%
%   See also LO2, GO3

colors = ['b','g','r','c','m','y'];
ncolors = size(colors,2);
ic = 1;

global Ao bo

if nargin == 1, bo = [0;0]; else bo = b; end
Ao = A;

[E,D] = eig(A);
if isreal(E)
	e1 = D(1,1); e2 = D(2,2);
	ev1 = E(:,1)'; ev2 = E(:,2)';
	nv1 = ev1; nv2 = ev2;
	realev = 1;
else
	e1 = D(1,1); 
	ev = E(:,1)'; ev1 = real(ev); ev2 = imag(ev);
	nv1 = ev1/norm(ev1); nv2 = ev2/norm(ev2);
	realev = 0;
end

tso = 20; ts = [0 tso];
xfa = 10; xfo = 20; vfo = .04;
ax = xfa * [-1 1 -1 1];

clf;
gax = axes;
axis(ax);
zoom on;

stopit = 0;

while stopit == 0

[x0 y0 n] = ginput(1);

if size(n) == [0 0]
	stopit = 1;
else
switch n
	case {1,105} % mouse button - input initial data
				 % 'i' - keyboard initial data
		if n == 105
			id = input('Initial Data : ');
			if size(id) == [1 2]
				x0 = id(1); y0 = id(2);
				idfl = 1;
			else
				disp('Initial data must be row vector with 2 entries');
				idfl = 0;
			end
		else
			idfl = 1;
		end
		
		if idfl == 1
			u0 = [x0 y0]';
    		[t,y] = ode45('lo2',ts,u0);
			hold on; plot(y(:,1),y(:,2),colors(ic)); hold off;
			[t,y] = ode45('lo2',-ts,u0);
			hold on; plot(y(:,1),y(:,2),colors(ic)); hold off;

			xx = get(gax,'Xlim'); vf = vfo * xx(2);
			v0 = Ao * u0 - bo; v0 = vf * v0/norm(v0);
			wd = v0(1) - v0(2); ws = v0(1) + v0(2);
			hold on; 
			plot([x0 - wd,x0,x0 - ws],[y0 - ws,y0,y0 + wd],colors(ic));
	    	hold off;
			
			ic = ic + 1; if ic > ncolors, ic = 1; end
		end
		
	case {101,118} % 'e' or 'v' - plot eigendirections
		ez1 = xfo * nv1' *[-1 1]; ez2 = xfo * nv2' *[-1 1];
		hold on; plot(ez1(1,:),ez1(2,:),'k'); 
		         plot(ez2(1,:),ez2(2,:),'k'); hold off;
	case 107 % 'k' - keyboard
		keyboard
	case 112 % 'p' - print eigenvalues and vectors
	    if realev == 1
		  disp(['Eigenvalue: ', num2str(e1),'   Eigenvector: [',num2str(ev1),']''']);
		  disp(['Eigenvalue: ', num2str(e2),'   Eigenvector: [',num2str(ev2),']''']);
		else
		  disp(['Eigenvalue: ', num2str(e1),...
		       '   Eigenvector: [',num2str(ev1),']'' + i * [',num2str(ev2),']''']);
		end
	case 113 % 'q' - quit
		stopit = 1;
	case 114 % 'r' - reset zoom
		zoom out; %vf = vfo;
	case 116 % 't' - change time interval
		tso = input('Input time interval :');  ts = [0 tso];
	case 119 % 'w' - zoom out
		zoom(.5); %vf = vf / .5;
	case 122 % 'z' - zoom in
		zoom(2.); %vf = vf / 2.;
	otherwise
		disp( 'Options are:')
		disp('i - type initial data')
		disp('e or v - plot eigendirections')
		disp('k - keyboard')
		disp('t - change time interval')
		disp('p - print eigenvalues and vectors')
		disp('r - reset zoom')
		disp('w - zoom out')
		disp('z - zoom in')
		disp('q - quit')
end
end
end
