function phaseg(o,ti)
%
%  phasem(o,ti)
%
%  Phase portrait of 2D o.d.e. given in file o
%    ti - time interval
%
% Use cursor and mouse button to input initial data
% keyboard for other options:
%
% 'i' - type initial data
% 'k' - keyboard
% 'q' - quit
% 'r' - reset zoom
% 't' - change time interval
% 'w' - zoom out
% 'z' - zoom in
% all others - print options
%
%   See also GO2

if nargin < 2, ti = 50; end

pa = 10;
maxy = 1;

colors = ['b','g','r','c','m','y'];
ncolors = size(colors,2);
ic = 1;

tso = ti; ts = [0 tso];
axo = 4 * [-1 1 -1 1];

zfac = 1; 
ax = zfac * axo;

vfo = .1;

figure(2); clf;
figure(1);clf;

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
				disp(['Initial data :  ',num2str(x0),'  ',num2str(y0)]);
				idfl = 1;
			else
				disp('Initial data must be row vector with 2 entries');
				idfl = 0;
			end
		else
			disp(['Initial data :  ',num2str(x0),'  ',num2str(y0)]);
			idfl = 1;
		end
		
	if idfl == 1
			u0 = [x0 y0]';
    		[t,y] = ode45(o,ts,u0);
			hold on; plot(y(:,1),y(:,2),colors(ic)); hold off;
% 			[t,y] = ode45(o,-ts,u0);
% 			hold on; plot(y(:,1),y(:,2),colors(ic)); hold off;

	figure(2);
		maxi = max(abs(y(:,1))) + .5;
		maxy = max(maxy,maxi);
    	hold on; plot(t,y(:,1),colors(ic)); hold off;
		axis([0,tso,-maxy,maxy]);

	figure(1);
	ic = ic + 1; if ic > ncolors, ic = 1; end
	
	end


	case 107 % 'k' - keyboard
		keyboard
	case 113 % 'q' - quit
		stopit = 1;
	case 114 % 'r' - reset zoom
		zfac = 1; 
		ax = zfac * axo;
		axis(ax);
	case 116 % 't' - change time interval
		tso = input('Input time interval :');  ts = [0 tso];
	case 119 % 'w' - zoom out
		zfac = 2* zfac;
		ax = [x0 x0 y0 y0 ] + zfac * axo;
		axis(ax);
	case 122 % 'z' - zoom in
		zfac = zfac/2;
		ax = [x0 x0 y0 y0 ] + zfac * axo;
		axis(ax);
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
