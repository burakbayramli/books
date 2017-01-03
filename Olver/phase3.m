function phase3(o,ti,ax)
%
%  phase3(o,ti,ax)
%
%  Phase portrait of 3D o.d.e. given in file o
%    ti - time interval (default 200)
%    ax - axis info for plot
%
% Use keyboard to input initial data
%
%   See also PHASE

if nargin < 2, ti = 200; end
if nargin < 3, ax = 4 * [-1 1 -1 1 -1 1]; end

colors = ['b','g','r','c','m','y'];
ncolors = size(colors,2);
ic = 1;

tso = ti; ts = [0 tso];

clf;
gax = axes;
axis(ax);
rotate3d;

stopit = 0;

while stopit == 0

			id = input('Initial Data : ');
			if size(id) == [1 3]
				x0 = id(1); y0 = id(2); z0 = id(3);
				idfl = 1;
			else
				disp('Initial data must be row vector with 3 entries');
				idfl = 0;
			end
		
		if idfl == 1
			u0 = [x0 y0 z0]';
    		[t,y] = ode23(o,ts,u0);
			hold on; plot3(y(:,1),y(:,2),y(:,3),colors(ic)); hold off;
			[t,y] = ode45(o,-ts,u0);
			hold on; plot3(y(:,1),y(:,2),y(:,3),colors(ic)); hold off;

			ic = ic + 1; if ic > ncolors, ic = 1; end
		end
		
end
