function phasem(o,ti)
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

if nargin < 2, ti = 200; end

global buttonid;

buttonid = 0;

icons = ['text(.5,.5,''start''   ,''Horiz'',''center'')'
         'text(.5,.5,''pause''   ,''Horiz'',''center'')'
		 'text(.5,.5,''stop''    ,''Horiz'',''center'')'];
callbacks = ['button(1)'
             'button(2)'
			 'button(3)'];
presstype =  ['flash '
			  'toggle'
              'flash '];

hf = figure(2); 
clf;

bg = btngroup(hf,'GroupID','b','ButtonId',['i';'p';'s'],...
            'IconFunctions',icons,'GroupSize',[1 3],...
	    'CallBack',callbacks,'Position',[.3 0 .4 .07],...
            'PressType',presstype);


pa = 10;

colors = ['b','g','r','c','m','y'];
ncolors = size(colors,2);
ic = 1;

tso = ti; ts = [0 tso];
axo = 4 * [-1 1 -1 1];

zfac = 1; 
ax = zfac * axo;

vfo = .1;

figure(1);
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
			disp(['Initial data :  ',num2str(x0),'  ',num2str(y0)]);
		end
		
		if idfl == 1
			u0 = [x0 y0]';
    		[t,y] = ode45(o,ts,u0);
			hold on; plot(y(:,1),y(:,2),colors(ic)); hold off;
% 			[t,y] = ode45(o,-ts,u0);
% 			hold on; plot(y(:,1),y(:,2),colors(ic)); hold off;

			ic = ic + 1; if ic > ncolors, ic = 1; end
		end

	figure(2);
	
	pen = plot([0 sin(x0)],[0,-cos(x0)],'erasemode','xor');
    hold on;
	bob = plot(sin(x0),-cos(x0),'.','erasemode','xor','markersize',24);
	hold off;
	axis(1.3*[-1 1 -1 1]);

startix = 0;
	  buttonid = 0;

while startix == 0 
  if buttonid == 1, startix =1; end
  if buttonid == 3, startix =-1; end
  pause(.2);
end 

if startix ==1

buttonid = 0;
stopix = 0;
i = 1;
n = size(y,1);
 
while stopix == 0 
  switch buttonid
   case 1  
	  buttonid = 0;
      while buttonid == 0, pause(.2); end 
	  buttonid = 0;
   case 2  
	  buttonid = 0;
      while buttonid == 0, pause(.2); end 
	  buttonid = 0;
   case 3
	  stopix = 1;
	  buttonid = 0;
  end

	set(pen,'xdata',[0, sin(y(i,1))],'ydata',[0, -cos(y(i,1))]);
	set(bob,'xdata',sin(y(i,1)),'ydata',-cos(y(i,1)));
	drawnow

i = i + 1; if i > n, stopix = 1; end

	kk = 0;
	for ii=1:1000*pa, kk = 1-kk; end

end
end

figure(1);
	
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
