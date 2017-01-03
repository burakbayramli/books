function heat(bc,L,c,n,dt,figno)

if nargin < 1, bc = 'f'; end
if nargin < 2, L = pi; end
if nargin < 3, c = 1; end
if nargin < 4, n = 256; end
if nargin < 5, dt = .005; end
if nargin < 6, figno = 1; end

global buttonid;

buttonid=0;

icons = ['text(.5,.5,''pause''     ,''Horiz'',''center'')'
         'text(.5,.5,''show time'' ,''Horiz'',''center'')'
	 	 'text(.5,.5,''restart''   ,''Horiz'',''center'')'
         'text(.5,.5,''init. data'',''Horiz'',''center'')'
         'text(.5,.5,''input''     ,''Horiz'',''center'')'
	 	 'text(.5,.5,''stop''      ,''Horiz'',''center'')'];
callbacks = ['button(1)'
             'button(2)'
             'button(3)'
   	    	 'button(4)'
             'button(5)'
             'button(6)'];
presstype =  ['toggle'
              'flash '
              'flash '
              'flash '
              'flash '
	    	  'flash '];

hf = figure(figno); 

bg = btngroup(hf,'GroupID','b','ButtonId',['p';'s';'r';'0';'d';'i'],...
            'IconFunctions',icons,'GroupSize',[1 6],...
	    'CallBack',callbacks,'Position',[.1 0 .8 .07],...
            'PressType',presstype);

h = L/n;

l = dt*c^2/h^2; l2 = l/2;

B = (1 - l) * diag(ones(n+1,1)) +  l2 *( diag(ones(n,1),1) + diag(ones(n,1),-1));
A = (1 + l) * diag(ones(n+1,1)) -  l2 *( diag(ones(n,1),1) + diag(ones(n,1),-1));

switch bc 
	case 'p'
		B(1,n+1) = l; B(n+1,1) = l;
		A(1,n+1) = -l; A(n+1,1) = -l;
	case 'n'
		B(1,n+1) = l2; B(n+1,1) = l2;
		A(1,n+1) = -l2; A(n+1,1) = -l2;
end

C = A\B;

x = linspace(0,L,n+1)';

x0 = 1;
f0 = 0*x;
del = f0; del(25) = 1;
f1 = exp(-40*(x-x0).^2);
f2 = (x < 1) .*x + (x>1 & x < 2.8) .* (3 - 2*x) + (x > 2.8) .* (x - pi) * 2.6 / (pi - 2.8); 
f3 = 1 - abs(2*x/L - 1) + .1 * sin(50*x);

f = f1; 
      
clf;
ax = axes('Position',[.15 .17 .75 .75]);
A = 2;
t=0; 

sol= plot(x,f,'EraseMode','background');
axis([0 L -A A]);
pause

w = f;

stopit = 0;

while stopit == 0 
  switch buttonid
   case 1  
      title(['Time: ',num2str(t)]);
	  buttonid = 0;
      while buttonid == 0, pause(.2); end 
	  buttonid = 0;
   case 2
      title(['Time: ',num2str(t)]);
	  buttonid = 0;
   case 3
      clf;
	  sol= plot(x,f,'EraseMode','background');
	axis([0 L -A A]);
	w = f;
	buttonid = 0;
   case 4
      title(['Time: ',num2str(t)]);
	  f = input('Initial temperature: ');
       clf;
	  sol= plot(x,f,'EraseMode','background');
	axis([0 L -A A]);
	pause
	w = f;
	buttonid = 0;
   case 5
      title(['Time: ',num2str(t)]);
	  keyboard
	  buttonid = 0;
   case 6
	  stopit = 1;
      title(['Time: ',num2str(t)]);
	  buttonid = 0;
  end

	w = C * w;
	
	set(sol,'ydata',w); drawnow; 

	t=t+dt;

end



