function varargout=optimgui(varargin)
% OPTIMGUI
%  Graphical interface for Unconstrained optimization methods
%  as found in chapters 8-11 in "An Introduction to 
%  Constrained Optimization" by Chong and Zak.
%
%  To start, simply type 'optimgui' at the command line.
%  Requires MATLAB 5.2 (might run in 5.1 or even 5.0).
%
%  Uses Prof. Chong's secant based line search routine.
%  Minimizes the Rosenbrock, or "banana" function.  
%  As soon as you drag the starting location, or change
%  the method, the optimization begins.  

% T. Krauss  2/13/98

if nargin == 0
    init
    setfunc
    return
end

if (nargout == 0)
  feval(varargin{:});
else
  [varargout{1:nargout}] = feval(varargin{:});
end


%------------------------------------------------------------
function setfunc
% setfunc: set the function

funcMenu = findobj(gcf,'tag','PopupMenu2');
algMenu = findobj(gcf,'tag','PopupMenu1');  

if get(funcMenu,'value')==1,
  x=-2:.05:2;
  y=-1:.05:2.25;
  [X,Y]=meshgrid(x,y);
  f=f_rosenb([X(:)';  Y(:)']);
  f=reshape(f,length(y),length(x));
  contour(x,y,f,[1 3 7 16 32 64 128])
  hold on;
  xlabel('x_1')
  ylabel('x_2')
  %title('Minimization of Rosenbrock function')
  plot(1,1,'o')
  text(1,1,'Solution')
  grid on
  l = line(-1.2,2,'erasemode','xor','marker','o','linewidth',1.5,'color','r');
  assignin('base','l',l)
  h = line(-1.2,2,'erasemode','xor','linestyle','none','marker','x',...
     'markersize',14,'linewidth',1.5,'buttondownfcn','optimgui(''startdown'')');
  assignin('base','h',h)
  x0=findobj(gcf,'tag','x0');
  y0=findobj(gcf,'tag','y0');
  set(x0,'string',num2str(get(h,'xdata')))
  set(y0,'string',num2str(get(h,'ydata')))
else,
  disp('peaks not yet implemented')
  set(funcMenu,'value',1)
end

%------------------------------------------------------------
function x0change
h=evalin('base','h');
set(h,'xdata',str2num(get(findobj(gcf,'tag','x0'),'string')))
optimgui('optimize')

%------------------------------------------------------------
function y0change
h=evalin('base','h');
set(h,'ydata',str2num(get(findobj(gcf,'tag','y0'),'string')))
optimgui('optimize')

%------------------------------------------------------------
function startdown
% startdown.m:  button down function for starting point line
% of optim demo

set(gcf,'windowbuttonmotionfcn','set(h,''userdata'',''motion'')')
set(gcf,'windowbuttonupfcn','set(h,''userdata'',''up'')')

h=evalin('base','h');
x0=findobj(gcf,'tag','x0');
y0=findobj(gcf,'tag','y0');
f_h=findobj(gcf,'tag','f');

done=0;
while ~done
  waitfor(h,'userdata')
  msg = get(h,'userdata');
  switch msg
  case 'motion'
      pt = get(gca,'currentpoint');
      set(h,'xdata',pt(1,1),'ydata',pt(1,2))
      set(x0,'string',num2str(pt(1,1)))
      set(y0,'string',num2str(pt(1,2)))
      set(f_h,'string',num2str(optimgui('f_rosenb',[pt(1,1); pt(1,2)])))

  case 'up'
      done = 1;
  end
  set(h,'userdata',[])
end

set(gcf,'windowbuttonmotionfcn','')
set(gcf,'windowbuttonupfcn','')
optimgui('optimize')

%------------------------------------------------------------
function optimize
  funcMenu = findobj(gcf,'tag','PopupMenu2');
  algMenu = findobj(gcf,'tag','PopupMenu1');  
  iter_h=findobj(gcf,'tag','iter');
  f_h=findobj(gcf,'tag','f');
  step_h=findobj(gcf,'tag','step');
  steplabel_h=findobj(gcf,'tag','steplabel');
  Pushbutton1_h = findobj(gcf,'tag','Pushbutton1');
  set(Pushbutton1_h,'visible','on','userdata',[])
  x0=findobj(gcf,'tag','x0');
  y0=findobj(gcf,'tag','y0');
  
  h=evalin('base','h');
  set(h,'buttondownfcn','optimgui(''startdown''), optimgui(''stop'')')
  set([x0 y0 step_h],'enable','off')  
  
  l=evalin('base','l');

  am = get(algMenu,'value');

  switch am
  case {1,2,3,4,5,6}  % gradient or conjugate gradient
    k=0;
    x = [get(h,'xdata'); get(h,'ydata')];
    set(l,'xdata',x(1),'ydata',x(2))
    g = g_rosenb(x);
    d = -g;
    while norm(g)>1e-6
      set(iter_h,'string',num2str(k))
      set(f_h,'string',num2str(optimgui('f_rosenb',x)))

      if am>1  % do line search
          alpha=linesearch_secant('g_rosenb',x,d);
      else  % fixed step size
          alpha=str2num(get(step_h,'string'));
      end
      x = x + alpha*d;
      g1 = g_rosenb(x);
      switch am
      case {1,2}  % gradient
        beta = 0;
      case 3  % hestenes-stiefel
        beta = g1'*(g1-g)/(d'*(g1-g));
      case 4  % polak-ribiere
        beta = g1'*(g1-g)/(g'*g);
      case 5  % fletcher-reeves
        beta = (g1'*g1)/(g'*g);
      case 6  % powell
        beta = max(0,g1'*(g1-g)/(g'*g));
      end
      k = k+1;
      if rem(k,6)==0
          beta = 0;
      end
      g = g1;
      d = -g + beta*d;
      xd = get(l,'xdata'); yd=get(l,'ydata');
      set(l,'xdata',[xd(:); x(1)],'ydata',[yd(:); x(2)])

      drawnow
      if ~isempty(get(Pushbutton1_h,'userdata'))
          break
      end
    end
    
  case {7,8,9}  % quasi-newton
    k=0;
    H=eye(2);
    x = [get(h,'xdata'); get(h,'ydata')];
    set(l,'xdata',x(1),'ydata',x(2))
    g = g_rosenb(x);
    d = -H*g;
    while norm(g)>1e-6
      set(iter_h,'string',num2str(k))
      set(f_h,'string',num2str(optimgui('f_rosenb',x)))

      alpha=linesearch_secant('g_rosenb',x,d);
      x = x + alpha*d;
      
      g1 = g_rosenb(x);
      delx = alpha*d;
      delg = g1-g;
      switch am
      case 7  % rank-1
        z = delx - H*delg;
        H1 = H + (z*z')/(delg'*z);
      case 8  % DFP
        H1 = H + (delx*delx')/(delx'*delg) - (H*delg)*(H*delg)'/(delg'*H*delg);
      case 9  % BFGS
        H1 = H + (1+(delg'*H*delg)/(delg'*delx))*(delx*delx')/(delx'*delg) - ...
         (H*delg*delx'+(H*delg*delx')')/(delg'*delx);
      end
      k = k+1;
      if rem(k,6)==0
          H1 = eye(2);
      end
      H = H1;
      g = g1;
      d = -H*g;
      xd = get(l,'xdata'); yd=get(l,'ydata');
      set(l,'xdata',[xd(:); x(1)],'ydata',[yd(:); x(2)])

      drawnow
      if ~isempty(get(Pushbutton1_h,'userdata'))
          break
      end

   end

 case 10  % newton
    k=0;
    x = [get(h,'xdata'); get(h,'ydata')];
    set(l,'xdata',x(1),'ydata',x(2))
    g = g_rosenb(x);
    d = -inv(F_rosenb(x))*g;
    while norm(g)>1e-6
      set(iter_h,'string',num2str(k))
      set(f_h,'string',num2str(optimgui('f_rosenb',x)))

      alpha=linesearch_secant('g_rosenb',x,d);
      x = x + alpha*d;
      
      g = g_rosenb(x);
      d = -inv(F_rosenb(x))*g;

      xd = get(l,'xdata'); yd=get(l,'ydata');
      set(l,'xdata',[xd(:); x(1)],'ydata',[yd(:); x(2)])
      k=k+1;
 
      drawnow
      if ~isempty(get(Pushbutton1_h,'userdata'))
          break
      end
    end
end
set(Pushbutton1_h,'visible','off','userdata',[])
set(h,'buttondownfcn','optimgui(''startdown'')')
set([x0 y0],'enable','on')  
if am==1
  set([step_h steplabel_h],'enable','on')
else
  set([step_h steplabel_h],'enable','off')
end
  

%------------------------------------------------------------
function stop
  Pushbutton1_h = findobj(gcf,'tag','Pushbutton1');
  set(Pushbutton1_h,'userdata','stop')

%------------------------------------------------------------
function f=f_rosenb(x)
% rosenblatt's "banana" function
f = 100*(x(2,:)-x(1,:).^2).^2+(1-x(1,:)).^2;

function g=g_rosenb(x)
% Gradient of rosenblatt's "banana" function
g = [-400*(x(2,:)-x(1,:).^2).*x(1,:)-2*(1-x(1,:));
      200*(x(2,:)-x(1,:).^2) ];
      
function F=F_rosenb(x)
% Hessian of rosenblatt's "banana" function
F = [-400*(x(2,:)-3*x(1,:).^2)+2    -400*x(1,:);
      -400*x(1,:)   200];

%------------------------------------------------------------
function alpha=linesearch_secant(grad,x,d)
%Line search using secant method
%Note: I'm not checking for alpha > 0.

epsilon=10^(-5); %line search tolerance
max = 200; %maximum number of iterations
alpha_curr=0;
alpha=10^(-5);
dphi_zero=feval(grad,x)'*d;
dphi_curr=dphi_zero;
 
 i=0;
 while abs(dphi_curr)>epsilon*abs(dphi_zero),
   alpha_old=alpha_curr;
   alpha_curr=alpha;
   dphi_old=dphi_curr;
   dphi_curr=feval('optimgui',grad,x+alpha_curr*d)'*d;
   alpha=(dphi_curr*alpha_old-dphi_old*alpha_curr)/(dphi_curr-dphi_old);
   i=i+1;
   if (i >= max) & (abs(dphi_curr)>epsilon*abs(dphi_zero)),
      disp('Line search terminating with number of iterations:');
      disp(i);
      break;
   end
 end %while
 
 
%------------------------------------------------------------
function init

h0 = figure('Position',[176 62 672 646], ...
	'Tag','Fig1');
mat3=[0.119047   0.1532507    0.77529 0.719814];

h1 = axes('Parent',h0, ...
	'Position',mat3, ...
    	'Tag','Axes1');
    	
mat60 = {  'Gradient, fixed step'
    'Steepest Descent'
    'CG: Hestenes-Stiefel'
    'CG: Polak-Ribiere'
    'CG: Fletcher-Reeves'
    'CG: Powell'
    'Quasi-Newton: rank-1'
    'Quasi-Newton: DFP'
    'Quasi-Newton: BFGS'
    'Newton''s Method'};

h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'Callback','optimgui(''optimize'')', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.3675595238095238 0.03869969040247678 0.3898809523809523 0.03095975232198142], ...
	'String',mat60, ...
	'Style','popupmenu', ...
	'Tag','PopupMenu1', ...
	'Value',1);
	
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'Callback','optimgui(''setfunc'')', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.1220238095238095 0.04024767801857585 0.1830357142857143 0.03095975232198142], ...
	'String',{'banana';'peaks'}, ...
	'Style','popupmenu', ...
	'Tag','PopupMenu2', ...
	'Value',1);
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.1339285714285714 0.9411764705882353 0.07291666666666666 0.03869969040247678], ...
	'String','X0', ...
	'Style','text', ...
	'Tag','StaticText1');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.130952380952381 0.8947368421052629 0.07291666666666666 0.03869969040247678], ...
	'String','Y0', ...
	'Style','text', ...
	'Tag','StaticText1');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[1 1 1], ...
	'Callback','optimgui(''x0change'')', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.21131      0.93808      0.23958       0.0387], ...
	'String','-1.2', ...
	'Style','edit', ...
	'Tag','x0');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[1 1 1], ...
	'Callback','optimgui(''y0change'')', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[ 0.21131      0.89474      0.23958       0.0387], ...
	'String','2', ...
	'Style','edit', ...
	'Tag','y0');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'Enable','inactive', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.6205357142857143 0.8947368421052631 0.2410714285714286 0.03869969040247678], ...
	'String','2.1895', ...
	'Style','edit', ...
	'Tag','f');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'Enable','inactive', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.6205357142857143 0.9380804953560371 0.2410714285714286 0.03869969040247678], ...
	'String','0', ...
	'Style','edit', ...
	'Tag','iter');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.4672619047619048 0.8947368421052629 0.1488095238095238 0.03869969040247678], ...
	'String','f(Xn)', ...
	'Style','text', ...
	'Tag','StaticText1');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.4672619047619048 0.9380804953560373 0.1488095238095238 0.03869969040247678], ...
	'String','Iterations', ...
	'Style','text', ...
	'Tag','StaticText1');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[1 1 1], ...
	'Callback','optimgui(''optimize'')', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.7901785714285714 0.02631578947368422 0.1904761904761905 0.03869969040247678], ...
	'String','.001', ...
	'Style','edit', ...
	'Tag','step');
h1 = uicontrol('Parent',h0, ...
	'Units','normalized', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[0.79018     0.066563      0.16964       0.0387], ...
	'String','Step size:', ...
	'Style','text', ...
	'Tag','steplabel');
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','optimgui(''stop'')', ...
	'FontSize',10, ...
	'FontWeight','bold', ...
	'ListboxTop',0, ...
	'Position',[286.7586206896552 82.55172413793105 54.62068965517243 21.72413793103449], ...
	'String','STOP', ...
	'Tag','Pushbutton1', ...
	'Visible','off');
