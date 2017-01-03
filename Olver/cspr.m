function cspr(c,mode)
%
%    cspr(c,mode)
%
%  Show oscillations of a linear one-dimensional mass-spring system
%
%  c = vector indicating which combination of modes to be shown
%
%  mode = 'f'  -- both ends fixed
%         'd'  -- one free and one fixed end  (default if omitted)
%         'u'  -- both ends free
%
%   See also DYST, DSPR, GSPR, MSPR


if nargin ==1, mode = 'd'; end

n = size(c,2); C = diag(c);

global buttonid;

buttonid = 0;

icons = ['text(.5,.5,''pause''   ,''Horiz'',''center'')'
         'text(.5,.5,''input''   ,''Horiz'',''center'')'
         'text(.5,.5,''stop''    ,''Horiz'',''center'')'];
callbacks = ['button(1)'
             'button(2)'
             'button(3)'];
presstype =  ['toggle'
              'flash '
              'flash '];

hf = figure(1); 
clf;

bg = btngroup(hf,'GroupID','b','ButtonId',['p';'i';'s'],...
            'IconFunctions',icons,'GroupSize',[1 3],...
	    'CallBack',callbacks,'Position',[.3 0 .4 .07],...
            'PressType',presstype);


pa = 50; tol = .00001;

dt = .5;

A = diag(ones(n+1,1)) - diag(ones(n,1),-1);
A(:,n+1) = [];

switch mode
	case 'd'
		A(1,:) = []; 
		Z0 = []; Zn = n+1; 
		nn = n+1; n0 = 0; n1 = n+2;
	case 'u'
		A(n+1,:) = []; A(1,:) = []; 
		Z0 = []; Zn = [];  
		nn = n; n0 = -3; n1 = n+4;
	otherwise
		Z0 = 0; Zn = n+1; 
		nn = n+2; n0 = -1; n1 = n+2;
end


Z = [Z0,1:n,Zn];
n2 = n/2; U = n2 * ones(1,nn);
vn = [1:n]';


K = A' * A;

[E,D] = eig(K);
d = diag(D);
[d,I] = sort(d);
D = diag(d);
E = E(:,I);
l = sqrt(d);
L = diag(l);

zerol = abs(l) < tol;
zeroL = diag(zerol);

r = .5;

ax = axes('Position',[.15 .15 .75 .75]);

hold on;
sprs = plot([n2 n2],[Z(1),Z(nn)],'erasemode','xor');hold off;
hold off;
hold on;
plt = plot(U,Z,'.','erasemode','xor','markersize',24);
hold off;

axis([0 n n0 n1])



s = 0;

stopit = 0;

while stopit == 0 
  switch buttonid
   case 1  
	  buttonid = 0;
      while buttonid == 0, pause(.2); end 
	  buttonid = 0;
   case 2
      rst = 1;
	  disp(['Currently  r = ',num2str(r),'  dt = ',num2str(dt)]);
      keyboard
	  buttonid = 0;
   case 3
	  stopit = 1;
	  buttonid = 0;
  end

s = s + dt;

X = E * C * (sin(L*s) + zeroL*s);
M = vn + sum(X,2);
Z = [Z0,M',Zn];

%keyboard


	set(sprs,'ydata',[Z(1),Z(nn)]);

	set(plt,'ydata',Z);
drawnow


k = 0;
for i=1:1000*pa, k = 1-k; end

end



