function A = dspr(n,mode)
%
%    A = dspr(n,mode)
%
%  Show the normal dynamical modes of oscillation
%    of a linear one-dimensional mass-spring system
%
%  n -- number of masses
% 
%  mode = 'f'  -- both ends fixed  (default if omitted)
%         'd'  -- one free and one fixed end
%         'u'  -- both ends free
%
%  A -- incidence matrix
%
%   See also DYST, CSPR, GSPR, MSPR, 


if nargin ==1, mode = 'f'; end

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


pa = 5525; tol = .00001;

dt = .5;

A = diag(ones(n+1,1)) - diag(ones(n,1),-1);
A(:,n+1) = [];

switch mode
	case 'd'
		A(1,:) = []; 
		Z0 = []; Zn = repmat(n+1,n,1); 
		V0 = []; Vn = [1:n]'; 
		nn = n+1; n0 = 0; n1 = n+2;
	case 'u'
		A(n+1,:) = []; A(1,:) = []; 
		Z0 = []; Zn = [];  
		V0 = []; Vn = []; 
		nn = n; n0 = 0; n1 = n+1;
	otherwise
		Z0 = repmat(0,n,1); Zn = repmat(n+1,n,1); 
		V0 = [1:n]'; Vn = [1:n]'; 
		nn = n+2; n0 = -1; n1 = n+2;
end

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

disp('Eigenvalues');
disp(' ');
disp(d');
disp('Eigenvectors');
disp(' ');
disp(E);
disp('Frequencies');
disp(' ');
disp(l');

r = .5;

ax = axes('Position',[.15 .15 .75 .75]);

W = repmat(1:n,n,1);
V = W';
Z = [Z0,W,Zn];
U = [V0,V,Vn];

axis([0 n+1 n0 n1])

sprs = zeros(1,nn);

for j=1:n,
   hold on; sprs(j) = plot([j j],[Z(j,1),Z(j,nn)],'erasemode','xor');hold off;
end

hold on;
plt = plot(U,Z,'.','erasemode','xor','markersize',24);
hold off;


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

M = V + E * r * (sin(L*s) + zeroL*s);
Z = [Z0,M',Zn];

%keyboard


for j=1:n,
	set(sprs(j),'ydata',[Z(j,1),Z(j,nn)]);
end
for j=1:nn,
	set(plt(j),'ydata',Z(:,j));
end
drawnow


k = 0;
for i=1:1000*pa, k = 1-k; end

end



