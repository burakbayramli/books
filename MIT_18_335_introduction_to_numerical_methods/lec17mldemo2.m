% MIT 18.335 - Lecture 17 MATLAB Demo 2
% The Divide-and-Conquer Algorithm
% Per-Olof Persson, November 14, 2007

format long

% Create symmetric matrix A
n=6;
randn('state',2)
A0=randn(n,n); A0=A0+A0';
A0,pause

% Reduce to tridiagonal
A=hess(A0);
A,pause

% Split
beta=A(n/2+1,n/2),pause
T1=A(1:n/2,1:n/2),pause
T2=A(n/2+1:n,n/2+1:n),pause

That1=T1;
That1(n/2,n/2)=T1(n/2,n/2)-beta;
That1,pause

That2=T2;
That2(1,1)=T2(1,1)-beta;
That2,pause

% Compute eigenvalues of That1,That2 exactly
% In the real algorithm this would be recursion
[Q1,D1]=eig(That1);
[Q2,D2]=eig(That2);

d=[diag(D1);diag(D2)],pause
z=[Q1(end,:), Q2(1,:)]',pause
neg=beta<0;
w=z*sqrt(abs(beta));

ll=-12:0.01:12;
ff=1+0*ll;
for ii=1:n
  ff=ff+(-1)^neg*w(ii).^2./(d(ii)-ll);
end

plot(ll,ff)
axis([-12,12,-5,5])
line([-12,12],[0,0],'color','k')
for ii=1:n
  line([d(ii),d(ii)],[-5,5],'linestyle','--','color',[.5,.5,.5])
end
pause

% Compare with true eigenvalues
l=eig(A0), pause
line(l,0*l,'linestyle','none','marker','.','markersize',16,'color',[0,.5,0])
