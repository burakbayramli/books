%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise  8.2
%% Schwarz  method of domain decomposition with overlapping 
%% monodimensionnal case with two subdomains
%% Finite differences solution of the boundary conditions problem
%%  -u"+cu=f  on [a,b]
%%  u(a)=ua
%%  u(b)=ub
%% Study of the performances versus the size of the overlapping region
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
close all
a=0;        % bounds of the interval of study
b=1;
nx=501;      % number of discretization points
h=(b-a)/(nx+1);   % space discretization step
% no is the half number of intervals in the overlapping region
ITER=[];CPU=[];
NO=[1:floor(nx/10)];
for no=NO
   fprintf('no=%d',no)
   [iter,cpu]=DDM_FunSchwarz1d(no,0);
   ITER=[ITER;iter];
   CPU=[CPU;cpu];
   fprintf(' iter=%d cpu=%f \n',iter,cpu)
end
plot(NO,ITER,'-or')
title('iterations versus overlapping size')
xlabel('NO')
ylabel('# iter')
figure
plot(NO,CPU,'-or')
title('CPU versus overlapping size')
xlabel('NO')
ylabel('CPU')
figure
plot(ITER,CPU,'-or')
title('cpu versus iterations')
xlabel('# iter')
ylabel('CPU')
