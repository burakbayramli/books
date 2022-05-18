%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [iter,cpu]=DDM_FunSchwarz1d(no,detailed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercises 8.1 and 8.2
%% Schwarz  method of domain decomposition with overlapping 
%% monodimensionnal case with two subdomains
%% Finite differences solution of the boundary conditions problem
%%  -u"+cu=f  on [a,b]
%%  u(a)=ua
%%  u(b)=ub
%%  no is the half number of intervals in the overlapping region
%%  detailed = 1 for intermediate plots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
close all
tic
ua=0.1;     % boundary conditions sol(a)=ua and sol(b)=ub
ub=0;
a=0;        % bounds of the interval of study
b=1;
eps=0.001;    % tolerance for the convergence of the Schwarz method
nx=501;      % number of discretization points
h=(b-a)/(nx+1);   % space discretization step
c=10.;   % stiffness coefficient
f='DDM_rhs1d'; % right hand side function
over=no*h;  
xl=0.5*(a+b)-over;
xr=0.5*(a+b)+over;
nl=(nx+1)/2+no-1;  % number of intervals in each subdomain
nr=nl;  
Xr=linspace(xl,b,nr+2)';
Xl=linspace(a,xr,nl+2)';
X=linspace(a,b,nx+2)';
plot(X,feval(f,X))
title('Right hand side')
nalpha=2*no;          % index of point in Ul where 
% the boundary condition for Ur is obtained 
nbeta=(nx+1)/2-no;  % index of point in Ur where 
% the boundary condition for Ul is obtained 
%
% For comparison sake, the global solution is computed on the whole domain withe the standard finite
% differences method 
%
A=toeplitz([2+c*h^2,-1,zeros(1,nx-2)])/h^2;  
ftot=feval(f,X(2:nx+1));
ftot(1)=ftot(1)+ua/h^2;
ftot(nx)=ftot(nx)+ub/h^2;
sol=A\ ftot;
sol=[ua,sol',ub];
%
% matrix for the subdomains
Asub=toeplitz([2+c*h^2,-1,zeros(1,nl-2)])/h^2;
% right hand side for the left subdomain (without the boundary condition at  xr)
fleft=feval(f,Xl(2:nl+1)); fleft(1)=fleft(1)+ua/h^2;
% right hand side for the right subdomain (without the boundary condition at  xl)
fright=feval(f,Xr(2:nl+1));    fright(nr)=fright(nr)+ub/h^2;

% arbitrary value for the boundary condition u_l(x_r)=alpha at the first iteration
alpha=0.1;  
maxiter=200;
errl=[];errr=[];ERR=[];

iter =0;
erro=1.;
while iter<maxiter & erro>eps    
%    figure % uncomment this line to open a new figure at each iteration
    iter=iter+1;
    % boundary conditions
    fl=fleft;
    fl(nl)=fl(nl)+alpha/h^2;
    U=Asub\ fl;
    Ul=[ua,U',alpha];
%   
    beta=U(nbeta);
    % boundary conditions
    fr=fright;
    fr(1)=fr(1)+beta/h^2;
 
    U=Asub\ fr;
    Ur=[beta,U',ub];
    alpha=U(nalpha);
%    
    errl=[errl,norm(sol(1:nl+2)-Ul)];  % error on left subdomain
    errr=[errr,norm(sol(nx-nr+1:nx+2)-Ur)]; % error on right subdomain
    erro=norm(Ul(nl+2-2*no:nl+2)-Ur(1:2*no+1)); % error on overlapping region
    ERR=[ERR,erro];         
    if  detailed== 1
      plot(X,sol,'b',Xl,Ul,'r',Xr,Ur,'g');
      legend('total','Uleft','Uright');
      title(strcat('Iteration number  ',int2str(iter)) )
      pause(0.1)
    end
end
cpu=toc;
if detailed == 1
  fprintf('convergence reached in %d iterations\n',iter);
  figure
  plot([1:iter],log(ERR),'-*b',[1:iter],log(errl),'-og',[1:iter],log(errr),'-xr')
  legend('overlapping','left','right')
  xlabel('iter')
  ylabel('log(err)')
  title('Error evolution')
end