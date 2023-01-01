%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 5.2
%% Integration of test function with Gauss quadrature
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[x,w]=SPE_xwGauss(20);
fprintf('function exp: exact int.=%f Gauss quad.=%f\n',...
                                          exp(1.)-exp(-1.),exp(x)'*w)

n=10
IPmat=quad(@exp,-1,1);
for i=2:n
 [x,w]=SPE_xwGauss(i);
 fprintf('%d points for exp : exact int.=%f  \n Gauss quad.%f \n  matlab quad.=%f\n',...
                         i,exp(1.)-exp(-1.),exp(x)'*w,IPmat)
end 
nbeval=1000; % number of integral evaluations
tic
n=4;                  % 4 points are enough to get 
[x,w]=SPE_xwGauss(n);     % 6 significant digits in the approximation
for i=1:nbeval
  exp(x)'*w;
end	  
cpu_qg=toc;
tic;
for i=1:nbeval
  IPmat=quad(@exp,-1,1);
end	  
cpu_mat=toc;
fprintf(...
' %d evaluations of Quad Gauss in %f seconds, of quad in %f secondes',...
nbeval,cpu_qg,cpu_mat); 
