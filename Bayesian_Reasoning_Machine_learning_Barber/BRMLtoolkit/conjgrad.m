function [x, errCG]=conjgrad(A,b,x0,opts)
%CONJGRAD conjudate gradients for minimising a quadratic 0.5*x'*A*x-x'*b
% [x, errCG]=conjgrad(A,b,x0,opts)
% x0 is the initial starting solution
% x is the return solution, and errCG the error 0.5*x'*A*x-x'*b
% opts.plotprogress: set to 1 to see the evolution of the quadratic
% opts.maxits : maximum number of iterations
% opts.tol : terminiation criterion for change in 0.5*x'*A*x-x'*b
x=x0;
g=A*x-b;
pme=-g;
err=realmax; errloop=[];
for loop=1:opts.maxits
    alphame=-(pme'*g)/(pme'*A*pme);
    x=x+alphame*pme;
    gnew=A*x-b;
    beta=(gnew'*gnew)/(g'*g);
    pme=-gnew+beta*pme;
    g=gnew;
    errCG = 0.5*x'*(g-b);    
    if opts.plotprogress; errloop=[errloop errCG]; plot(errloop,'-o'); drawnow; end
    if err-errCG<opts.tol; break;end
end