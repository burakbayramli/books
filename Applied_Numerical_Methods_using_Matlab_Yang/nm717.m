%nm717 to minimize an objective function f(x) by various methods.
clear, clf
f=inline('x(1)^4-16*x(1)^2-5*x(1)+x(2)^4-16*x(2)^2-5*x(2)','x');
l=[-5 -5]; u=[5 5]; %lower/upperbound
x0=[0 0]
[xo_nd,fo]= opt_Nelder(f,x0)
[xos,fos]=fminsearch(f,x0) %cross-check by MATLAB built-in routines
[xou,fou]=fminunc(f,x0)
kmax=500; q=1; TolFun=1e-9;
[xo_sa,fo_sa]=sim_anl(f,x0,l,u,kmax,q,TolFun)
% Apply the genetic algorithm (GA) 
Np=30; %population size
Nb=[12 12]; %the numbers of bits for representing each variable
Pc=0.5; Pm=0.01; %Probability of crossover/mutation
eta=1; kmax=100; %learning rate and the maximum # of iterations
[xo_gen,fo_gen]=genetic(f,x0,l,u,Np,Nb,Pc,Pm,eta,kmax)
