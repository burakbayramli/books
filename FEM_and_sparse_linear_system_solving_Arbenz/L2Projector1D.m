function L2Projector1D(foo)

n = 5; % number of subintervals
h = 1/n; % mesh size
x = 0:h:1; % mesh

M = MassAssembler1D(x); % assemble mass
b = LoadAssembler1D(x,foo); % assemble load

Pf = M\b; % solve linear system

plot(x,Pf,'o',x,Pf,0:h/10:1,foo(0:h/10:1)) % plot L^2 projection
