%nm733 to solve a Linear Programming problem.
% Min f*x=-3*x(1)-2*x(2) s.t. Ax<=b, Aeq=beq and l<=x<=u
x0=[0 0]; %initial point
f=[-3 -2]; %the coefficient vector of the objective function
A=[3 4; 2 1]; b=[7; 3]; %the inequality constraint Ax<=b
Aeq=[-3 2]; beq=2; %the equality constraint Aeq*x=beq
l=[0 0]; u=[10 10]; %lower/upper bound l<=x<=u
[xo_lp,fo_lp]=linprog(f,A,b,Aeq,beq,l,u)
cons_satisfied=[A; Aeq]*xo_lp-[b; beq] %how constraints are satisfied
f733o=inline('-3*x(1)-2*x(2)', 'x');
[xo_con,fo_con]= fmincon(f733o,x0,A,b,Aeq,beq,l,u)
