%nm732_1 to solve a constrained optimization problem by fmincon()
clear, clf
ftn='((x(1)+1.5)^2+5*(x(2)-1.7)^2)*((x(1)-1.4)^2+.6*(x(2)-.5)^2)';
f722o=inline(ftn,'x');
x0=[0 0.5] %initial guess
A=[]; B=[]; Aeq=[]; Beq=[]; %no linear constraints
l=-inf*ones(size(x0)); u=inf*ones(size(x0)); % no lower/upperbound
options= optimset('LargeScale','off'); %just [] is OK.
[xo_con,fo_con]=fmincon(f722o,x0,A,B,Aeq,Beq,l,u,'f722c',options) 
[co,ceqo]=f722c(xo_con) % to see how constraints are.
