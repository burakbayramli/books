%nm7p09.m to solve the warehouse location problem
f='sqrt([sum((x-[-16 4]).^2) sum((x-[6 5]).^2) sum((????????).^2)])';
fp_warehouse=inline([f '*[?;?;?]'],'x');
x0=[1 1]; A=[]; b=[]; Aeq=[]; beq=[]; l=[]; u=[];
xo=fmincon(fp_warehouse,x0,A,b,Aeq,beq,l,u,'fp_warehouse_c')
