%nm713.m: do_Nelder
f713= inline('x(1)*(x(1)-4-x(2)) +x(2)*(x(2)-1)','x');  
x0=[0 0], TolX=1e-4; TolFun=1e-9; MaxIter=100;
[xon,fon]=opt_Nelder(f713,x0,TolX,TolFun,MaxIter)
%minimum point and its function value
[xos,fos]=fminsearch(f713,x0) %use the MATLAB built-in function
