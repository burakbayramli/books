%nm4e03 - astrophysics
clear, clf
global G Ms Me R T
G=6.67/1e11;
Ms=1.98*1e30; Me=5.98*1e24;
R=1.49*1e11;
T=3.15576*1e7;  w=2*pi/T;
x0=1e6 %initial guess
format short e
disp('(a)')
rn=newtons('phys',x0,1e-4,100)
rn1=newtons1('phys',x0,1e-4,100)
%rsc=secant('phys',x0,1e-4,100)
%rsc1=secant1('phys',x0,1e-4,100)
rfs=fsolve('phys',x0,optimset('fsolve'))
rfs1=fsolve('phys',x0,optimset('MaxFunEvals',1000)) %with more iterations
x01=1e10 %with another starting guess closer to the solution
rfs2=fsolve('phys',x01,optimset('MaxFunEvals',1000)) 
residual_errs=phys([rn rfs rfs1 rfs2]);
fprintf('\n %14.4e %14.4e %14.4e %14.4e\n', residual_errs)
disp('(b)')
rnb=newtons1('physb',x0,1e-4,100)
%rscb=secant1('phys1',x0,1e-4,100)
rfsb=fsolve('physb',x0,optimset('fsolve'))
residual_errs=phys([rn rnb rfsb]);
fprintf('\n %14.4e %14.4e %14.4e\n', residual_errs)
disp('(c)')
scale=1e11;
rns=newtons('phys',x0/scale,1e-6,100,scale)*scale
rnsb=newtons('physb',x0/scale,1e-6,100,scale)*scale
%rscs=secant('phys',x0/scale,1e-4,100,scale)*scale;
%rscsb=secant('physb',x0/scale,1e-4,100,scale)*scale;
%options=optimset('TolX',1e-4,'TolFun',1e-10,'MaxIter',100);
%rss=fsolve('phys',x0/scale,options,scale)*scale;
rfss=fsolve('phys',x0/scale,optimset('fsolve'),scale)*scale
rfssb=fsolve('physb',x0/scale,optimset('fsolve'),scale)*scale
residual_errs=phys([rns rnsb rfss rfssb]);
fprintf('\n %14.4e %14.4e %14.4e %14.4e\n', residual_errs)
disp('(d)')
A=[w^2 -2*R*w^2 w^2*R^2 G*(Me-Ms) 2*G*Ms*R -G*Ms*R^2];
rr=roots(A)
residual_errs=phys([rns rnsb rfss rfssb rr(5)]);
fprintf('\n %14.4e %14.4e %14.4e %14.4e %14.4e\n', residual_errs)
A1=A.*scale.^[5 4 3 2 1 0];
rrs=roots(A1)*scale; 
r=[rns rnsb rfss rfssb rrs(5)];
R-r;
format short