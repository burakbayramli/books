%nm5p06a
warning off MATLAB:divideByZero
fp56a=inline('sin(x)./x','x'); fp56a2=inline('sin(1./y)./y','y'); 
IT=pi/2; % True value of the integral 
a=0; b=100; N=200; tol=1e-4; MGL=20; a1=0; b1=1; a2=0.001; b2=1;
format short e
e_s=smpsns(fp56a,a,b,N)-IT
e_as=adapt_smpsn(fp56a,a,b,tol)-IT
e_ql=quadl(fp56a,a,b,tol)-IT
e_GL=Gauss_Legendre(fp56a,a,b,MGL)-IT
e_ss=smpsns(fp56a,a1,b1,N)+smpsns(fp56a2,a2,b2,N)-IT
e_Iasas=adapt_smpsn(fp56a,a1,b1,tol)+ ...
???????????????????????????? -IT
e_Iqq=quad(fp56a,a1,b1,tol)+??????????????????????????? -IT
warning on MATLAB:divideByZero
