%nm5p06b
warning off MATLAB:divideByZero
fp56b=inline('exp(-x.*x)','x'); 
fp56b1=inline('ones(size(x))','x'); 
fp56b2=inline('exp(-1./y./y)./y./y','y'); 
a=0; b=200; N=200; tol=1e-4; IT=sqrt(pi)/2;
a1=0; b1=1; a2=0; b2=1; MGH=2; 
e_s= smpsns(fp56b,a,b,N)-IT
e_as= adapt_smpsn(fp56b,a,b,tol)-IT
e_q= quad(fp56b,a,b,tol)-IT
e_GH= Gauss_Hermite(fp56b1,MGH)/2-IT
e_ss= smpsns(fp56b,a1,b1,N)+smpsns(fp56b2,a2,b2,N)-IT
Iasas= adapt_smpsn(fp56b,a1,b1,tol)+ ...
+????????????????????????????? -IT
e_qq=quad(fp56b,a1,b1,tol)+????????????????????????? -IT
warning off MATLAB:divideByZero
