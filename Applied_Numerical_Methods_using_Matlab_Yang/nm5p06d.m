%nm5p06d
fp56a=inline('sin(x)./x','x'); fp56a2=inline('sin(1./y)./y','y'); 
syms x
IT2=pi/2-double(int(sin(x)/x,0,1)) %true value of the integral
disp('Change of upper limit of the integration interval')
a=1; b=[100 1e3 1e4 1e7]; tol=1e-4;
for i=1:length(b)
  Iq2=quad(fp56a,a,b(i),tol);
  fprintf('With b=%12.4e, err_Iq=%12.4e\n', b(i),Iq2-IT2);
end
disp('Change of lower limit of the integration interval')
a2=[1e-3 1e-4 1e-5 1e-6 0]; b2=1; tol=1e-4;
for i=1:5 
  Iq2=quad(fp56a2,a2(i),b2,tol);
  fprintf('With a2=%12.4e, err_Iq=%12.4e\n', a2(i),Iq2-IT2);
end
