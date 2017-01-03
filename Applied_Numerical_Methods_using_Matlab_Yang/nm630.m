%nm630: Heun/Euer/RK4 method to solve a 1st-order differential equation
clear, clf
a=1; r=1; tspan=[0 2]; y0=0;  
t=tspan(1)+[0:100]*(tspan(2)-tspan(1))/100;
yt=1-exp(-a*t);
plot(t,yt,'k'), hold on
df61=inline('-y+1','t','y');
N=4;
[t1,ye]=ode_Euler(df61,tspan,y0,N);
[t1,yh]=ode_Heun(df61,tspan,y0,N);
[t1,yr]=ode_RK4(df61,tspan,y0,N);
[to,yo]=ode23(df61,tspan,y0);
plot(t,yt,'k', t1,ye,'b:', t1,yh,'b:', t1,yr,'r:', to,yo,'m:')
plot(t1,ye,'bo', t1,yh,'b+', t1,yr,'r*')
N=100;
tic, [te,ye]=ode_Euler(df61,tspan,y0,6*N); time(1)=toc; 
tic, [th,yh]=ode_Heun(df61,tspan,y0,3*N); time(2)=toc; 
tic, [tr,yr]=ode_RK4(df61,tspan,y0,N); time(3)=toc; 
options=odeset('RelTol',10^-8, 'AbsTol',10^-9);
tic, [to23,yo23]=ode23(df61,tspan,y0,options); time(4)=toc;
tic, [to45,yo45]=ode45(df61,tspan,y0,options); time(5)=toc;
yte=1-exp(-a*te); yth=1-exp(-a*th); ytr=1-exp(-a*tr); 
yto23=1-exp(-a*to23); yto45=1-exp(-a*to45); 
disp('Euler        Heun          RK4           ode23         ode45')
format short e
err=[norm(ye-yte)/length(te) norm(yh-yth)/length(th) norm(yr-ytr)/length(tr)...
      norm(yo23-yto23)/length(to23) norm(yo45-yto45)/length(to45)]
format short
time
Number_of_grid=[length(te) length(th) length(tr) length(to23) length(to45)]
%fprintf('\nerror of Runge-Kutta=%10.4f\n',norm(yr-ytr)/length(tr))
%fprintf('\nerror of ode23=%10.4f\n',norm(yo23-yto23)/length(to23))
%fprintf('\nerror of ode45=%10.4f\n',norm(yo45-yto45)/length(to45))
tic, for k=1:100*N, a=k*2; end, t_multiplication=toc 
tic, for k=1:100*N, df61(0,1); end, t_ftncall=toc
