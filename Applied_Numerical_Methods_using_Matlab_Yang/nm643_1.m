%nm643_1: RK4/Milne/Adams/Hamming method to solve a differential eq
clear, clf
a=1;  r=1;   t0=0; tf=10.; N=50;
Kf= 1;
if Kf==1,  y0=0; f643=inline('1-exp(-t)','t'); df643=inline('-y+1','t','y');
 elseif Kf==2, y0=1; f643=inline('exp(-t)','t');  df643=inline('-y','t','y');
 elseif Kf==3, y0=0; f643=inline('exp(t)-1','t'); df643=inline('y+1','t','y');
 else y0=1; f643=inline('t-2+3*exp(-t/2)','t'); df643=inline('(t-y)/2','t','y');
end
for KC=0:1 %with modifier inactivated or activated
  tic, [t1,yR]= ode_RK4(df643,[t0 tf],y0,N); time(1)=toc;
  %tic, [t1,yM]= ode_Milne(df643,[t0 tf],y0,N,KC); t=toc
  tic, [t1,yA]= ode_ABM(df643,[t0 tf],y0,N,KC); time(2+2*KC)=toc;
  tic, [t1,yH]= ode_Ham(df643,[t0 tf],y0,N,KC); time(3+2*KC)=toc;
  tic, [t2,y45]= ode45(df643,[t0 tf],y0); time(6)=toc;
  yt1= f643(t1);   yt2= f643(t2);
  subplot(521+KC*2)
  %plot(t1,yt1,'k', t1,yR,'r:', t1,yM,'m:', t1,yA,'b:', t1,yH,'g:')
  plot(t1,yt1,'k', t1,yR,'k', t1,yA,'k--', t1,yH,'k:')
  subplot(522+KC*2)
  tmp= abs(yt1)+eps; l_t1= length(t1); %tmp=1;
  eR=abs(yR-yt1)./tmp;  %eM=abs(yM-yt1)./tmp;
  eA=abs(yA-yt1)./tmp;  eH=abs(yH-yt1)./tmp;
  tmp2= abs(yt2)+eps; l_t2= length(t2);
  e45=abs(y45-yt2)./tmp2;
  err(1)=norm(eR)/l_t2;
  err(2+2*KC)=norm(eA)/l_t1;
  err(3+2*KC)=norm(eH)/l_t1;
  err(6)=norm(e45)/l_t2;
  %plot(t1,eR,'r', t1,eM,'m', t1,eA,'b', t1, eH,'g')
  plot(t1,eR,'r', t1,eA,'b', t1, eH,'g')
  pause, plot(t1,eR,'k', t1,eA,'k--', t1, eH,'k:')
  hold on, plot(t2,e45,'m')
  %plot(t1,eR,'k', t1,eM,'k', t1,eA,'k', t1, eH,'k')
  grid on
end
disp('     RK4     ABM      Hamming ABM_modifier Ham_modifier')
err
time