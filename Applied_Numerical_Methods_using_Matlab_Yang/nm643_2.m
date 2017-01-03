%nm643_2: ode23()/ode45()/ode113() to solve a differential eq
clear
a=1;  r=1;  t0=0;  tf=10.;  N=50;
Kf= 3;
if Kf==1,  y0=0; f643=inline('1-exp(-t)','t'); df643=inline('-y+1','t','y');
 elseif Kf==2, y0=1; f643=inline('exp(-t)','t');  df643=inline('-y','t','y');
 elseif Kf==3, y0=0; f643=inline('exp(t)-1','t'); df643=inline('y+1','t','y');
 else y0=1; f643=inline('t-2+3*exp(-t/2)','t'); df643=inline('(t-y)/2','t','y');
end
figure(Kf), clf
KC=1; %With ABM & Hamming having modifier
tic, [t1,yR]= ode_RK4(df643,[t0 tf],y0,N); time(1)=toc;
%tic, [t1,yM]= ode_Milne(df643,[t0 tf],y0,N); t_Milne=toc
tic, [t1,yA]= ode_ABM(df643,[t0 tf],y0,N,0); time(2)=toc;
tic, [t1,yAm]= ode_ABM(df643,[t0 tf],y0,N); time(3)=toc;
tic, [t1,yH]= ode_Ham(df643,[t0 tf],y0,N,0); time(4)=toc;
tic, [t1,yHm]= ode_Ham(df643,[t0 tf],y0,N); time(5)=toc;
yt1= f643(t1);
figure(1)
subplot(525)
%plot(t1,yt1,'k', t1,yR,'r:', t1,yM,'m:', t1,yA,'b:', t1,yH,'g:')
plot(t1,yt1,'k', t1,yR,'r:', t1,yA,'b:', t1,yH,'m:')
tmp= abs(yt1)+eps; l_t1= length(t1);
eR=abs(yR-yt1)./tmp; err(1)=norm(eR)/l_t1;
%eM=abs(yM-yt1)./tmp; e_Milnes=norm(eM)/length(t1)
eA=abs(yA-yt1)./tmp; err(2)=norm(eA)/l_t1;
eAm=abs(yAm-yt1)./tmp; err(3)=norm(eAm)/l_t1;
eH=abs(yH-yt1)./tmp; err(4)=norm(eH)/l_t1;
eHm=abs(yHm-yt1)./tmp; err(5)=norm(eHm)/l_t1;
subplot(526)
%plot(t1,eR,'r', t1,eM,'m', t1,eA,'b', t1,eH,'m')
plot(t1,eR,'r', t1,eA,'b', t1,eH,'m')
grid on, plot(t1,eR,'k', t1,eA,'k--', t1,eH,'k:')
subplot(527)
plot(t1,yt1,'k', t1,yR,'r', t1,yAm,'b:', t1,yHm,'m:')
subplot(528)
plot(t1,eR,'r', t1,eA,'b', t1,eH,'g')
grid on, plot(t1,eR,'k', t1,eAm,'k--', t1,eHm,'k:')

options=odeset('RelTol',1e-4); %0.00055);
tic, [t23,yode23]= ode23(df643,[t0 tf],y0,options); time(6)=toc;
%options=odeset('AbsTol',0.00025);
tic, [t45,yode45]= ode45(df643,[t0 tf],y0,options); time(7)=toc;
%options=odeset('RelTol',0.0000002); %0.00055); %);
%options=odeset('RelTol',0.00055); %);
tic, [t113,yode113]= ode113(df643,[t0 tf],y0,options); time(8)=toc;
yt23= f643(t23); tmp= abs(yt23)+eps;
eode23=abs(yode23-yt23)./tmp;
err(6)=norm(eode23)/length(t23);
yt45= f643(t45); tmp= abs(yt45)+eps;
eode45=abs(yode45-yt45)./tmp;
err(7)=norm(eode45)/length(t45);
yt113= f643(t113); tmp= abs(yt113)+eps;
eode113=abs(yode113-yt113)./tmp;
err(8)=norm(eode113)/length(t113);
subplot(529)
plot(t23,yode23,'k', t45,yode45,'b', t113,yode113,'r')
subplot(5,2,10)
plot(t23,eode23,'k', t45,eode45,'b--', t113,eode113,'r:')
grid on
disp('      RK4         ABM         ABM-m        Hamming     Hamming-m      ode23       ode45      ode113')
format short e, err, format short
time
Number_of_grid=[length(t1) length(t1) length(t1) length(t1) length(t1) length(t23) length(t45) length(t113)]
%t0=0; tf=2;
% a set of differential eq.s, i.e., state equation
%[t1,yR]=ode_RK4('df611',[t0 tf],[0 0],N)
%[t1,yM]=ode_Milne('df611',[t0 tf],[0 0],N,KC)
%[t1,yA]=ode_ABM('df611',[t0 tf],[0 0],N,KC)
%[t1,yH]=ode_Ham('df611',[t0 tf],[0 0],N,KC)