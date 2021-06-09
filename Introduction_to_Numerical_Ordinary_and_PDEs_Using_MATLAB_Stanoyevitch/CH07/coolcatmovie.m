%coolcatmovie.m: script for making coolcat movie matrix M of EFR 7.7

%act one:  eyes shifting left/right
t=0:.02:2*pi; counter=1;
A=[0  0  .5  1  2  2.5  3  3  1.5  0; ...
     0  3  4   3  3   4   3  0  -1   0];
x=1+.4*cos(t); y=2+.4*sin(t);xp=1+.15*cos(t); yp=2+.15*sin(t);
LE=[x;y]; LEh=mkhom(LE); LP=[xp;yp]; LPh=mkhom(LP);
REh=reflx(LEh, 1.5); RPh=reflx(LPh, 1.5);
LW=[.3 -1; .2 -.8]; LW2=[.25 -1.1;.25 -.6]; %left whiskers
LWh=mkhom(LW); LW2h=mkhom(LW2); 
RWh=reflx(LWh, 1.5); RW2h=reflx(LW2h, 1.5); %reflect left whiskers
                                             %to get right ones
M=[1 1.5 2;.25 -.25 .25]; Mh=mkhom(M);  %matrix & homogenization of
                                         %cats mouth
Mhrefl=refly(Mh,-.25); %homogeneous coordinates for frown 
for n=0:(2*pi)/20:2*pi
plot(A(1,:), A(2,:),'k')
axis([-2 5 -3 6]), axis('equal')
hold on
plot(LW(1,:), LW(2,:),'k'), plot(LW2(1,:), LW2(2,:),'k')
plot(RWh(1,:), RWh(2,:),'k')
plot(RW2h(1,:), RW2h(2,:),'k')
plot(Mhrefl(1,:), Mhrefl(2,:),'k')
fill(LE(1,:), LE(2,:),'y'), fill(REh(1,:), REh(2,:),'y')
LPshft=shift(LPh,-.25*sin(n),0); RPshft=shift(RPh,-.25*sin(n),0);
fill(LPshft(1,:), LPshft(2,:),'k'), fill(RPshft(1,:), RPshft(2,:),'k')
Mov(:, counter)=getframe;
hold off
counter = counter +1;
end

%act two:  eyes shifting up/down
for n=0:(2*pi)/20:2*pi
plot(A(1,:), A(2,:),'k')
axis([-2 5 -3 6]), axis('equal')
hold on
plot(LW(1,:), LW(2,:),'k'), plot(LW2(1,:), LW2(2,:),'k')
plot(RWh(1,:), RWh(2,:),'k')
plot(RW2h(1,:), RW2h(2,:),'k')
plot(Mhrefl(1,:), Mhrefl(2,:),'k')
fill(LE(1,:), LE(2,:),'y'), fill(REh(1,:), REh(2,:),'y')
LPshft=shift(LPh,0,.25*sin(n)); RPshft=shift(RPh,0,.25*sin(n));
fill(LPshft(1,:), LPshft(2,:),'k'), fill(RPshft(1,:), RPshft(2,:),'k')
Mov(:, counter)=getframe;
hold off
counter = counter +1;
end

%act three:  whisker rotating up/down then smiling
for n=0:(2*pi)/10:2*pi
plot(A(1,:), A(2,:),'k')
axis([-2 5 -3 6]), axis('equal')
hold on
fill(LE(1,:), LE(2,:),'y'),fill(LP(1,:), LP(2,:),'k')
fill(REh(1,:), REh(2,:),'y'),fill(RPh(1,:), RPh(2,:),'k')
LWrot=rot(LWh,.3,.2,-pi/6*sin(n)); LW2rot=rot(LW2h, .25,.25,-pi/6*sin(n));
RWrot=reflx(LWrot, 1.5); RW2rot=reflx(LW2rot, 1.5);
plot(LWrot(1,:), LWrot(2,:),'k'), plot(LW2rot(1,:), LW2rot(2,:),'k')
plot(RWrot(1,:), RWrot(2,:),'k'),plot(RW2rot(1,:), RW2rot(2,:),'k')
if n == 2*pi
   plot(Mh(1,:), Mh(2,:),'k')
   for n=1:10, L(:,n)=getframe; end
   Mov(:, counter:(counter+9))=L;
   break
else
   plot(Mhrefl(1,:), Mhrefl(2,:),'k')

end
Mov(:, counter)=getframe;

hold off
counter = counter +1;
end

%THE END
