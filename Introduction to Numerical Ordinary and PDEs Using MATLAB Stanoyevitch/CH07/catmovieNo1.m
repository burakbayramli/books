%script for EFR 7.6(a):  catmovieNo1.m  cat movie creation
%Basic CAT movie, where cat closes and reopens its eyes.
clf, counter=1;

A=[0  0  .5  1  2  2.5  3  3  1.5  0; ...
     0  3  4   3  3   4   3  0  -1   0]; %Basic CAT matrix
Ah = mkhom(A); %use the M-file from EFR 7.6

t=0:.02:2*pi;  %creates time vector for parametric equations for eyes
xL=1+.4*cos(t); y=2+.4*sin(t); %creates circle for left eye 
LE=mkhom([xL; y]); %homogeneous coordinates for left eye
xR=2+.4*cos(t); y=2+.4*sin(t); %creates circle for right eye
RE=mkhom([xR; y]); %homogeneous coordinates for right eye
xL=1+.15*cos(t); y=2+.15*sin(t); %creates circle for left pupil
LP=mkhom([xL; y]); %homogeneous coordinates for left pupil
xR=2+.15*cos(t); y=2+.15*sin(t); %creates circle for right pupil 
RP=mkhom([xR; y]); %homogeneous coordinates for right pupil

for s=0:.2:2*pi
 factor = (cos(s)+1)/2;
 plot(A(1,:), A(2,:), 'k'), hold on
 axis([-2 5 -3 6]), axis('equal')
 LEtemp=vertscale(LE,factor,2); LPtemp=vertscale(LP,factor,2);
 REtemp=vertscale(RE,factor,2); RPtemp=vertscale(RP,factor,2);
 hold on
 fill(LEtemp(1,:), LEtemp(2,:),'y'), fill(REtemp(1,:), REtemp(2,:),'y')
 fill(LPtemp(1,:), LPtemp(2,:),'k'), fill(RPtemp(1,:), RPtemp(2,:),'k')
 M(:, counter) = getframe;
 hold off
 counter=counter+1;
end
