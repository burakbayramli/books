% Drift and trend

%drift
y1=0; y1_old=0;
ry1=zeros(500,1); 
for nn=1:500,   
   ry1(nn)=y1;
   y1=0.5+y1_old;
   y1_old=y1;
end;

%trend
y2=0; y2_old=0; t=0;
ry2=zeros(500,1); 
for nn=1:500,   
   ry2(nn)=y2;
   y2=(0.5*t)+y2_old;
   y2_old=y2;
   t=t+0.02;
end;

figure(1)
subplot(2,1,1)
plot(ry1,'k');
xlabel('n');
ylabel('drift');

subplot(2,1,2)
plot(ry2,'k');
xlabel('n');
ylabel('trend');


