% BILD027, Programm zum Plotten des Stabilit"atsbereiches
% von Einschrittverfahren
clf
m = 400;
z = [0  0; 0  5];
plot(z(:,1),z(:,2),'LineWidth',2);
hold on
axis equal
% RKV explizit, r = 1
p = [1 1];
q = [0 1];
v = bld020405a(p,q,m);
plot(v(:,1),v(:,2),'k','LineWidth',2);
hold on

% RKV explizit, r = 2
p = [1/2 1  1];
q = [0   0  1];
v = bld020405a(p,q,m);
plot(v(:,1),v(:,2),'k','LineWidth',2);
hold on

% RKV explizit, r = 3
p = [1/6  1/2 1  1];
q = [0    0   0  1];
v = bld020405a(p,q,m);
[r,s] = size(v) ;
for k = 1:r
   plot(v(k,1),v(k,2),'k.','markersize',4);
   hold on
end

% RKV explizit, r = 4
p = [1/24 1/6 1/2  1  1];
q = [0    0   0    0  1];
v = bld020405a(p,q,m);
[r,s] = size(v) ;
for k = 1:r
   plot(v(k,1),v(k,2),'k.','Markersize',4);
   hold on
end

% RKV explizit, r = 5
p = [1/120 1/24 1/6 1/2  1  1];
q = [0     0    0   0    0  1];
v = bld020405a(p,q,m);
[r,s] = size(v) ;
for k = 1:r
   plot(v(k,1),v(k,2),'k.','Markersize',4);
   hold on
end
% RKV explizit, r = 6
p = [1/600 1/120 1/24 1/6 1/2  1  1];
q = [0     0     0    0   0    0  1];
v = bld020405a(p,q,m);
[r,s] = size(v) ;
for k = 1:r
   plot(v(k,1),v(k,2),'k.','Markersize',4);
   hold on
end
%title(' Stab.-bereiche fuer explizite RKV, p=r = 1-6','fontsize',18)
text(-1.5,0.65,'r = 1','fontsize',24)
text(-1.5,1.4,'r = 2','fontsize',24)
text(-1.2,2,'r = 3','fontsize',24)
text(0.4,2.1,'r = 4','fontsize',24)
text(0.7,3,'r = 5','fontsize',24)
text(-3,2.9,'r = 6','fontsize',24)
text(0.7,4.1,'r = 6','fontsize',24)
axis equal
%grid on
