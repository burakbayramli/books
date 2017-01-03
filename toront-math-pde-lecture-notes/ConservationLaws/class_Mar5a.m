
t_0 = 0; t_f = 1; N = 100; M = 100;
% here I've got k = h
% I take initial data which is -1 on the left and 1 on the right so 
% I want to set the BCs to 0.
BCs = 0;
[u1,x,t] = conservative_scheme(@shock1,@F_eu,t_0,t_f,M,N,BCs);
[u2,x,t] = conservative_scheme(@shock2,@F_eu,t_0,t_f,M,N,BCs);

figure(1)
clf
plot(x,u1(:,1),'o','LineWidth',2);
figure(1)

figure(2)
clf
plot(x,u2(:,1),'x','LineWidth',2);
figure(2)

figure(1)
plot(x,u1(:,1),'o-'),
figure(1)
pause(1)
for j=2:M+1
    plot(x,u1(:,j),'LineWidth',2);
    figure(1)
end

figure(2)
plot(x,u2(:,1),'o-');
figure(2)
pause(1)
% for j=1:M/2-5
for j=2:M+1
    plot(x,u2(:,j),'LineWidth',2);
    figure(2)
end

