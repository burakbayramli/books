

t_0 = 0;
t_f = 1;
M = 100;
N = 100;


[u,u_true,x,t] = explicit_upwind(@f,t_0,t_f,M,N);
for j=0:20
    plot(x,u_true(:,1+5*j),'r','LineWidth',2);
    hold on
    plot(x,u(:,1+5*j),'LineWidth',2);
    axis([0,2*pi,-.3,1.3]);
    title('Explicit Upwind','FontSize',16)
    xlabel('true solution is in red','FontSize',16)
    hold off
    figure(1)
    pause(1)
end

pause

[u,u_true,x,t] = centered_difference(@f,t_0,t_f,M,N);
for j=0:20
    plot(x,u_true(:,1+5*j),'r','LineWidth',2);
    hold on
    plot(x,u(:,1+5*j),'LineWidth',2);
    axis([0,2*pi,-.3,1.3]);
    title('Centered Difference','FontSize',16)
    xlabel('true solution is in red','FontSize',16)
    hold off
    figure(1)
    pause(1)
end

pause

[u,u_true,x,t] = lax_friedrichs(@f,t_0,t_f,M,N);
for j=0:20
    plot(x,u_true(:,1+5*j),'r','LineWidth',2);
    hold on
    plot(x,u(:,1+5*j),'LineWidth',2);
    axis([0,2*pi,-.3,1.3]);
    title('Lax-Friedrichs','FontSize',16)
    xlabel('true solution is in red','FontSize',16)
    hold off
    figure(1)
    pause(1)
end

pause


[u,u_true,x,t] = lax_wendroff(@f,t_0,t_f,M,N);
for j=0:20
    plot(x,u_true(:,1+5*j),'r','LineWidth',2);
    hold on
    plot(x,u(:,1+5*j),'LineWidth',2);
    axis([0,2*pi,-.3,1.3]);
    title('Lax-Wendroff','FontSize',16)
    xlabel('true solution is in red','FontSize',16)
    hold off
    figure(1)
    pause(1)
end

pause




[u,u_true,x,t] = beam_warming(@f,t_0,t_f,M,N);
for j=1:21
    plot(x,u_true(:,j),'r','LineWidth',2);
    hold on
    plot(x,u(:,j),'LineWidth',2);
    axis([0,2*pi,-.3,1.3]);
    title('Beam-Warming','FontSize',16)
    xlabel('true solution is in red','FontSize',16)
    hold off
    figure(1)
    pause(1)
end
