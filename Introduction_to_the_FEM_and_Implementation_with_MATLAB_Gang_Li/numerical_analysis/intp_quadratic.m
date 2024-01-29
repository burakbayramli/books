dp(:,1)=[0.5 2 4]';             % x-coordinates of input data points
dp(:,2)=[2 1 4]';               % data valaue of input data points
x_vector=[0:0.05:5]';           % output points  
u=CompLagIntp1D(dp, x_vector);  % compute the interpolation function
                                % at the output points
figure (1);                     % launch figure 1
plot(x_vector,u,'k-','LineWidth',2);  % plot the interpolation function
                                      % at the output points
hold on;                              % hold on
plot(dp(:,1),dp(:,2),'ko','LineWidth',5); % plot the input data points
set(gca,'fontsize',16);                   % set font size of the figure
xlabel('x','fontsize',18);                % label x-axis  
ylabel('u','fontsize',18);                % label y-axis