function []=plot_geometry_2D(x_p,y_p,x_v,y_v,L_x,L_y,n_gauss,L_el_x,L_el_y,x_gauss,y_gauss)

% Plot of geometry

n_el_x=length(x_p)-1;
n_el_y=length(y_p)-1;

% Screen dimensions
scrsz=get(0,'ScreenSize');
bar=64;

figure('Color',[1 1 1],'Position',[0 0 scrsz(3) (scrsz(4)-bar)])
axes('FontSize',14)

% Ficticious plots for legend
plot(x_v(1),y_v(end-1+1),'bo','LineWidth',3)
hold on
plot(x_p(1),y_p(end-1+1),'go','LineWidth',3,'MarkerSize',15)
plot(x_p(1)+L_el_x*(1-1)+x_gauss(1),y_p(1)+L_el_y*(1-1)+y_gauss(1),'ro','LineWidth',3)

for n=1:n_el_x+1
    plot([x_p(n),x_p(n)],[y_p(1),y_p(n_el_y+1)],'k','LineWidth',2)
end

for n=1:n_el_y+1
    plot([x_p(1),x_p(n_el_x+1)],[y_p(n),y_p(n)],'k','LineWidth',2)
end

for i=1:n_el_y
    for j=1:n_el_x
        text(x_p(1)+L_el_x/2+L_el_x*(j-1)+L_el_x/20,y_p(1)+L_el_y*(n_el_y-1/2)-L_el_y*(i-1)+L_el_y/20,num2str((i-1)*n_el_x+j),'Color','k','FontSize',14)
    end
end

n=1;
for i=1:length(y_v)
    for j=1:length(x_v)
        plot(x_v(j),y_v(end-i+1),'bo','LineWidth',3)
        text(x_v(j)+L_el_x/10,y_v(end-i+1)-L_el_y/10,num2str(n),'Color','b')
        n=n+1;
    end
end

n=1;
for i=1:length(y_p)
    for j=1:length(x_p)
        plot(x_p(j),y_p(end-i+1),'go','LineWidth',3,'MarkerSize',15)
        text(x_p(j)+L_el_x/10,y_p(end-i+1)+L_el_y/10,num2str(n),'Color','g')
        n=n+1;
    end
end

for i=1:n_el_y
    for j=1:n_el_x
        for n=1:n_gauss
            plot(x_p(1)+L_el_x*(j-1)+x_gauss(n),y_p(1)+L_el_y*(i-1)+y_gauss(n),'ro','LineWidth',3)
        end
    end
end

hold off
title('Geometry','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
legend('Velocity','Pressure','Gauss points','Location','SouthEast')
grid off
xlim([x_p(1)-L_x/10,x_p(end)+L_x/10])
ylim([y_p(1)-L_y/10,y_p(end)+L_y/10])

end