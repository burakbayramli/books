function demoRobotArm
%DEMOROBOTARM demo of controlling a robot arm
figure

T=200;
n=4; % number of links in arm

l=(1:T)/T; % for colour plotting

% generate a sample trajectory we wish to follow:
trueh=0.5*randn(n,1);
vh=0.1*randn(n,1);
for t=2:T
    trueh(:,t)=trueh(:,t-1)+vh;
    vh=vh+0.005*randn(n,1);
end
x=sum(cos(trueh)); y=sum(sin(trueh));

avh=0.005*randn(n,T); avcosh=cos(avh); avsinh=sin(avh); % variational  approx initialisation


ssh=0.0100*ones(1,T);
ssx=0.0001*ones(1,T); ssy=ssx;
mh=pi*ones(n,1); % intial mean of angle
ssh(1)=10000.000000000100; % initial angle variance

del=0.01; % integration discretisation

figure(1); cla; hold on
for t=1:T
    plot(x(t),y(t),'o','color',[l(t),1-l(t),0]);
    if t>1; line([x(t-1) x(t)],[y(t-1) y(t)],'color',[l(t),1-l(t),0]);  end
end
axis equal; set(gca,'box','on'); title('desired end arm trajectory'); drawnow; a=axis;


for loop=1:500
    for i=randperm(n)
        for t=randperm(T)
            tmpx = -sum(avcosh([1:i-1 i+1:n],t))+x(t);
            tmpy = -sum(avsinh([1:i-1 i+1:n],t))+y(t);
            h=avh(i)-pi:del:avh(i)+pi;
            if t>1 && t<T
                logp = -0.5*(h-avh(i,t-1)).^2/ssh(t)-0.5*(h-avh(i,t+1)).^2/ssh(t+1) ...
                    -0.5*(cos(h)-tmpx).^2/ssx(t)-0.5*(sin(h)-tmpy).^2/ssy(t);
            elseif t==T
                logp = -0.5*(h-avh(i,t-1)).^2/ssh(t)-0.5*(cos(h)-tmpx).^2/ssx(t)-0.5*(sin(h)-tmpy).^2/ssy(t);
            else
                logp = -0.5*(h-mh(i)).^2/ssh(1)-0.5*(h-avh(i,2)).^2/ssh(2)...
                    -0.5*(cos(h)-tmpx).^2/ssx(t)-0.5*(sin(h)-tmpy).^2/ssy(t);
            end
            p=condexp(logp')';
            avh(i,t)=sum(h.*p);
            avcosh(i,t)=sum(cos(h).*p);
            avsinh(i,t)=sum(sin(h).*p);
        end
    end
    xx=sum(cos(avh));  yy=sum(sin(avh));
    
    figure(2); cla; hold on
    for t=1:T
        plot(xx(t),yy(t),'+','color',[l(t),1-l(t),0]);
        if t>1; line([xx(t-1) xx(t)],[yy(t-1) yy(t)],'color',[l(t),1-l(t),0]); end
    end
    axis equal;set(gca,'box','on');title('learned end arm trajectory'); drawnow
    figure(3); nplots=50;
    ts=ceil(linspace(1,T,nplots));
    
    for tt=1:length(ts)
        subplot(5,nplots/5,tt);cla; hold on
        ttt=ts(tt); c=tt/length(ts);
        xxx=cumsum(cos(avh(:,ttt)));  yyy=cumsum(sin(avh(:,ttt)));
        for i=1:n
            plot(0,0,'k.');
            if i<n
                plot(xxx(i),yyy(i),'k.');
            else
                plot(xxx(i),yyy(i),'*','color',[c,1-c,0]);
            end
            if i>1; line([xxx(i-1) xxx(i)],[yyy(i-1) yyy(i)])
            else
                line([0 xxx(i)],[0 yyy(i)])
            end
        end
        axis(1.5*a); set(gca,'xticklabel',''); set(gca,'yticklabel',''); set(gca,'box','on');
    end
    drawnow % these are the learned arm angle positions through time
end