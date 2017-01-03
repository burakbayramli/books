function demoSoftLoss
%DEMOSOFTLOSS demo of classification using soft loss
figure
x1=0.5*randn(2,20)+repmat([-1.25 -1.25]',1,20);
x1=[x1 6*(0.5*randn(2,2)+repmat([1.25 1.25]',1,2))];
x0=0.5*randn(2,20)+repmat([1.25 1.25]',1,20);

opts.maxit=200;
opts.tol=0.001;
opts.eta=0.01;
opts.plotprogress=1;
[w0 b0 p10 p11]=LogReg(x1,x0,opts); % train using Gradient Ascent

% SoftLoss training:
figure
x=horzcat(x1,x0);
c=ones(1,size(x,2));
c(size(x1,2)+1:end)=0;
w=zeros(size(x,1),1); b=0; lambda=0.01;
beta=10.1;
for loop=1:20
    for i=randperm(length(w))
        ws=[-50:0.1:50];
        wtry=w;
        for j=1:length(ws);
            wtry(i)=ws(j);
            E(j)=softloss(x,c,wtry,b,lambda,beta);
        end
        w(i)=ws(argmin(E));
    end
    bs=[-50:0.1:50];
    for j=1:length(bs);
        btry=bs(j);
        Eb(j)=softloss(x,c,w,btry,lambda,beta);
    end
    b=bs(argmin(Eb));
    s(loop)=softloss(x,c,w,b,lambda,beta);
    plot(s); title('soft loss'); drawnow
end
figure
hold on
plot(x1(1,:),x1(2,:),'o');
plot(x0(1,:),x0(2,:),'o','MarkerFaceColor','k');
xx1=-10:10:10; xx2 = -(b+w(1)*xx1)./w(2); plot(xx1,xx2);
xx1=-10:10:10; xx2 = -(b0+w0(1)*xx1)./w0(2); plot(xx1,xx2,'r:');
axis([min(x(1,:)) max(x(1,:)) min(x(2,:)) max(x(2,:))]);
set(gca,'box','on')
legend('class 1','class 0','soft loss decision boundary','log reg decision boundary')