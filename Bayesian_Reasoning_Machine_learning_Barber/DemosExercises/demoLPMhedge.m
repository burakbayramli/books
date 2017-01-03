function demoLPMhedge
%DEMOLPMHEDGE: Linear Parameter Regression : hedge fund factors
figure
x(:,1)=randn(5,1);
train=1:20; test=21:25;
for t=2:25
    x(:,t)=x(:,t-1)+0.1*randn(5,1);
end
w0=randn(5,1);
y=w0'*x;
for i=1:5
    subplot(6,1,i); plot(train,x(i,train),'-o'); hold on
    plot(test,x(i,test),'-ro');
end
subplot(6,1,6); plot(train,y(train),'-x')
hold on
w=LPMregtrain(x(:,train),y(train),'LPMlinear',10e-2);
ytest = w'*LPMlinear(x(:,test)); 
plot(test,ytest,'-rx')