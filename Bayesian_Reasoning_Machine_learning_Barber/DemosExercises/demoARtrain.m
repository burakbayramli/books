function demoARtrain
T=100; t=1:T;
v = 0.01*(t-2).^2-0.2.*t; v = v + 7*randn(1,T); % noisy data
plot(t,v,'.');

L=3; % AR order
[a res]=ARtrain(v,L); % fit the AR model
% prediction:
vv(1:T)=v(1:T);TT=T+40;
for t=T+1:TT
    vhat = vv(t-L:t-1);
    vv(t) = vhat*a;
end
hold on; plot(T+1:TT,vv(T+1:TT),'-m')
s=sqrt(mean(res.^2));plot(T+1:TT,vv(T+1:TT)-s,':m');plot(T+1:TT,vv(T+1:TT)+s,':m')