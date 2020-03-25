%OU_process_splitting.m
clear all,clc
gam=[3:0.5:4.5,4.7,5]; %splitting levels
N=10^4;
for iter=1:10 % repeat 10 independent times to estimate RE
    x_ini=1;y_ini=1;
    data=repmat([x_ini,y_ini],N,1);
    for t=1:length(gam)
        %resample the paths
        data=data(ceil(rand(N,1)*size(data,1)),:);
        elite=[];
        for i=1:N
            [indicator,x,y]=ou_split(gam(t),data(i,1),data(i,2));
            if indicator
                elite=[elite;x(end),y(end)];% store the successful hits
            end
        end
        t
        c(t)=size(elite,1)/N; % conditional probability estimate
        data=elite;
    end
    ell(iter)=prod(c);
end
mean(ell)
std(ell)/sqrt(10)/mean(ell)

