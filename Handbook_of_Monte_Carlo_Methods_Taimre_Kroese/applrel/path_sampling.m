function Y=path_sampling(p_bar)
%samples the repair times that belong to the paths
k=length(p_bar);
% step 2
P=cumprod(p_bar);
p_star=(1-p_bar).*[1,P(1:end-1)];
p_star=p_star/(1-P(end));
% step 3
[dummy,idx]=histc(rand,[0,cumsum(p_star)]);
% step 4
lam=-log(1-p_bar); %exponential rates
for i=1:idx-1
    Y(i)=expt(lam(i),0,1);
end
% step 5
Y(idx)=1-log(rand)/lam(idx);
% step 6
for i=idx+1:k
    Y(i)= -log(rand)/lam(i);
end

