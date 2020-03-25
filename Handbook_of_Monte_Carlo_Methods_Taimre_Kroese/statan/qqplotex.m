%qqplotex.m
nk = [100,1;
    100, 10;
    1000, 10;
    10000,100];
hold on
for count = 1:4;
    n= nk(count,1);
    k = nk(count,2);
    x = sum(-log(rand(k,n)),1);  %generate the data
    x = sort(x); %sort it
    i = 1:n;
    y = icdf('normal',i/(n+1),0,1); %compute the inverse of Phi
    subplot(2,2,count)
    plot(x,y,'.')
    hold on
    p = polyfit(x,y,1)   %find the regression parameters
    f = polyval(p,x);    %the values of the regression line
    plot(x,f)
end
hold off
