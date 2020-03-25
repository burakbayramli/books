%QMC_run_script.m
clear all,clc,trading_days=2:350;err=[];
for n=trading_days
    err=[err;asian_option_QMC(n)];
    n
end
plot(trading_days,err(:,1)), hold on
plot(trading_days,err(:,2),'r')