clear;

load('../Data/AUDCAD_unequal_ret', 'ret');

moments={mean(ret), std(ret), skewness(ret), kurtosis(ret)};
[ret_sim, type]=pearsrnd(moments{:}, 100000, 1);

g=inline('sum(log(1+f*R))/length(R)', 'f', 'R');
% g=inline('prod(1+f*R)^(1/length(R))-1', 'f', 'R');

myf=0:23;
myg=NaN(24, 1);
for f=myf
    myg(f+1)=g(f, ret_sim);
end

plot(myf, myg);
    
minusG=@(f)-g(f, ret);
minusGsim=@(f)-g(f, ret_sim);

optimalF=fminbnd(minusGsim, 0, 20); % optimal leverage based on simulated returns
fprintf(1, 'Optimal leverage=%f optimal growth rate=%f\n', optimalF, -minusGsim(optimalF));

minR=min(ret_sim); % minimum return in simulated series
fprintf(1, 'minR=%f\n', minR);

maxDD=calculateMaxDD(cumprod(1+optimalF*ret_sim)-1); % max drawdown with optimal leverage
fprintf(1, 'f=%i maxDD with optimal leverage=%f\n', optimalF, maxDD);

maxDD=calculateMaxDD(cumprod(1+optimalF/2*ret_sim)-1);  % max drawdown with half of optimal leverage
fprintf(1, 'f=%i maxDD with half of optimal leverage=%f\n', optimalF/2, maxDD);

maxDD=calculateMaxDD(cumprod(1+optimalF/7*ret_sim)-1);  % max drawdown with 1/7 of optimal leverage
fprintf(1, 'f=%i maxDD with 1/7 of optimal leverage=%f\n', optimalF/7, maxDD);

maxDD=calculateMaxDD(cumprod(1+optimalF/1.4*ret)-1); % max drawdown with 1/1.4 of optimal leverage for historical returns
fprintf(1, 'f=%i maxDD for historical returns=%f\n', optimalF/1.4, maxDD);


D=0.5;
fprintf(1, 'Growth rate on simulated returns using D=%3.1f of optimal leverage on full account=%f\n', D, -minusGsim(optimalF*D));
fprintf(1, 'MaxDD on simulated returns using D of optimal leverage on full account=%f\n', calculateMaxDD(cumprod(1+optimalF*D*ret_sim)-1));

% CPPI 
g_cppi=0;
% g_debug=0;
drawdown=0;
for t=1:length(ret_sim)
    g_cppi=g_cppi+log(1+ret_sim(t)*D*optimalF*(1+drawdown));
    %     g_cppi=(1+g_cppi)*(1+r(t)*D*optimalF*(1+drawdown))-1;
    %     g_debug=g_debug+log(1+r(t)*D*optimalF)
    
    %     if (g_cppi/t >= 1)
    %         keyboard;
    %     end
    
    drawdown=min(0, (1+drawdown)*(1+ret_sim(t))-1);
end
g_cppi=g_cppi/length(ret_sim);
% g_cppi=(1+g_cppi)^(1/length(r))-1;

fprintf(1, 'Growth rate on simulated returns using CPPI=%f\n', g_cppi);

fprintf(1, 'Growth rate on historical returns using D of optimal leverage on full account=%f\n', -minusG(optimalF*D));
fprintf(1, 'MaxDD on historical returns using D of optimal leverage on full account=%f\n', calculateMaxDD(cumprod(1+optimalF*D*ret)-1));

% CPPI
g_cppi=0;
drawdown=0;
for t=1:length(ret)
    %     g_cppi=(1+g_cppi)*(1+ret(t)*D*optimalF*(1+drawdown))-1;
    g_cppi=g_cppi+log(1+r(t)*D*optimalF*(1+drawdown));

    drawdown=min(0, (1+drawdown)*(1+ret(t))-1);
end
g_cppi=g_cppi/length(ret);
% g_cppi=(1+g_cppi)^(1/length(ret))-1;

fprintf(1, 'Growth rate on historical returns using CPPI=%f\n', g_cppi);

