% function []=ucsd_garch_demo();
clear all;
%The function give a quick demonstration fo some of the features of the UCSD_GARCH toolbox.  Enjoy.
%
%
% Written by: Kevin Sheppard      18-March-2001
% Included in the ucsd_garch toolbox and the JPL library
% Requires the JPL toolbox and the optimization toolbox
%
% 
clc;
fprintf(1,'Beginning the UCSD_GARCH demo program.  This program is designed to\n') 
fprintf(1,'help you get a basic feel for the programs included, but does not show all\n')
fprintf(1,'functionality.  Please press a key to continue.\n\n')

pause;
fprintf(1,'We will begin by simulatig a simple univariate GARCH process with normal innovations.\n')
fprintf(1,'The command to do this is:\n\n')
fprintf(1,'[x,h]=garchsimulate([.01; .2; .75],1,1,1000)\n\n')
fprintf(1,'where x is the output data series, h is the output conditional variances, the vector [.01; .04; .94]\n') 
fprintf(1,'represents the constant, arch and garch parameters, the two 1s represent the arch and garch orders.\n')
fprintf(1,'and 500 is the amount of output desired.\n\n')
[x,h]=garchsimulate(1000,[.01 .2 .75]',1,1);
fprintf(1,'We shoudl test out data for the presense of GARCH.  This can be done by calling lmtest2\n')
fprintf(1,'[statistic, pval] = lmtest2(x,5)\n')
fprintf(1,'where statistic is the estimated statistic and pval are the associated probabilities the squares of the residuals are independent.\n')
[results] = lmtest2(x,5);
info.cnames=strvcat('Statistic','P-Value');
info.rnames=strvcat(' ','1','2','3','4','5');
matrix=[results.statistic';results.pval']';
mprint(matrix,info);
fprintf(1,'You should reject the null of no autocorrelation in the squares at the 10 percent level. \n')
fprintf(1,'Now we can fit the data using the GARCHPQ proceedure\n')
fprintf(1,'Please press a key to continue.\n\n')
pause;
fprintf(1,'The command to do this is:\n\n')
fprintf(1,'[parameters, likelihood, stderrors, robustSE, ht, scores, grad]=garchpq(x,1,1)\n \n');
fprintf(1,'\n \n Press any key to estimate the model \n \n')
pause
[parameters, likelihood, ht, stderrors, robustSE, scores, grad]=garchpq(x,1,1);
fprintf(1,'\n \n Press any key to continue the demo \n \n')
pause
fprintf(1,'The estimated constant parameter from this model is: %4.4f \n',parameters(1))
fprintf(1,'The estimated ARCH parameter from this model is: %4.4f \n',parameters(2))
fprintf(1,'The estimated GARCH parameter from this model is: %4.4f \n',parameters(3))
fprintf(1,'The standard T-statistics for these three parameters are %4.4f,  %4.4f,  and %4.4f  respectively.\n',parameters./(diag(stderrors).^(0.5)));
fprintf(1,'and the Robust T-statistics for these three parameters are %4.4f,  %4.4f,  and %4.4f  respectively.\n',parameters./(diag(robustSE).^(0.5)));
fprintf(1,'Now we''ll plot the data, the estimated standard deviations, and the standardized residuals')
fprintf(1,'\n \n Press any key to continue the demo \n \n')
pause
figure(1);
subplot(3,1,1);plot(1:1000,x);legend('Data')
subplot(3,1,2);plot(1:1000,sqrt(ht),1:1000,sqrt(h));legend('Estimated Std Dev','Actual Std Dev');
subplot(3,1,3);plot(1:1000,x./sqrt(ht),'r');legend('Standardized Residuals');
fprintf(1,'Finally, we can test our standardized residuals for GARCH using lmtest2\n')
fprintf(1,'[statistic, pval] = lmtest2(x./sqrt(ht),5)\n')
[results] = lmtest2(x./sqrt(ht),5);
info.cnames=strvcat('Statistic','P-Value');
info.rnames=strvcat(' ','1','2','3','4','5');
matrix=[results.statistic';results.pval']';
mprint(matrix,info);
fprintf(1,'\nAs you can see, we fail to reject the null that the standardized resiudals have autocorrelation in squares.\n\n');
fprintf(1,'\n \n Press any key to continue the demo \n \n')
pause
fprintf(1,'We can also do a test that the data are from a normal distribution with unknown mean and variance.\n')
fprintf(1,'This test uses the function lilliefors.\n')
[stat, critval, H]=lilliefors(x,.05);
if H==1
    fprintf(1,'The calculated statistic was %1.4f, with the critical value at the 5 percent level at %1.4f \n',[stat critval])
    fprintf(1,'We reject the null that the data are from a normal distribution with unknown mean and variance\n')
else
    fprintf(1,'The calculated statistic was %1.4f, with the critical value at the 5 percent level at %1.4f \n',[stat critval]) 
    fprintf(1,'We fail to reject the null that the data are from a normal distribution with unknown mean and variance.\n')
    fprintf(1,'This is not suprising, given that the data are truly normal with heteroskedasticity and the low power of this type of tests.\n')
end
fprintf(1,'\n \n Press any key to continue the demo \n \n');
pause;
fprintf(1,'We can perform a KS test on the standardized data to test that it is standard normal.\n') ;
fprintf(1,'We call this test by [statistic, siglevel, H]=kolmorgorov(x./sqrt(ht),.05,''norm_cdf'').\n');
[statistic, siglevel, H]=kolmogorov(x./sqrt(ht),.05,'norm_cdf');
fprintf(1,'The calculated statistic is %1.4f with an asymptotic significance level of %1.4f.\n\n',[statistic siglevel]);

fprintf(1,'This concludes the first portion of the demo.  The next portion will cover the fattailed_garch and the multigarch programs\n')
fprintf(1,'\n \n Press any key to continue the demo or CTRL-C to quit.\n \n');
pause;
clc;
fprintf(1,'We will not simulate some data from the T-distribution, with 6 d.f. using the following command.  \n\n')
fprintf(1,'[x,h]=fattailed_garchsimulate([.01 .05 .8 6]'',1,1,1000,''STUDENTST'')\n')
fprintf(1,'\n \n Press any key to continue the demo \n \n');
pause;
[x,h]=fattailed_garchsimulate([.01 .05 .8 8]',1,1,2000,'STUDENTST');
fprintf(1,'And we can now fit a garch model with different types of innovations wo the generated data.\n')
fprintf(1,'We will begin by fittering a model with normal errors using:\n')
fprintf(1,'[parameters, likelihood, stderrors, robustSE, ht, scores] = fattailed_garch(x , 1 ,1 , ''NORMAL'')\n')
fprintf(1,'\n \n Press any key to continue the demo \n \n');
pause;
[parameters, likelihood, stderrors, robustSE, ht, scores] = fattailed_garch(x , 1 ,1 , 'NORMAL');
fprintf(1,'\nWe can now fit models using T-distributed errors using:\n')
fprintf(1,'[parametersT, likelihoodT, stderrorsT, robustSET, htT, scoresT] = fattailed_garch(x , 1 ,1 , ''STUDENTST'')\n')
fprintf(1,'\n \n Press any key to continue the demo \n \n');
pause;
[parametersT, likelihoodT, stderrorsT, robustSET, htT, scoresT] = fattailed_garch(x , 1 ,1 , 'STUDENTST');
fprintf(1,'\nWe can initially compare the estimation by a likelihood ratio test, as the T asymptotically nests the normal.\n')
fprintf(1,'The likelihood ratio is -2*(likelihood-likeilhoodT)=%4.4f and the pval is %4.4f\n',[-2*(likelihood-likelihoodT) 1-chis_cdf(-2*(likelihood-likelihoodT),1)])
fprintf(1,'Finally, we can estimate a GED GARCH model using:\n')
fprintf(1,'[parametersGED, likelihoodGED, stderrorsGED, robustSEGED, htGED, scoresGED] = fattailed_garch(x , 1 ,1 , ''GED'')\n')
fprintf(1,'And we can plot the three estimated variances.  They should all be the same as they are all consistent in a QMLE sense for the true variance\n')
fprintf(1,'\n \n Press any key to continue the demo \n \n');
pause;
[parametersGED, likelihoodGED, stderrorsGED, robustSEGED, htGED, scoresGED] = fattailed_garch(x , 1 ,1 , 'GED');
figure(2);
subplot(2,1,1);plot(1:2000,ht,1:2000,htT,1:2000,htGED);legend('Normal Estimated Variance','T Estimated Variance','GED Estimated Variance');
subplot(2,1,2);plot(1:2000,x);legend('Data');
fprintf(1,'\n \n Press any key to continue the demo \n \n');
pause;
fprintf(1,'We can now examine the estimated parameters of the models\n');
parameters=[parameters' 3]';
matrix=[parameters'; parametersT';parametersGED']';
info.cnames=strvcat('Normal','T-dist','GED');
info.rnames=strvcat(' ','CONSTANT','ARCH','GARCH','SHAPE');
mprint(matrix,info)
fprintf(1,'\n and the estimated robust standard errors\n');
robust=[diag(robustSE)' 1]';
matrix2=[robust'.^0.5; diag(robustSET)'.^0.5;diag(robustSEGED)'.^0.5]';
matrix3=matrix./matrix2;
info.cnames=strvcat('Normal','T-dist','GED');
info.rnames=strvcat(' ','CONSTANT','ARCH','GARCH','SHAPE');
mprint(matrix3,info)
fprintf(1,'Unfortunately we cannot compare the GED and the T GARCHes by standard LR tests as they are not nested\n')
fprintf(1,'We can however test the null that the errors are from a the given distribution using the KS Test\n')
fprintf(1,'We will call the KS test 3 times using [statistic, siglevel, H]=kolmorgorov(x,pval,dist,varargin)')
[statistic, siglevel, H]=kolmogorov(x./sqrt(ht),.05,'norm_cdf');
[statisticT, siglevelT, HT]=kolmogorov(x./sqrt(htT),.05,'stdtdis_cdf',parametersT(4));
[statisticGED, siglevelGED, HGED]=kolmogorov(x./sqrt(htGED),.05,'gedcdf',parametersGED(4));
fprintf(1,'\n\nThe results fo the Kolmogorov Smirnov Test for each of the distributions are:\n\n')
matrix=[statistic, siglevel, H;statisticT, siglevelT, HT;statisticGED, siglevelGED, HGED];
info.rnames=strvcat(' ','Normal','T-dist','GED');
info.cnames=strvcat('Statistic   ','Asymp Pvalue   ','Reject Null?');
mprint(matrix,info)
fprintf(1,'\n');
fprintf(1,'\n \n Press any key to continue the demo or CTRL-C to quit.\n \n');
pause;
clc
fprintf(1,'The final potion of the demo will be using real data.')
fprintf(1,'The data was provided by Andrew Patton \n and was used in the') 
fprintf(1,'paper,''The conditional Copula in Finance''\n')
fprintf(1,'The data set consists of two data series, US-DM and US-Yen Exchange rates.\n')
fprintf(1,'The data are 100 times the log diference, and span Jan 1990 throught Dec 1999.\n')
fprintf(1,'Multiplying by 100 makes some of the lower bounds easier to work with when the series volatility is very small.\n')
fprintf(1,'Press any key for a plot of the data.\n')
pause
load fx;
data=[x';y']';
figure(3);
plot(cumprod(1+data/100));legend('US-Yen (X) Daily Exchange Rate','US-DM (Y) Daily Exchange Rate');axis([1 2513 0.5 1.5]);
figure(4);
subplot(2,1,1);plot(x/100);legend('US-Yen (X) Daily Exchange ''Return''');axis([1 2513 -.04  .04])
subplot(2,1,2);plot(y/100,'r');legend('US-DM (Y) Daily Exchange ''Return''');axis([1 2513 -.08  .08])
fprintf(1,'\nPress any key to continue the demo.\n \n');
pause;
fprintf(1,'We can check for autocorrelation in the mean using lmtest1 We will be workign with the US-DM data set.\n');
fprintf(1,'You call this function in the same way you call lmtest2\n\n');
[results] = lmtest1(y,5);
info.cnames=strvcat('Statistic','P-Value');
info.rnames=strvcat(' ','1','2','3','4','5');
matrix=[results.statistic';results.pval']';
mprint(matrix,info);
fprintf(1,'\nAs you can see, we have autocorrelation in the residual.  To remove this, we can use ARMAXFILTER')
fprintf(1,'\n\n [parameters, stderrors, robustSE, SEregression, errors, LLF, scores, likelihoods]= ... \n            armaxfilter(y,1,1,1);\n')
fprintf(1,'Press any key to estimate.\n')
pause;
[parameters, errors, LLF , SEregression, stderrors, robustSE, scores, likelihoods]=armaxfilter(y,1,1,1);
fprintf(1,'\n We estimated an ARMA(1,1) model and noe the lmtest1 on the errors shows:\n')
[results] = lmtest1(errors,5);
info.cnames=strvcat('Statistic','P-Value');
info.rnames=strvcat(' ','1','2','3','4','5');
matrix=[results.statistic';results.pval']';
mprint(matrix,info);
fprintf(1,'And thus no residual autocorrelation at the 5%% level.\n')
fprintf(1,'Press any key to continue.')
pause;
fprintf(1,'We can now fit a GARCH model to the data.\nWe will start with a typical garch with normal errors\n')
fprintf(1,'[parameters, likelihood, stderrors, robustSE, ht, scores] = fattailed_garch(errors , 1 , 1 , ''NORMAL'')');
fprintf(1,'\nPress any key to estimate.')
pause;
[parameters, likelihood, stderrors, robustSE, ht, scores] = fattailed_garch(errors , 1 , 1 , 'NORMAL');
fprintf(1,'\n\nWe can now test to see if the residuals are conditionally normal using kolmogorov as detailed earlier.\n')
fprintf(1,'We call this test by [statistic, siglevel, H]=kolmorgorov(errors./sqrt(ht),.05,''norm_cdf'').\n');
[statistic, siglevel, H]=kolmogorov(errors./sqrt(ht),.05,'norm_cdf');
fprintf(1,'The calculated statistic %4.4f was with a sig level of %4.4f.  \nThus we reject the null that the model is adequate.\n',[statistic,siglevel])
fprintf(1,'We can now see if a more complicated variance estimation will be sufficient, the Asymetric Power Garch.\n')
fprintf(1,'We can call this type of GARCH using the multigarch function.\n')
fprintf(1,'We call this by: [parametersAP, likelihoodAP, stderrorsAP, robustSEAP, htAP, scoresAP]=multigarch(errors,1,1,''APGARCH'',''NORMAL'')\n\n')
fprintf(1,'Press any key to estimate.')
pause;
[parametersAP, likelihoodAP, stderrorsAP, robustSEAP, htAP, scoresAP]=multigarch(errors,1,1,1,'APGARCH','NORMAL');
fprintf(1,'\n\nWe can test if this is better model by using a LR test, -2(likelihood-likelihoodAP)\n')
fprintf(1,'The value of this statistic is %4.4f and the sig level is %4.4f \n',[-2*(likelihood-likelihoodAP), 1-chis_cdf(-2*(likelihood-likelihoodAP),length(parametersAP)-length(parameters))])
fprintf(1,'Thus we reject the initial model in favor of the AP GARCH.\n')
[statistic, siglevel, H]=kolmogorov(errors./sqrt(htAP),.05,'norm_cdf');
fprintf(1,'Again, we can test to see if the distribution is sufficient.  We get a statistic of %4.4f and a pval or %4.4f.\n',[statistic, siglevel]);
fprintf(1,'Again we reject the null fo a correct distribution.  We can now fit a GED Garch using the fattailed_garch function.\n')
fprintf(1,'[parametersGED, likelihoodGED, stderrorsGED, robustSEGED, htGED, scoresGED] = fattailed_garch(errors , 1 ,1 , ''GED''\n');
fprintf(1,'Press any key to estimate.')
pause;
[parametersGED, likelihoodGED, stderrorsGED, robustSEGED, htGED, scoresGED] = fattailed_garch(errors , 1 ,1 , 'GED');
[statistic, siglevel, H]=kolmogorov(errors./sqrt(htGED),.05,'gedcdf',parametersGED(4));
fprintf(1,'\n\nAgain, we can test to see if the distribution is sufficient.  We get a statistic of %4.4f and a pval or %4.4f.\n',[statistic, siglevel]);
fprintf(1,'We do not reject the null at the 5%% level.  Finally we can estimate the model with T distributd errors and AP form\n');
fprintf(1,'using: [parametersT, likelihoodT, stderrorsT, robustSET, htT, scoresT]=multigarch(errors,1,1,''APGARCH'',''STUDENTST'')\n\n');
fprintf(1,'Press any key to estimate.')
pause;
[parametersT, likelihoodT, stderrorsT, robustSET, htT, scoresT]=multigarch(errors,1,1,1,'APGARCH','STUDENTST');
[statistic, siglevel, H]=kolmogorov(errors./sqrt(htT),.05,'stdtdis_cdf',parametersT(6));
fprintf(1,'Again, we can test to see if the distribution is sufficient.  We get a statistic of %4.4f and a pval or %4.4f.\n',[statistic, siglevel]);
fprintf(1,'We fail to reject the null that the distribution if correctly specified.\n')
fprintf(1,'\n\n\nI hope youf found the demo useful and enjoy the toolbox.  If you  have any corections, suggestions or other\ninquiries, please email me at kevin.sheppard@economics.ox.ac.uk.\n\n\nI''ve started work on Version 2 which I expect will contain Multivariate Garch models as well as more C-Mex.\n\n')