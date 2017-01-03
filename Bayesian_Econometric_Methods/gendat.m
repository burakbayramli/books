%this m-file uses the MATLAB random 
%number generators, and compares
%numerical means and standard deviations to known values. 
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));
clc;
num_draws = 10
%----------------------
%Do results for uniform 
%random number generator
%--------------------------
disp('RESULTS - MEAN, STD');
disp('-----------------------------------------------------------------');
disp(' ');
disp('UNIFORM');
tempp = rand(num_draws,1);
unifans = [mean(tempp) std(tempp)];
trueans = [.5 (1/sqrt(12))];
[unifans; trueans]
disp(' ');
clear tempp;
%----------------------
%Do results for Normal 
%random number generator
%--------------------------
disp('STANDARD NORMAL');
tempp = randn(num_draws,1);
normans = [mean(tempp) std(tempp)];
trueans = [0 1];
[normans; trueans]
disp(' ');
clear tempp;
%----------------------
%Do results for Student-t
%random number generator
%--------------------------
disp('STUDENT-T(3)');
tempp = trnd(3,num_draws,1);
tans = [mean(tempp) std(tempp)];
trueans = [0 sqrt(3)];
[tans; trueans]
disp(' ');
clear tempp;
%----------------------
%Do results for Beta
%random number generator
%--------------------------
disp('BETA(3,2)');
tempp = betarnd(3,2,num_draws,1);
betaans = [mean(tempp) std(tempp)];
trueans = [ (3/(3+2))  ( 1 / 5 )];
[betaans; trueans]
disp(' ');
clear tempp;
%----------------------
%Do results for Exponential
%random number generator
%--------------------------
disp('Exponential (5)');
tempp = exprnd(5,num_draws,1);
expans = [mean(tempp) std(tempp)];
trueans = [5 5];
[expans; trueans]
disp(' ');
clear tempp;
%----------------------
%Do results for Chi-Square
%random number generator
%--------------------------
disp('Chi-Square (3)');
tempp = chi2rnd(3,num_draws,1);
chi2ans = [mean(tempp) std(tempp)];
trueans = [3 sqrt(6)];
[chi2ans; trueans]
disp(' ');
clear tempp;
%----------------------
%Do results for Gamma
%random number generator
%--------------------------
disp('Gamma(4,2)');
tempp = gamrnd(4,2,num_draws,1);
gamans = [mean(tempp) std(tempp)];
trueans = [8 4];
[gamans; trueans]
disp(' ');
clear tempp;