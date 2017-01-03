function demoFanCurve
% demoFanCurve  Multivariate fit of fan data:  dp = f(q,v).  The fit function is
%               linear in v and cubic in q, including cross products
%
% Synopsis:  demoFanCurve
%
% Input:  None, all data is read from files
%
% Output: Print out of surface fit coefficients, and plot of data and fit

% --- Load and plot experimental data
[q7,dp7]   = loadColData('fan7v.dat',2,1);
[q8,dp8]   = loadColData('fan8v.dat',2,1);
[q9,dp9]   = loadColData('fan9v.dat',2,1);
[q10,dp10] = loadColData('fan10v.dat',2,1);
[q11,dp11] = loadColData('fan11v.dat',2,1);
[q12,dp12] = loadColData('fan12v.dat',2,1);
[q13,dp13] = loadColData('fan13v.dat',2,1);

plot(q7,dp7,'o',q8,dp8,'s',q9,dp9,'+',q10,dp10,'^',...
     q11,dp11,'h',q12,dp12,'v',q13,dp13,'*')
legend('7 V','8 V','9 V','10 V','11 V','12 V','13 V');

% --- Construct global fit function:  dp = f(q,v)
q  = [q7; q8; q9; q10; q11; q12; q13];               %  q column vector
dp = [ dp7;  dp8;  dp9;  dp10;  dp11;  dp12;  dp13]; %  dp column vector
v  = [ 7*ones(size(dp7));                            %  v column vector with
       8*ones(size(dp8));                            %  same length as q and dp
       9*ones(size(dp9));
      10*ones(size(dp10));
      11*ones(size(dp11));
      12*ones(size(dp12));
      13*ones(size(dp13)) ];
      
A = amatrix13(q,v);      %  Assemble matrix for overdetermined system
c = A\dp;                %  Solve overdetermined system
fprintf('\nc = \n');
fprintf('  %14.4e\n',c); %  Print in scientific notation, one c(i) per line

% --- Evaluate curve fit at each voltage and add plot to current figure
hold on
addQVfit('amatrix13',q7,7,c);      %  add 7V curve fit to current figure
addQVfit('amatrix13',q8,8,c);      %  add 8V curve fit, etc
addQVfit('amatrix13',q9,9,c);
addQVfit('amatrix13',q10,10,c);
addQVfit('amatrix13',q11,11,c);
addQVfit('amatrix13',q12,12,c);
addQVfit('amatrix13',q13,13,c);
hold off;  axis([0 60 0 0.12]);
xlabel('q  (CFM)');  ylabel('\Delta p   (inch H_2O)');

% =========================================
function addQVfit(Afun,q,v,a)
% addQVfit  Evaluate and plot curve fit for dp = fcn(q,v) given coefficients
%           of the global polynomial fit
%
% Synopsis:  addQVfit(Afun,q,v,a)
%
% Input:  Afun = (string) name of function to evaluate matrix of
%                the overdetermined system defined by basis functions
%         q = vector of flow rate values
%         v = scalar value of fan voltage
%         a = vector of coefficients obtained from least squares fit
%             of dp = f(q,v).  The a vector must be compatible with
%             the matrix returned by Afun
%
% Output: Plot of dp = f(q,v) is added to current figure window
qfit = linspace(min(q),1.1*max(q))';  %  Note that qfit is a column vector
vfit = v*ones(size(qfit));            %  Create v vector compatible with qfit
Afit = feval(Afun,qfit,vfit);         %  Eval basis fcns at qfit and vfit
dpfit = Afit*a;                       %  dp value obtained from fit
plot(qfit,dpfit)                      %  Add line plot to current figure

% =========================================
function A = amatrix13(q,v)
% amatrix13  Evaluate matrix of overdetermined system for fit that
%            is linear in v and cubic in q
%
% Synopsis:  A = amatrix13(q,v)
%
% Input: q = vector of flow rate values
%        v = vector of fan voltages.  Note length(q) must equal length(v)
%
% Output:  A = matrix of overdetermined system defined by basis functions
A = [ones(size(q))  q  q.^2  q.^3  v  v.*q  v.*q.^2  v.*q.^3];
