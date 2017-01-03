clear;
% Daily data on EWA-EWC
load('inputData_ETF', 'tday', 'syms', 'cl');
idxA=find(strcmp('EWA', syms));
idxC=find(strcmp('EWC', syms));

x=cl(:, idxA);
y=cl(:, idxC);

% Augment x with ones to  accomodate possible offset in the regression
% between y vs x.

x=[x ones(size(x))];

delta=0.0001; % delta=1 gives fastest change in beta, delta=0.000....1 allows no change (like traditional linear regression).

yhat=NaN(size(y)); % measurement prediction
e=NaN(size(y)); % measurement prediction error
Q=NaN(size(y)); % measurement prediction error variance

% For clarity, we denote R(t|t) by P(t).
% initialize R, P and beta.
R=zeros(2);
P=zeros(2);
beta=NaN(2, size(x, 1));
Vw=delta/(1-delta)*eye(2);
Ve=0.001;


% Initialize beta(:, 1) to zero
beta(:, 1)=0;

% Given initial beta and R (and P)
for t=1:length(y)
    if (t > 1)
        beta(:, t)=beta(:, t-1); % state prediction. Equation 3.7
        R=P+Vw; % state covariance prediction. Equation 3.8
    end
    
    yhat(t)=x(t, :)*beta(:, t); % measurement prediction. Equation 3.9

    Q(t)=x(t, :)*R*x(t, :)'+Ve; % measurement variance prediction. Equation 3.10
    
    
    % Observe y(t)
    e(t)=y(t)-yhat(t); % measurement prediction error
    
    K=R*x(t, :)'/Q(t); % Kalman gain
    
    beta(:, t)=beta(:, t)+K*e(t); % State update. Equation 3.11
    P=R-K*x(t, :)*R; % State covariance update. Euqation 3.12
    
end


fig = figure;
plot(beta(1, :)');
print(fig,'/tmp/beta1m','-dpng')

fig =figure;
plot(beta(2, :)');
print(fig,'/tmp/beta2m','-dpng')

fig = figure;
plot(e(3:end), 'r');
hold on;
plot(sqrt(Q(3:end)));
print(fig,'/tmp/Qm','-dpng')

y2=[x(:, 1) y];

longsEntry=e < -sqrt(Q); % a long position means we should buy EWC
longsExit=e > -sqrt(Q);

shortsEntry=e > sqrt(Q);
shortsExit=e < sqrt(Q);

numUnitsLong=NaN(length(y2), 1);
numUnitsShort=NaN(length(y2), 1);

numUnitsLong(1)=0;
numUnitsLong(longsEntry)=1; 
numUnitsLong(longsExit)=0;
numUnitsLong=fillMissingData(numUnitsLong); % fillMissingData can be downloaded from epchan.com/book2. It simply carry forward an existing position from previous day if today's positio is an indeterminate NaN.

numUnitsShort(1)=0;
numUnitsShort(shortsEntry)=-1; 
numUnitsShort(shortsExit)=0;
numUnitsShort=fillMissingData(numUnitsShort);

numUnits=numUnitsLong+numUnitsShort;
positions=repmat(numUnits, [1 size(y2, 2)]).*[-beta(1, :)' ones(size(beta(1, :)'))].*y2; % [hedgeRatio -ones(size(hedgeRatio))] is the shares allocation, [hedgeRatio -ones(size(hedgeRatio))].*y2 is the dollar capital allocation, while positions is the dollar capital in each ETF.
pnl=sum(lag(positions, 1).*(y2-lag(y2, 1))./lag(y2, 1), 2); % daily P&L of the strategy
ret=pnl./sum(abs(lag(positions, 1)), 2); % return is P&L divided by gross market value of portfolio
ret(isnan(ret))=0;

figure;
plot(cumprod(1+ret)-1); % Cumulative compounded return

fprintf(1, 'APR=%f Sharpe=%f\n', prod(1+ret).^(252/length(ret))-1, sqrt(252)*mean(ret)/std(ret));
% APR=0.262252 Sharpe=2.361162


