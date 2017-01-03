clear;

lookback=252; % use lookback days as estimation (training) period for determining factor exposures.
numFactors=5;
topN=50; % for trading strategy, long stocks with topN expected 1-day returns.

% test on SP600 smallcap stocks. (This MATLAB binary input file
% contains tday, stocks, op, hi, lo, cl arrays.
load('IJR_20080114');

mycls=fillMissingData(cl);

positionsTable=zeros(size(cl));

% note the rows of dailyret are the observations at different time
% periods
dailyret=(mycls-lag1(mycls))./lag1(mycls);

for t=lookback+1:length(tday)

    % here the columns of R are the different observations.
    R=dailyret(t-lookback+1:t,:)'; 

    % avoid any stocks with missing returns
    hasData=find(all(isfinite(R), 2)); 
    
    R=R(hasData, :);
    
    avgR=smartmean(R, 2);
    % subtract mean from returns
    R=R-repmat(avgR, [1 size(R, 2)]); 

    % compute covariance matrix, with observations in rows.
    covR=smartcov(R'); 

    % X is the factor exposures matrix, B the variances of factor returns
    [X, B]=eig(covR); 
    
    X(:, 1:size(X, 2)-numFactors)=[]; % Retain only numFactors

    % b are the factor returns for time period t-1 to t.
    results=ols(R(:, end), X); 
    b=results.beta;

    % Rexp is the expected return for next period assuming factor
    % returns remain constant.
    Rexp=avgR+X*b;
    
    [foo idxSort]=sort(Rexp, 'ascend');
    
    positionsTable(t, hasData(idxSort(1:topN)))=-1; % short topN stocks with lowest expected returns
    positionsTable(t, hasData(idxSort(end-topN+1:end)))=1; % buy topN stocks with highest  expected returns
end

ret=smartsum(backshift(1, positionsTable).*dailyret, 2); % compute daily returns of trading strategy
avgret=smartmean(ret)*252 % compute annualized average return of trading strategy
% A very poor return!
% avgret =
% 
%    -1.8099
