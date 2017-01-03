clear;

load('inputData_ETF', 'syms', 'tday', 'cl');

uso=cl(:, strcmp('USO', syms));
xle=cl(:, strcmp('XLE', syms));
tday_ETF=tday;

load('inputDataDaily_CL_20120502', 'tday', 'contracts', 'cl');

ratioMatrix=(fwdshift(1, cl')./cl')'; % back/front

ratio=NaN(size(ratioMatrix, 1), 1);
isExpireDate=false(size(ratio));

isExpireDate=isfinite(cl) & ~isfinite(fwdshift(1, cl));

% Define front month as 40 days to 10 days before expiration
numDaysStart=40;
numDaysEnd=10;

for c=1:length(contracts)-1
    expireIdx=find(isExpireDate(:, c));
    if (c==1)
        startIdx=expireIdx-numDaysStart;
        endIdx=expireIdx-numDaysEnd;
    else % ensure next front month contract doesn't start until current one ends
        startIdx=max(endIdx+1, expireIdx-numDaysStart);
        endIdx=expireIdx-numDaysEnd;
    end
        
    if (~isempty(expireIdx))
        ratio(startIdx:endIdx)=ratioMatrix(startIdx:endIdx, c);
    end
end

[tday idxA idxB]=intersect(tday_ETF, tday);
uso=uso(idxA);
xle=xle(idxA);
ratio=ratio(idxB);

positions=zeros(length(tday), 2);

% Contango, negative roll return, buy spot, short future
contango=find(ratio > 1);
positions(contango, :)=repmat([-1 1], [length(contango) 1]);
% Backwardation, positive roll return, short spot, long future
backwardation=find(ratio < 1);
positions(backwardation, :)=repmat([1 -1], [length(backwardation) 1]);

ret=smartsum(lag(positions, 1).*([uso xle]-lag([uso xle], 1))./lag([uso xle], 1), 2)/2;

ret(isnan(ret))=0;

cumret=cumprod(1+ret)-1;

plot(cumret);

fprintf(1, 'Avg Ann Ret=%7.4f Sharpe ratio=%4.2f \n',252*smartmean(ret), sqrt(252)*smartmean(ret)/smartstd(ret));
fprintf(1, 'APR=%10.4f\n', prod(1+ret).^(252/length(ret))-1);
[maxDD maxDDD]=calculateMaxDD(cumret);
fprintf(1, 'Max DD =%f Max DDD in days=%i\n\n', maxDD, round(maxDDD));
% Avg Ann Ret= 0.1592 Sharpe ratio=1.05 
% APR=    0.1591
% Max DD =-0.192321 Max DDD in days=487


% isContango=zeros(size(ratio));
% isContango(ratio > 1)=1;
% 
% hold on; plot(isContango, 'r'); hold on;