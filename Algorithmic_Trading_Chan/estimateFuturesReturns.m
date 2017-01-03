clear;

% load('inputDataDaily_VX_20120507', 'tday', 'contracts', 'cl');
load('//dellquad/Futures_data/inputDataDaily_CL_20120813', 'tday', 'contracts', 'cl');
% load('//dellquad/Futures_data/inputDataDaily_TU_20120813', 'tday', 'contracts', 'cl');
% load('//dellquad/Futures_data/inputDataDaily_BR_20120813', 'tday', 'contracts', 'cl');
% load('//dellquad/Futures_data/inputDataDaily_HG_20120813', 'tday', 'contracts', 'cl');
% load('//dellquad/Futures_data/inputDataDaily_C2_20120813', 'tday', 'contracts', 'cl');
% load('inputDataDaily_C2_20120813', 'tday', 'contracts', 'cl');
% load('//dellquad/Futures_data/inputDataDaily_HO2_20120813', 'tday', 'contracts', 'cl');

% Find spot prices
spotIdx=find(strcmp(contracts, '0000$'));
spot=cl(:, spotIdx);
cl(:, spotIdx)=[];
contracts(spotIdx)=[];

T=[1:length(spot)]';
isBadData=~isfinite(spot);
spot(isBadData)=[];
T(isBadData)=[];
res=ols(log(spot), [T ones(size(T, 1), 1)]);

fprintf(1, 'Average annualized spot return=%f\n', 252*smartmean(res.beta(1)));



% Fitting gamma to forward curve
gamma=NaN(size(tday));
for t=1:length(tday)
   
    FT=cl(t, :)';
    idx=find(isfinite(FT));
    idxDiff=fwdshift(1, idx)-idx; % ensure consecutive months futures
    if (length(idx) >= 5 && all(idxDiff(1:4)==1))
        FT=FT(idx(1:5)); % only uses the nearest 5 contracts
        T=[1:length(FT)]';
%         scatter(T, log(FT));
        res=ols(log(FT), [T ones(size(T, 1), 1)]);
        gamma(t)=-12*res.beta(1);
    end
end
isBadData=find(isnan(gamma));
gamma(isBadData)=[];
tday(isBadData)=[];

plot(gamma);


%print -r300 -djpeg fig5_4
% hold on;

fprintf(1, 'Average annualized roll return=%f\n', smartmean(gamma));

% save('C2_gamma', 'tday', 'gamma');