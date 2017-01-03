clear;
topN=40;

load('inputData_XLEcomponents');

cls_comp=cl;

stocks_comp=stocks;

load('inputData_XLE');

xle=strmatch('XLE', stocks, 'exact');

cls_xle=cl(:, xle);

trainDataLength=250;
testDataLength=250;

trainData=[length(tday)-testDataLength-trainDataLength+1:length(tday)-testDataLength];

adf=NaN(size(cls_comp, 2), 1);

for s=1:size(cls_comp, 2)
	res=cadf(cls_xle(trainData), cls_comp(trainData, s), 0, 1);
	adf(s)=res.adf;
end

[foo, idx]=sort(adf, 'ascend');

idx=idx(1:topN);

stocks_comp(idx)

res=ols(cls_xle(trainData), cls_comp(trainData, idx));

res.beta

basket_MV=smartsum(repmat(res.beta', [size(cls_xle(:, 1)) 1]).*cls_comp(:, idx), 2);
basket_XLE_MV=basket_MV-cls_xle;

mean_trainData=mean(basket_XLE_MV(trainData))
std_trainData=std(basket_XLE_MV(trainData))

testData=[length(tday)-testDataLength+1:length(tday)];

plot(basket_XLE_MV(trainData));
hold on;
plot([NaN(length(trainData), 1); basket_XLE_MV(testData)], 'r');
legend('Train Data', 'Test Data');
hold off;
xlabel('Days');
ylabel('Baskt\_XLE\_MV');
title('Market value of hedged basket');
