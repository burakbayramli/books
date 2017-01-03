clear;

entryZscore=0.1;

data=load('inputDataOHLCDaily_20120517', 'syms', 'tday', 'op', 'hi', 'lo', 'cl');
idx=find(strcmp('FSTX', data.syms));

op=data.op(:, idx);
hi=data.hi(:, idx);
lo=data.lo(:, idx);
cl=data.cl(:, idx);

stdretC2C90d=backshift(1, smartMovingStd(calculateReturns(cl, 1), 90));

longs= op >= backshift(1, hi).*(1+entryZscore*stdretC2C90d);
shorts=op <= backshift(1, lo).*(1-entryZscore*stdretC2C90d);

positions=zeros(size(cl));

positions(longs)=1;
positions(shorts)=-1;

ret=positions.*(op-cl)./op;
ret(isnan(ret))=0;

fprintf(1, '%s APR=%10.4f Sharpe=%4.2f\n', data.syms{idx}, prod(1+ret).^(252/length(ret))-1, mean(ret)*sqrt(252)/std(ret));
% APR=    0.1327 Sharpe=1.44
cumret=cumprod(1+ret)-1; % compounded ROE

plot(cumret);


[maxDD maxDDD]=calculateMaxDD(cumret);
fprintf(1, 'Max DD =%f Max DDD in days=%i\n\n', maxDD, round(maxDDD));

