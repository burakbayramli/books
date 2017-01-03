function numRTTrades=calculateNumRTTrades(positionTable)
%numRTTrades=calculateNumRTTrades(positionTable)
positionTable0=backshift(1, positionTable);
positionTable0(1, :)=0;
assert(ndims(positionTable)==2);

assert(ndims(positionTable)==2);
capital=abs(positionTable);
capital0=backshift(1, capital);
capital0(1, :)=0;
capital=max(capital, capital0); 

numtrades=abs(positionTable-positionTable0);

numRTTrades=smartsum(smartsum(numtrades./capital, 2), 1)/2;


