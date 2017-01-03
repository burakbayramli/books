function avgNumHoldDays=calculateAvgHoldday(positionTable)

positionTable0=backshift(1, positionTable);

assert(ndims(positionTable)==2);

capital=smartsum(abs(positionTable), 2);
capital1=fwdshift(1, capital);
capital1(end, :)=0;

numtrades=smartsum(abs(positionTable-positionTable0), 2);

avgNumHoldDays=smartsum(smartsum((capital+capital1), 2))/smartsum(numtrades);
