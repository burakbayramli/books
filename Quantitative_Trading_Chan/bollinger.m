clear;

% First, we define a few parameters for the program.

localSymbol='ESM9'; % Symbol for the front contract
symbol='ES'; % IB symbol for this future
secType='FUT'; % security type
exchange='GLOBEX';

accountName='DU61345'; % This is your Interactive Brokers account name 
numContract=1; % Your desired order size

lookback=10; % Number of periods used to compute Bollinger band
entryZ=2; % The distance between the upper and the lower band is
          % 2*entryZ
exitZ=1; % Exit when price revert to 1 standard deviation from the mean

starthh=9; % Start running strategy at 9:30 ET.
startmin=30;

endhh=16; % Stop running strategy at 16:14:59 ET.
endmm=14;
endss=59;

connect2TWS % Start a new connection to IB
pause(20) % need to pause program to wait for connection to be 
          % established


while (1) % Wait until 2 minutes before 9:30 to begin program
    mytime=clock;
    myyyyy=mytime(1);
    mymm=mytime(2);
    mydd=mytime(3);
    myhh=mytime(4);
    mymin=mytime(5);
 
    if (myhh > starthh || (myhh == starthh & mymin >= startmin-2))
        break;
    end
end

% OK, finally it is time to starting getting information from your brokerage account. Note that IB requires a subscription to each symbol (ES in our case) before you can receive market data updates.

requestAccountUpdates( 1,accountName); % Start receiving account 
   % positions updates
reqMktData(1, symbol, secType, exchange, '', 0, '', '', localSymbol, ''); 

% Start receiving market data for this contract
 
isUpdated=false; % Indicate whether we have updated the last price in
     % the current 1-minute bar

% Now for the main loop for sending orders.

for i=1:Inf
    mytime=clock; % Unfortunately matlab2ibapi cannot yet retrieve 
% server time from IB, hence we need to use local
% time

    % Date and time strings for use in requesting historical data
    myyyyy=mytime(1);
    mymm=mytime(2);
    if (mymm < 10)
        mymmstr=['0', num2str(mymm)];
    else
        mymmstr=num2str(mymm);
    end
    
    mydd=mytime(3);
    if (mydd < 10)
        myddstr=['0', num2str(mydd)];
    else
        myddstr=num2str(mydd);
    end
 
    myhh=mytime(4);
    mymin=mytime(5);
    myss=mytime(6);
    myss=str2double(regexprep(num2str(myss), '\..*', ''));
    
    if (i==1) % As a start, see what we already own in account
        port=getPortfolioUpdates(accountName, symbol);
        if (~isempty(port))
            assert(strcmp(port.symbol, symbol));
            pos=port.position;
        else
            pos=0;
        end
    end
    
    if (myhh >= endhh) % Near market close, exit all positions
        if (pos>0)
	   % Send Market Sell orders to exit
         id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
             localSymbol,'SELL',pos,'MKT',0,0,'DAY','');
        elseif (pos<0)
         % Send Market Buy orders to exit
         id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
             localSymbol,'BUY',abs(pos),'MKT',0,0,'DAY','');
        end
        pos=0;
        break;
    end

    if (i==1) % First time around, get 1 day’s worth of 1-min bar 
		  % historical data from IB
	  
        [es]=reqHistData(i, symbol, secType, exchange, '', 0, '', '',...
             localSymbol, [num2str(myyyyy) mymmstr myddstr ' ' ...
             num2str(myhh) ':' num2str(mymin) ':' num2str(myss)], ...
             '1 D','1 min','TRADES',1,1,2);
        tic;
    else % Thereafter, simply get last price every 60 seconds
        elapsed=toc;
        if (elapsed >= 60) % sample trade prices every minute
            lastPrice=getLastPrice(1);

		% Update historical price array with last price
            es.close(1:end-1)=es.close(2:end);
            es.close(end)=lastPrice;
            tic;
            isUpdated=true;
        end
    end

    if (i==1 || isUpdated)
        cls_hist=es.close; % Update historical price array with last
 				   % price
        cls_hist=cls_hist(end-lookback+1:end);
        ma=mean(cls_hist); % moving average
        mstd=std(cls_hist); % moving standard deviation
        
        isUpdated=false;
    end

    askPrice=getAskPrice(1);
    bidPrice=getBidPrice(1);
    lastPrice=getLastPrice(1);
    
    % Calculate deviation of ask or bid price from moving average
    zscoreAsk=(askPrice-ma)./mstd;
    zscoreBid=(bidPrice-ma)./mstd;
    
   % Check if this is time to send orders
    if (myhh == starthh & mymin < startmin)
        continue;
    end
    
    if (pos==0 & zscoreAsk < -entryZ ) % entry only
        id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
	     localSymbol, 'BUY', numContract, 'MKT',0,0,'DAY','');
        pos=numContract;
    elseif (pos < 0 & zscoreAsk < -entryZ) % exit and entry
        id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
	     localSymbol, 'BUY', 2*numContract, 'MKT',0,0,'DAY','');
        pos=numContract;
    elseif (pos < 0 & zscoreAsk < -exitZ) % exit only
        id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
	     localSymbol, 'BUY', numContract, 'MKT',0,0,'DAY','');
        pos=0;
    elseif (pos==0 & zscoreBid > entryZ) % entry only
        id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
	     localSymbol, 'SELL', numContract, 'MKT',0,0,'DAY','');
        pos=-numContract;
    elseif (pos > 0 & zscoreBid > entryZ ) % exit and entry
        id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
	     localSymbol, 'SELL', 2*numContract, 'MKT',0,0,'DAY','');
        pos=-numContract;
    elseif (pos > 0 & zscoreBid > exitZ ) % exit only
        id=placeOrder(symbol,secType,exchange, '', 0, '', '',...
	     localSymbol, 'SELL', numContract, 'MKT',0,0,'DAY','');
        pos=0;
    end
 
end
 
disconnectFromTWS;
