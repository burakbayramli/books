function my_prices=fillMissingData(prices, varargin)
%  my_prices=fillMissingData(prices) fills missing price with previous
%  day's price
%  my_prices=fillMissingData(prices, tday, cday) fills missing prices with previous day's price including non-trading days
%  as specified in cday


if (nargin == 1)
    my_prices=prices;
    for t=2:size(my_prices, 1)
        missData=~isfinite(my_prices(t, :, :));
        my_prices(t, missData)=my_prices(t-1, missData);
    end
else
    % We deal only with 2 dim prices array here.
    tday=varargin{1};
    cday=varargin{2};
    my_prices=NaN*zeros(size(cday, 1), size(prices, 2));

    tdayIdx=find(tday==cday(1));
    if (~isempty(tdayIdx))
        my_prices(1, :)=prices(tdayIdx, :);
    end
    
    for t=2:size(my_prices, 1)
        tdayIdx=find(tday==cday(t));
        if (~isempty(tdayIdx))
            my_prices(t, :)=prices(tdayIdx, :);
            
            missData=find(~isfinite(my_prices(t, :)));
            my_prices(t, missData)=my_prices(t-1, missData);
        else
            my_prices(t, :)=my_prices(t-1, :);
        end
    end
end