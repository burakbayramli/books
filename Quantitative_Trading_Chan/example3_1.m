% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

clear; % make sure previously defined variables are erased.

symbol='IBM'; % the stock of interest

historicalPriceFile =urlread(['http://finance.yahoo.com/q/hp?s=', symbol]); % retrieving a webpage

dateField=regexp(historicalPriceFile, '<td class="yfnc_tabledata1" nowrap align="right">([\d\w-]+)</td>', 'tokens'); % extracting the date field to a cell array of cells

numField=regexp(historicalPriceFile, '<td class="yfnc_tabledata1" align="right">([\d\.,]+)</td>', 'tokens'); % extracting the numbers field to a cell array of cells

dates=[dateField{:}]'; % convert to cell array of strings

numField=[numField{:}]'; % convert to cell array of strings

% convert to doubles array
op=str2double(numField (1:6:end)); % open
hi=str2double(numField (2:6:end)); % high
lo=str2double(numField (3:6:end)); % low
cl=str2double(numField (4:6:end)); % close
vol=str2double(numField (5:6:end)); % volume
adjCl=str2double(numField (6:6:end)); % adjusted close
