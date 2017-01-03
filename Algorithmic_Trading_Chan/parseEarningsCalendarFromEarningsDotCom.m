function [earnann]=parseEaringsCalendarFromEarningsDotCom(prevDate,todayDate, allsyms)
% [earnann]==parseEaringsCalendarFromEarningsDotCom(prevDate,todayDate,  allsyms)

% allsyms=regexprep(allsyms, '\.', ''); % for earnings.com, BF.B is BFB
allsyms=regexprep(allsyms, '-', ''); % for earnings.com, BF.B is BFB

earnann=zeros(size(allsyms));

prevEarningsFile=urlread(['http://www.earnings.com/earning.asp?date=', num2str(prevDate), '&client=cb']);
todayEarningsFile=urlread(['http://www.earnings.com/earning.asp?date=', num2str(todayDate), '&client=cb']);

% patternSym='finance.yahoo.com/q\?s=[\w-%\.=&]*">([\w-\.]+)</a>';
% patternSym='<a\s+href="http://finance.yahoo.com/q\?s=[\w%\.=&]*">([\w-\.]+)</a>';
% patternSym='<a href="http://finance.yahoo.com/q\?s=.+">([\w-\.]+)</a>';
% patternTime='<small>([\w:\s]+)</small>';

prevd=day(datenum(num2str(prevDate), 'yyyymmdd'));
todayd=day(datenum(num2str(todayDate), 'yyyymmdd'));

prevmmm=datestr(datenum(num2str(prevDate), 'yyyymmdd'), 'mmm');
todaymmm=datestr(datenum(num2str(todayDate), 'yyyymmdd'), 'mmm');

patternSym='<a\s+href="company.asp\?ticker=([%\*\w\._/-]+)&coid';

% prevDate
patternPrevDateTime=['<td align="center"><nobr>', num2str(prevd), '-', num2str(prevmmm), '([ :\dABPMCO]*)</nobr>'];

symA=regexp(prevEarningsFile, patternSym , 'tokens');
timeA=regexp(prevEarningsFile, patternPrevDateTime, 'tokens');

symsA=[symA{:}];
timeA=[timeA{:}];

assert(length(symsA)==length(timeA));

isAMC=~cellfun('isempty', regexp(timeA, 'AMC'));

patternPM='[ ]+\d:\d\d[ ]+PM'; % e.g. ' 6:00 PM'

isAMC2=~cellfun('isempty', regexp(timeA, patternPM));

symsA=symsA(isAMC | isAMC2);

[foo, idxA, idxALL]=intersect(symsA, allsyms);
earnann(idxALL)=1;

% today
patternTodayDateTime=['<td align="center"><nobr>', num2str(todayd), '-', num2str(todaymmm), '([ :\dABPMCO]*)</nobr>'];

symA=regexp(todayEarningsFile, patternSym , 'tokens');
timeA=regexp(todayEarningsFile, patternTodayDateTime, 'tokens');

symsA=[symA{:}];
timeA=[timeA{:}];

symsA=symsA(1:length(timeA));

assert(length(symsA)==length(timeA));

isBMO=~cellfun('isempty', regexp(timeA, 'BMO'));

patternAM='[ ]+\d:\d\d[ ]+AM'; % e.g. ' 8:00 AM'

isBMO2=~cellfun('isempty', regexp(timeA, patternAM));

symsA=symsA(isBMO | isBMO2);

[foo, idxA, idxALL]=intersect(symsA, allsyms);
earnann(idxALL)=1;
