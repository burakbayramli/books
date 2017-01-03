% PURPOSE: demo of fturns() 
%          find turning points in time-series 
% 
%---------------------------------------------------
% USAGE: fturns_d
%---------------------------------------------------

% set downturn turning point definition
%        y(t-4), y(t-3), y(t-2), y(t-1) <= y(t) [seq = 0, bdt = 4, eq = 1]
% which is the condition for a downturn if,
%        y(t+1) < y(t)  we have a downturn      [adt = 1]
% else no downturn
in.bdt = 4;
in.adt = 1;
in.eq = 1;
in.seq = 0;
% set upturn turning point definition
%        y(t-4), y(t-3), y(t-2), y(t-1) >= y(t) [seq = 0, but = 4, eq = 1]
% which is the condition for a upturn if, 
%        y(t+1) > y(t)  we have an upturn       [aut = 1]
% else no upturn
in.but = 4;
in.aut = 1;
dates = cal(1982,1,12); % create calendar structure variable
load test.dat;          % monthly time-series on employment for 8 states
y = growthr(test,dates);    % convert to growth-rates
yt = trimr(y,dates.freq,0); % truncate initial zeros
tdates = cal(1983,1,12);    % update calendar for truncation
ytime = yt(:,1);            % pull out state 1 time-series
results = fturns(ytime,in);
plt(results,tdates,'employment');
title('loose non-sequential definition of turns --- produces lots of turns');
pause;

% Now, change to a sequential definition
in.seq = 1;

% which sets downturn turning point definition to:
%        y(t-4) <= y(t-3) <= y(t-2) <= y(t-1) <= y(t) [seq = 1, bdt = 4, eq = 1]
% which is the condition for a downturn if,
%        y(t+1) < y(t)  we have a downturn            [adt = 1]
% else no downturn

% and the upturn turning point definition
%        y(t-4) >= y(t-3) >= y(t-2) >= y(t-1) >= y(t) [seq = 1, but = 4, eq = 1]
% which is the condition for a upturn if
%        y(t+1) > y(t)  which is an upturn            [aut = 1]
% else no upturn

results = fturns(ytime,in);
plt(results,tdates,'employment');
title('sequential definition --- produces fewer turns');
pause;

% Now, illustrate requiring many points after the turns
% which should rule out some of the turning points
% seen in the above graph
in.seq = 1;
in.aut = 4; % 4 periods of up after an upturn
in.adt = 4; % 4 periods of down after a downturn

% which sets downturn turning point definition to:
%        y(t-4), y(t-3), y(t-2), y(t-1) <= y(t)  [seq = 1, bdt = 4, eq = 1]
% which is the condition for a downturn if,
%        y(t+4) < y(t+3) < y(t+2) < y(t+1) < y(t)  we have a downturn [adt = 4]
% else no downturn

% and the upturn turning point definition
%        y(t-4), y(t-3), y(t-2), y(t-1) >= y(t)  [seq = 1, bdt = 4, eq = 1]
% which is the condition for a upturn if
%        y(t+4) > y(t+3) > y(t+2) > y(t+1) > y(t)  which is an upturn [aut = 4]
% else no upturn

results = fturns(ytime,in);
plt_turns(results,tdates,'employment');
title('tighter sequential with aut=4, adt=4 definition --- produces less turns');
pause;

% Now turn off the sequential requirement, which
% should produce more turns that meet this looser definition

in.seq = 0;
results = fturns(ytime,in);
plt(results,tdates,'employment');
title('looser non-sequential definition with aut=4, adt=4 --- produces more turns');
pause;