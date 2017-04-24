% ------------------------------------------------------------------------------
%
%                           function days2mdh
%
%  this function converts the day of the year, days, to the equivalent month
%    day, hour, minute and second.
%
%  author        : david vallado                  719-573-2600   22 jun 2002
%
%  revisions
%                -
%
%  inputs          description                    range / units
%    year        - year                           1900 .. 2100
%    days        - julian day of the year         0.0  .. 366.0
%
%  outputs       :
%    mon         - month                          1 .. 12
%    day         - day                            1 .. 28,29,30,31
%    hr          - hour                           0 .. 23
%    minute      - minute                         0 .. 59
%    sec         - second                         0.0 .. 59.999
%
%  locals        :
%    dayofyr     - day of year
%    temp        - temporary extended values
%    inttemp     - temporary integer value
%    i           - index
%    lmonth(12)  - integer array containing the number of days per month
%
%  coupling      :
%    none.
%
% [mon,day,hr,minute,sec] = days2mdh ( year,days);
% -----------------------------------------------------------------------------

function [mon,day,hr,minute,sec] = days2mdh ( year,days);

        % --------------- set up array of days in month  --------------
        for i= 1 : 12
            lmonth(i) = 31;
            if i == 2
                lmonth(i)= 28;
              end;
            if i == 4 | i == 6 | i == 9 | i == 11
                lmonth(i)= 30;
              end;
        end

        dayofyr= floor(days );

        % ----------------- find month and day of month ---------------
        if rem(year-1900,4) == 0
            lmonth(2)= 29;
          end

        i= 1;
        inttemp= 0;
        while ( dayofyr > inttemp + lmonth(i) ) & ( i < 12 )
            inttemp= inttemp + lmonth(i);
            i= i+1;
          end

        mon= i;
        day= dayofyr - inttemp;

        % ----------------- find hours minutes and seconds ------------
        temp= (days - dayofyr )*24.0;
        hr  = fix( temp );
        temp= (temp-hr) * 60.0;
        minute = fix( temp );
        sec = (temp-minute) * 60.0;

