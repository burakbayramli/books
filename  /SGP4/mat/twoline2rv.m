%  -----------------------------------------------------------------------------
% 
%                            procedure twoline2rv
% 
%  this function converts the two line element set character string data to
%    variables and initializes the sgp4 variables. several intermediate varaibles
%    and quantities are determined. note that the result is a structure so multiple
%    satellites can be processed simultaneously without having to reinitialize. the
%    verification mode is an important option that permits quick checks of any
%    changes to the underlying technical theory. this option works using a
%    modified tle file in which the start, stop, and delta time values are
%    included at the end of the second line of data. this only works with the
%    verification mode. the catalog mode simply propagates from -1440 to 1440 min
%    from epoch and is useful when performing entire catalog runs.
% 
% Author: 
%   Jeff Beck 
%   beckja@alumni.lehigh.edu
%   1.0  aug  6, 2006 - update for paper dav
%   2.0  mar  8, 2007 - misc fixes and manual operation updates
%   2.01 may  9, 2007 - fix for correction to year of 57
%   2.02 oct  8, 2007 - fix for manual jdstart jdstop matlab formats
% original comments from Vallado C++ version:
%   author        : david vallado                  719-573-2600    1 mar 2001
% 
%   inputs        :
%   longstr1      - TLE character string 
%   longstr2      - TLE character string 
%   typerun       - character for mode of SGP4 execution 
%                   'c' = catalog mode (propagates at 20 min timesteps from
%                           one day before epoch to one day after)
%                   'v' = verification mode (propagates according to start,
%                           stop, and timestep specified in longstr2)
%                   'm' = manual mode (prompts user for start, stop, and
%                           timestep for propagation)
%   typeinput     - type of manual input           mfe 'm', epoch 'e', dayofyr 'd'
% 
%   outputs       :
%     satrec      - structure containing all the sgp4 satellite information
% 
%   coupling      :
%     getgravconst
%     days2mdhms  - conversion of days to month, day, hour, minute, second
%     jday        - convert day month year hour minute second into julian date
%     sgp4init    - initialize the sgp4 variables
% 
%   references    :
%     norad spacetrack report #3
%     vallado, crawford, hujsak, kelso  2006
%
% [satrec, startmfe, stopmfe, deltamin] = twoline2rv(whichconst, longstr1, ...
%          longstr2, typerun,typeinput)
%  ----------------------------------------------------------------------------*/

function [satrec, startmfe, stopmfe, deltamin] = twoline2rv(whichconst, longstr1, ...
          longstr2, typerun,typeinput)

    global tumin radiusearthkm xke j2 j3 j4 j3oj2  

    deg2rad  =   pi / 180.0;         %  0.01745329251994330;  % [deg/rad]
    xpdotp   =  1440.0 / (2.0*pi);   % 229.1831180523293;  % [rev/day]/[rad/min]  

    revnum = 0; 
    elnum  = 0;
    year   = 0; 
    satrec.error = 0;

%     // set the implied decimal points since doing a formated read
%     // fixes for bad input data values (missing, ...)
    for (j = 11:16)
        if (longstr1(j) == ' ')
            longstr1(j) = '_';
        end
    end

    if (longstr1(45) ~= ' ')
        longstr1(44) = longstr1(45);
    end
    longstr1(45) = '.';
     
    if (longstr1(8) == ' ')
        longstr1(8) = 'U';
    end

    if (longstr1(10) == ' ')
        longstr1(10) = '.';
    end

    for (j = 46:50)
        if (longstr1(j) == ' ')
            longstr1(j) = '0';
        end
    end
    if (longstr1(52) == ' ')
        longstr1(52) = '0';
    end
    if (longstr1(54) ~= ' ')
        longstr1(53) = longstr1(54);
    end
    longstr1(54) = '.';

    longstr2(26) = '.';
     
    for (j = 27:33)
        if (longstr2(j) == ' ')
            longstr2(j) = '0';
        end
    end
     
    if (longstr1(63) == ' ')
        longstr1(63) = '0';
    end

    if ((length(longstr1) < 68) || (longstr1(68) == ' '))
        longstr1(68) = '0';
    end

    % parse first line
    carnumb = str2num(longstr1(1));
    satrec.satnum = str2num(longstr1(3:7));
    classification = longstr1(8);
    intldesg = longstr1(10:17);
    satrec.epochyr = str2num(longstr1(19:20));
    satrec.epochdays = str2num(longstr1(21:32));
    satrec.ndot = str2num(longstr1(34:43));
    satrec.nddot = str2num(longstr1(44:50));
    nexp = str2num(longstr1(51:52));
    satrec.bstar = str2num(longstr1(53:59));
    ibexp = str2num(longstr1(60:61));
    numb = str2num(longstr1(63));
    elnum = str2num(longstr1(65:68));
 
    % parse second line
    if (typerun == 'v')
        cardnumb = str2num(longstr2(1));
        satrec.satnum = str2num(longstr2(3:7));
        satrec.inclo = str2num(longstr2(8:16));
        satrec.nodeo = str2num(longstr2(17:25));
        satrec.ecco = str2num(longstr2(26:33));
        satrec.argpo = str2num(longstr2(34:42));
        satrec.mo = str2num(longstr2(43:51));
        satrec.no = str2num(longstr2(52:63));
        revnum = str2num(longstr2(64:68));
        startmfe = str2num(longstr2(70:81));        
        stopmfe  = str2num(longstr2(83:96)); 
        deltamin = str2num(longstr2(97:105)); 
    else
        cardnumb = str2num(longstr2(1));
        satrec.satnum = str2num(longstr2(3:7));
        satrec.inclo = str2num(longstr2(8:16));
        satrec.nodeo = str2num(longstr2(17:25));
        satrec.ecco = str2num(longstr2(26:33));
        satrec.argpo = str2num(longstr2(34:42));
        satrec.mo = str2num(longstr2(43:51));
        satrec.no = str2num(longstr2(52:63));
        revnum = str2num(longstr2(64:68));
    end

%     // ---- find no, ndot, nddot ----
    satrec.no   = satrec.no / xpdotp; %//* rad/min
    satrec.nddot= satrec.nddot * 10.0^nexp;
    satrec.bstar= satrec.bstar * 10.0^ibexp;

%     // ---- convert to sgp4 units ----
    satrec.a    = (satrec.no*tumin)^(-2/3);                % [er]
    satrec.ndot = satrec.ndot  / (xpdotp*1440.0);          % [rad/min^2]
    satrec.nddot= satrec.nddot / (xpdotp*1440.0*1440);     % [rad/min^3]

%     // ---- find standard orbital elements ----
    satrec.inclo = satrec.inclo  * deg2rad;
    satrec.nodeo = satrec.nodeo * deg2rad;
    satrec.argpo = satrec.argpo  * deg2rad;
    satrec.mo    = satrec.mo     *deg2rad;

    satrec.alta = satrec.a*(1.0 + satrec.ecco) - 1.0;
    satrec.altp = satrec.a*(1.0 - satrec.ecco) - 1.0;

%     // ----------------------------------------------------------------
%     // find sgp4epoch time of element set
%     // remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
%     // and minutes from the epoch (time)
%     // --------------------------------------------------------------

%     // ------------- temp fix for years from 1957-2056 ----------------
%     // ------ correct fix will occur when year is 4-digit in 2le ------
     if (satrec.epochyr < 57)
         year= satrec.epochyr + 2000;
       else
         year= satrec.epochyr + 1900;
     end;

     [mon,day,hr,minute,sec] = days2mdh ( year,satrec.epochdays );
     satrec.jdsatepoch = jday( year,mon,day,hr,minute,sec );

%     // input start stop times manually
     if ((typerun ~= 'v') && (typerun ~= 'c'))
         % ------------- enter start/stop ymd hms values --------------------
           if (typeinput == 'e')
               startyear = input('input start year');
               startmon  = input('input start mon');
               startday  = input('input start day');
               starthr   = input('input start hr');
               startmin  = input('input start min');
               startsec  = input('input start sec');
               jdstart = jday( startyear,startmon,startday,starthr,startmin,startsec );

               stopyear = input('input stop year');
               stopmon  = input('input stop mon');
               stopday  = input('input stop day');
               stophr   = input('input stop hr');
               stopmin  = input('input stop min');
               stopsec  = input('input stop sec');
               jdstop = jday( stopyear,stopmon,stopday,stophr,stopmin,stopsec );

               startmfe = (jdstart - satrec.jdsatepoch) * 1440.0;
               stopmfe  = (jdstop - satrec.jdsatepoch) * 1440.0;
               deltamin = input('input time step in minutes ');
           end;
           % -------- enter start/stop year and days of year values -----------
           if (typeinput == 'd')
               startyear    = input('input start year');
               startdayofyr = input('input start dayofyr');
               stopyear     = input('input stop year');
               stopdayofyr  = input('input stop dayofyr');

               [mon,day,hr,minute,sec] = days2mdh ( startyear,startdayofyr);
               jdstart = jday( startyear,mon,day,hr,minute,sec);
               [mon,day,hr,minute,sec] = days2mdh ( stopyear,stopdayofyr);
               jdstop = jday( stopyear,mon,day,hr,minute,sec);

               startmfe = (jdstart - satrec.jdsatepoch) * 1440.0;
               stopmfe  = (jdstop - satrec.jdsatepoch) * 1440.0;
               deltamin = input('input time step in minutes ');
           end;
           % ------------------ enter start/stop mfe values -------------------
           if (typeinput == 'm')
               startmfe = input('input start mfe: ');
               stopmfe  = input('input stop mfe: ');
               deltamin = input('input time step in minutes: ');
           end;
       end;
%     // perform complete catalog evaluation
     if (typerun == 'c')
         startmfe =  -1440.0;
         stopmfe  =  1440.0;
         deltamin = 20.0;
     end;
     
%     // ------------- initialize the orbit at sgp4epoch --------------
     sgp4epoch = satrec.jdsatepoch - 2433281.5; % days since 0 Jan 1950
     [satrec] = sgp4init(whichconst, satrec, satrec.bstar, satrec.ecco, sgp4epoch, ...
         satrec.argpo, satrec.inclo, satrec.mo, satrec.no, satrec.nodeo);

     