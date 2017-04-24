(*     ----------------------------------------------------------------      *)
(*                                                                           *)

                               UNIT SGP4IO;

(*                                                                           *)
(*    this file contains a function to read two line element sets. while     *)
(*    not formerly part of the sgp4 mathematical theory, it is               *)
(*    required for practical implemenation.                                  *) 
(*                                                                           *)
(*                            companion code for                             *)
(*               fundamentals of astrodynamics and applications              *)
(*                                    2007                                   *)
(*                              by david vallado                             *)
(*                                                                           *)
(*       (w) 719-573-2600, email dvallado@agi.com                            *)
(*                                                                           *)
(*  current :                                                                *)
(*             9 may 07  david vallado                                       *)
(*                         fix for correction to year of 57                  *)
(*  changes :                                                                *)
(*             2 apr 07  david vallado                                       *)
(*                         updates for manual operation                      *)
(*            14 aug 06  david vallado                                       *)
(*                         original baseline                                 *)
(*                                                                           *)
(*     ----------------------------------------------------------------      *)

                                  INTERFACE

(*     ----------------------------------------------------------------      *)

  Uses
     sgp4ext,
     sgp4unit;

  TYPE
     Str3 = STRING[3];

  VAR
     MonthTitle  : ARRAY[1..12] of Str3;


PROCEDURE TwoLine2RV         ( whichconst : gravconsttype;
                               VAR startmfe,stopmfe,deltamin         : extended;
                               LongStr1,LongStr2                     : Str69;
                               Show,Typerun, typeinput               : CHAR;
                               VAR SatRec                            : ElSetRec;
                               VAR OutFile                           : TEXT );


(*     ----------------------------------------------------------------      *)

                                IMPLEMENTATION

(*     ----------------------------------------------------------------      *)


(*-----------------------------------------------------------------------------
*
*                           procedure twoline2rv
*
*  this function converts the two line element set character string data to
*    variables and initializes the sgp4 variables. several intermediate varaibles
*    and quantities are determined. note that the result is a record so multiple
*    satellites can be processed simultaneously without having to reinitialize. the
*    verification mode is an important option that permits quick checks of any
*    changes to the underlying technical theory. this option works using a
*    modified tle file in which the start, stop, and delta time values are
*    included at the end of the second line of data. this only works with the
*    verification mode. the catalog mode simply propagates from -1440 to 1440 min
*    from epoch and is useful when performing entire catalog runs.
*
*  author        : david vallado                  719-573-2600    7 mar 2007
*
*  inputs        :
*    longstr1    - first line of the tle
*    longstr2    - second line of the tle
*    typerun     - type of run                    'v', 'c', 'm'
*    typeinput   - type of manual input           mfe 'm', epoch 'e', dayofyr 'd'
*
*  outputs       :
*    satrec      - structure containing all the sgp4 satellite information
*    startmfe    - start min from epoch
*    stopmfe     - stop min from epoch
*    deltamin    - time step                      min
*
*  coupling      :
*    days2mdhms  - conversion of days to month, day, hour, minute, second
*    jday   - convert day month year hour minute second into julian date
*    sgp4init    - initialize the sgp4 variables
*
*  references    :
*    norad spacetrack report #3
*    vallado, crawford, hujsak, kelso  2006
  ---------------------------------------------------------------------------- *)

PROCEDURE TwoLine2RV      ( whichconst : gravconsttype;
                            VAR startmfe,stopmfe,deltamin         : extended;
                            LongStr1,LongStr2                     : Str69;
                            Show,Typerun, typeinput               : CHAR;
                            VAR SatRec                            : ElSetRec;
                            VAR OutFile                           : TEXT );
   CONST
     Deg2Rad       : EXTENDED =   pi / 180.0;  { 0.01745329251994330; }
  TYPE
     Str7  = STRING[7];   Str6  = STRING[6];
     Str10 = STRING[10];  Str11 = STRING[11];
     Str12 = STRING[12];  Str34 = STRING[34];
     Rect = RECORD
              INTD : str12;   SatN : str6;
              Comm : str25;   Own  : str6;
              LD   : str11;   Sit  : str7;
              DD   : str11;   Peri : str34;
              Com  : str3;    RCSv : str10;
              RCSTy: str3;
            END;
   VAR
     LongString : Str255;
     CurrRec : Rect;
     J2oP2, MinfEpoch, nodeDot, ArgpDot, Edot,BC,Conv1,Conv2,Conv3,E1,
     nuo,sec,  p, xpdotp : EXTENDED;
     Offset,Year,Mon,Day,Hr,Minute, j, Init, Code,nexp : INTEGER;

     startsec, stopsec, startdayofyr, stopdayofyr, jdstart, jdstop : EXTENDED;
     startyear, stopyear, startmon, stopmon, startday, stopday,
     starthr, stophr, startmin, stopmin : INTEGER;

     MonStr, comc,rcst : str3;
     SatN1 : str6;
     Satn : longint;
     site : str7;
     owner : str10;
     intdes : str12;
     Common, TempName : Str25;
     period : str34;

     mu, radiusearthkm, xke, j2, j3, j4, j3oj2,
     VKmPerSec, tumin  : Extended;

   BEGIN
     { ------------------------- SET CONSTANTS ---------------------- }
     XPDOTP       :=   1440.0 / (2.0*pi);  { 229.1831180523293;}

     { sgp4fix identify constants and allow alternate values }
     getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );
     Conv1        := 2.0*Pi*TUMin/1440.0;
     VKmPerSec    :=   RadiusEarthKm * XKE / 60.0;

     VAL( COPY(Longstr1,2,6),satrec.satnum,code );
     WriteLn( satrec.satnum:6 );

     { ---- Find No, ndot, nddot ---- }
     VAL( COPY(Longstr2,53,11),satrec.No,code );   { rev/day   }
     VAL( COPY(Longstr1,34,10),satrec.ndot,code ); { rev/day2  }
     VAL( COPY(Longstr1,45,6),satrec.nddot,code ); { rev/2day2 }
     VAL( COPY(Longstr1,51,2),nexp,code );
     satrec.NDDot:= ( satrec.NDDot / 100000.0 ) * POWER( 10,NExp );

     VAL( COPY(LongStr1,54,6),satrec.bstar,code );
     VAL( COPY(Longstr1,60,2),nexp,code );
     satrec.bstar:= ( satrec.bstar / 100000.0 ) * POWER( 10,nExp );

     { ---- Ballistic Coefficient ---- }
     IF ABS(satrec.bstar) > 0.00000001 THEN
         BC:= 1.0/(12.741621*satrec.bstar)
       ELSE
         BC:= 1.111111111111111;

     { ---- Convert to sgp4 units ---- }
     satrec.no   := satrec.no / XPDOTP; { rad/min }
     satrec.a    := POWER( satrec.no*TUMin,(-2.0/3.0) );
     satrec.ndot := satrec.ndot  / (XPDOTP*1440);  { ? * minperday }
     satrec.nddot:= satrec.nddot / (XPDOTP*1440*1440);

     { ---- Find standard orbital elements ---- }
     VAL( COPY(Longstr2,27,7),satrec.ecco,code );
     satrec.ecco  := satrec.ecco / 10000000.0;
     VAL( COPY(Longstr2,9,8),satrec.inclo,code );
     satrec.inclo := satrec.inclo * deg2rad;
     VAL( COPY(Longstr2,18,8),satrec.nodeo,code );
     satrec.nodeo:= satrec.nodeo * deg2rad;
     VAL( COPY(Longstr2,35,8),satrec.argpo,code );
     satrec.argpo := satrec.argpo * deg2rad;
     VAL( COPY(Longstr2,44,8),satrec.mo,code );
     satrec.mo    := satrec.mo * deg2rad;

     { ---- Apogee and perigee altitudes ---- }
     IF ABS(satrec.ecco-1.0) > 0.000001 THEN
       BEGIN
         satrec.Altp:= ( (satrec.a*(1.0-satrec.ecco))-1.0 );
         satrec.Alta:= ( (satrec.a*(1.0+satrec.ecco))-1.0 );
       END
       ELSE
       BEGIN { Parabolic }
         satrec.Alta:= 999999.9;
         satrec.Altp:= 2.0* POWER( 4.0/(satrec.no*satrec.no),1.0/3.0 );
       END;

     { ---- Find Sgp4Epoch time of element set ---- }
     VAL( COPY(LongStr1,21,12),satrec.epochdays,code );
     VAL( COPY(LongStr1,19,2),satrec.epochyr,code );

     if (satrec.epochyr < 57) THEN
         year := satrec.epochyr + 2000
       else
         year:= satrec.epochyr + 1900;

     days2mdhms ( year,satrec.epochdays, mon,day,hr,minute,sec );
     jday( year,mon,day,hr,minute,sec, satrec.jdsatepoch );

     { ------------------ input start stop times manually ------------------- }
     if ((typerun <> 'v') and (typerun <> 'c')) then
       begin
         { ------------- enter start/stop ymd hms values -------------------- }
         if (typeinput = 'e') then
           begin
             write('input start y m d h m s ');
             readln(startyear,startmon,startday,starthr,startmin,startsec);
             jday( startyear,startmon,startday,starthr,startmin,startsec, jdstart );

             write('input stop  y m d h m s ');
             readln(stopyear,stopmon,stopday,stophr,stopmin,stopsec);
             jday( stopyear,stopmon,stopday,stophr,stopmin,stopsec, jdstop );

             startmfe := (jdstart - satrec.jdsatepoch)*1440.0;
             stopmfe  := (jdstop - satrec.jdsatepoch)*1440.0;

             write('input time step in minutes ');
             readln( deltamin );
           end;
         { -------- enter start/stop year and days of year values ----------- }
         if (typeinput = 'd') then
           begin
             write('input start y dayofyr ');
             readln(startyear,startdayofyr);

             write('input stop  y dayofyr ');
             readln(stopyear,stopdayofyr);

             days2mdhms ( startyear,startdayofyr, mon,day,hr,minute,sec );
             jday( startyear,mon,day,hr,minute,sec, jdstart );
             days2mdhms ( stopyear,stopdayofyr, mon,day,hr,minute,sec );
             jday( stopyear,mon,day,hr,minute,sec, jdstop );

             startmfe := (jdstart - satrec.jdsatepoch)*1440.0;
             stopmfe  := (jdstop - satrec.jdsatepoch)*1440.0;

             write('input time step in minutes ');
             readln( deltamin );
           end;
         { ------------------ enter start/stop mfe values ------------------- }
         if (typeinput = 'm') then
           BEGIN
             Write('input start stop and delta min from epoch ');
             readln( startmfe,stopmfe,deltamin );
           END;
       END;

     { ------------ perform complete catalog evaluation, -+ 1 day ----------- }
     if (UPCASE(typerun) = 'C') THEN
       BEGIN
         startmfe := -1440.0;
         stopmfe  :=  1440.0;
         deltamin :=    10.0;
       END;

     { ------------- Initialize the orbit at sgp4Epoch -------------- }
     { ---------- Init and the time must be set as shown! ----------- }
     SGP4Init( whichconst, satrec.satnum, satrec.jdsatepoch-2433281.5,
               satrec.bstar, satrec.ecco, satrec.argpo, satrec.inclo, 
               satrec.mo, satrec.no, satrec.nodeo, Satrec  );

(*    {$I debug8.pas }*)

   END;  { PROCEDURE TwoLine2RV }


BEGIN

END.


