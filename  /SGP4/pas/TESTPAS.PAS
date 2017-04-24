(*     ----------------------------------------------------------------      *)
(*                                                                           *)

                               PROGRAM TestPAS;
{
*  this program tests the sgp4 propagator.
*
*                          companion code for
*             fundamentals of astrodynamics and applications
*                                  2007
*                            by david vallado
*
*     (w) 719-573-2600, email dvallado@agi.com
*     *****************************************************************
*  current :
*             3 jul 08  david vallado
*                        add switch for afspc compatibility and improved operation
*  changes :

*             2 apr 07  david vallado
*                        fixes for manual operation
*            10 aug 06  david vallado
*                        update mfe for verification time steps, constants
*            20 jul 05  david vallado
*                         fixes for paper, corrections from paul crawford
*             7 jul 04  david vallado
*                         fix record file and get working
*            14 may 01  david vallado
*                         2nd edition baseline
*                   80  norad
*                         original baseline
*     ----------------------------------------------------------------       }

{$V-}                { Allow variable sized strings   }

   Uses
     NewDelay, crt, 
     sgp4ext,
     sgp4io,
     sgp4unit;

   VAR
     Typerun, typeinput, UnitType : CHAR;
     wc : integer;
     InFile,OutFile : Text;
     InFileName : STRING[66];
{}
PROCEDURE DoPrediction;
   VAR
     P,a,Ecc,Incl,node,Argp,Nu,M,ArgLat,TrueLon,LonPer, rad,
     tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2,
     Vkmpersec, startmfe,stopmfe,tsince,
     deltamin,Sec,days,JD   : Extended;
     Longstr1,Longstr2 : Str69;
     Year,code,Mon,Day,Hr,Minute : INTEGER;
     satrec : elsetrec;
     whichconst : gravconsttype;
     r,v,ro,vo,rs,vs : Vector;
     MonStr,Blk3 : Str3;
     Blk1 : char;
     ErrStr: Str12;
     LDate,DDate : STR11;
     satname:str25;

   BEGIN
     rad:= 180.0/pi;
     case wc OF
       721 : whichconst := wgs721; { low precision STR#3 values }
       72  : whichconst := wgs72;
       84  : whichconst := wgs84;
     end; { case }

     { sgp4fix identify constants and allow alternate values }
     getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );
     Vkmpersec := radiusearthkm * xke / 60.0;

     WHILE NOT EOF(InFile) DO
       BEGIN
         { ---- Read in data ---- }
         REPEAT
             Readln( InFile,LongStr1 );
         UNTIL (Longstr1[1] <> '#') or (EOF(InFile));

         if NOT EOF(InFile) THEN
         BEGIN
         if typerun = 'v' then
             Readln( InFile,LongStr2,Blk1,Startmfe, stopmfe,DeltaMin )
           else
             Readln( InFile,LongStr2 );
         TwoLine2RV( whichconst, startmfe,stopmfe,deltamin,
                     LongStr1,LongStr2,'N',typerun,typeinput,Satrec,outfile );

         WriteLn( OutFile,Satrec.satnum:6,' xx' );

         { write out first vector }
         Satrec.T := 0.0;
         SGP4( whichconst, Satrec, 0.0, r,v );
         Write( OutFile, Satrec.T:17:8 );
         WriteLn( OutFile,r[1]:17:8, r[2]:17:8, r[3]:17:8,
                          v[1]:14:9, v[2]:15:9, v[3]:15:9 );

         { --------------- Init parameters for each case ------------- }
         tsince := startmfe;

         { check so the first value isn't written twice }
         if ( abs(tsince) > 1.0e-8 ) THEN
             tsince := tsince - deltamin;

         jd:= satrec.jdsatepoch + startmfe/1440.0;

         { loop to perform the propagation }

         while ((tsince < stopmfe) and (satrec.error = 0)) DO
           BEGIN
             tsince := tsince + deltamin;
             if(tsince > stopmfe) THEN
                 tsince := stopmfe;

             SGP4( whichconst, Satrec, tsince, r,v );

             IF satrec.Error > 0 THEN
                 WRITEln( '*** Error: t:= ',tsince,'*** code = ',satrec.error:3 );

             Invjday( JD,   Year,Mon,Day,Hr,minute,Sec );
             MonStr:= MonthTitle[Mon];
{
             Write( OutFile,JD:12:4,Day:4,MonStr:4,ROUND(100*FRAC(Year*0.01)):3 );
             IF Minute < 10 THEN
                 Write( OutFile,Hr:3,':','0',minute:1,':',Sec:5:2 )
               ELSE
                 Write( OutFile,Hr:3,':',minute:2,':',Sec:5:2 );
             Write( OutFile, tsince:11:6 );
             WriteLn( OutFile,r[1]:17:6,r[2]:17:6,
                          r[3]:17:6,v[1]:17:8,v[2]:17:8,v[3]:17:8 );
}
             Write( OutFile, tsince:17:8 );
             Write( OutFile,r[1]:17:8, r[2]:17:8, r[3]:17:8,
                            v[1]:14:9, v[2]:15:9, v[3]:15:9 );


             rv2coe( r,v,mu,  P,a,Ecc,Incl,node,Argp,Nu,M,ArgLat,TrueLon,LonPer );

             Write( OutFile,a:15:4, ecc:9:6, incl*rad:11:5, node*rad:11:5,
                      argp*rad:11:5, nu*rad:11:5, m*rad:11:5 );

             Write( OutFile,year:5,Day:4,Mon:4 );
             IF Minute < 10 THEN
                 Writeln( OutFile,Hr:3,':','0',minute:1,':',Sec:5:2 )
               ELSE
                 Writeln( OutFile,Hr:3,':',minute:2,':',Sec:5:2 );

             jd:= satrec.jdsatepoch + tsince/1440.0;

           END;  { While through time }

         END; { if not eof }

      END; { While through infile }

   END;  { Procedure DoPrediction }


{ --------------------- M A I N  P R O G R A M ---------------------- }
BEGIN
   PatchCrt( Crt.Delay );

   { Help:= 'Y';   for temp writes in sgp4 }

   Write(' Input obsmode afspc a, improved i ' );
   Readln( obsmode);

   Write(' Input type run v, c, m' );
   Readln( Typerun );

   if ((typerun <> 'v') and (typerun <> 'c')) then
     begin
       write('input mfe, epoch (YMDHMS), or dayofyr approach, m,e,d');
       readln( typeinput );
     end
     else
       typeinput:= 'e';

   Write(' Input constants 721 72 84' );
   Readln( wc );

   WriteLn( 'Input infilename ' );
   ReadLn( InFileName );
   ASSIGN( InFile, InFileName );
   RESET( InFile );

   if typerun = 'v' then
       ASSIGN( OutFile, 'tpasver.out' )
     else
       if typerun = 'c' then
           ASSIGN( OutFile, 'tpasall.out' )
         else
           ASSIGN( OutFile, 'tpas.out' );

   REWRITE( OutFile );

   ASSIGN( Sgp4File, 'tpas.dbg' );
   REWRITE( Sgp4File );

   DoPrediction;

   CLOSE( InFile );
   CLOSE( OutFile     );      { detailed listing }
   CLOSE( Sgp4File    );      { debug listing }
END.   { Program Testpas }

