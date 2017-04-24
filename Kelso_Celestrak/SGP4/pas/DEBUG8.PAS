     { ----------------- Write out initial data --------------------- }
     IF Show = 'Y' THEN
       BEGIN
         { ----------- Calculate extra parameters from elset ------------ }
         MonStr:= MonthTitle[Mon];

         WriteLn( OutFile,CHR(12) );
         WriteLn( OutFile,'----------------------------------------------------------' );
         WriteLn( OutFile, 'Transmission Format : ' );
         WriteLn( OutFile,LongStr1 );
         WriteLn( OutFile,LongStr2 );
         WriteLn( OutFile );
         IF satrec.Method = 'd' THEN
             WRITELN( OutFile,'Deep Space perturbations used ' )
           ELSE
             WRITELN( OutFile,'Near Earth perturbations used ' );

         WriteLn( OutFile,' Epoch Time     ',Day:2,MonStr:4,Year:5,Hr:3,':',minute:2,':',
                         Sec:7:4,' UTC' );
         WriteLn( OutFile,'Beware: orbital elements may change during init!!! ' );
         WriteLn( OutFile,' Semimajor axis       ',satrec.a:14:8,' ER = ',
                          satrec.a*RadiusEarthkm:12:5, ' km' );
         WriteLn( OutFile,' Perigee altitude     ',satrec.Altp*RadiusEarthKm:14:8,' km' );

         IF satrec.Alta < 999999.5 THEN
             WriteLn( OutFile,' Apogee altitude      ',satrec.Alta*RadiusEarthKm:14:8,' km' )
           ELSE
             WriteLn( OutFile,' Apogee altitude      ','Infinity':14,'  km' );
         WriteLn( OutFile,' Period               ',TwoPi/satrec.no:14:8,' min' );

         NEWTONM( satrec.ecco,satrec.mo,  E1,Nuo );

         WriteLn( OutFile,' Eccentricity         ',satrec.ecco:15:9 );
         WriteLn( OutFile,' Inclination          ',satrec.inclo * Rad:10:4,'     ø' );
         WriteLn( OutFile,' Long of Asc Node     ',satrec.nodeo* Rad:10:4,'     ø' );
         WriteLn( OutFile,' Arg of Perigee       ',satrec.argpo* Rad:10:4,'     ø' );
         WriteLn( OutFile,' Mean Anomaly         ',satrec.mo  * Rad:10:4,'     ø' );
         WriteLn( OutFile,' True Anomaly         ',Nuo*Rad:14:8,' ø' );
         IF typerun <> 'C' THEN
            WriteLn( OutFile,' Mean Motion          ',satrec.no*XpDotp:14:8,' Rev/Day' )
          ELSE
            WriteLn( OutFile,' Mean Motion          ',satrec.no/conv1:14:8,' Rev/Day' );
         WriteLn( OutFile,' N Dot                ',satrec.ndot*XPDotp*1440:14:8,' .5Rev/day2' );
         WriteLn( OutFile,' N Dble Dot           ',satrec.nddot*XPDotp*1440*1440:14:8,' 1/6 rev/day3' );
         WriteLn( OutFile,' satrec.bstar                ',satrec.bstar:14:8,' none (ERm2/kg)' );
         WriteLn( OutFile,' Ballistic Coefficient',BC:12:6,'  kg/m2' );
         WriteLn( OutFile );
         WriteLn( OutFile );
         WriteLn( OutFile,satrec.jdsatepoch:12:4,Day:4,MonStr:4,ROUND(100*FRAC(Year*0.01)):3,Hr:3,':',minute:2,':',Sec:5:2,
                          satrec.epochdays:11:6 );
         WriteLn( OutFile );
       END;

