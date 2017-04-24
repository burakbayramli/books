/* ---------------------------------------------------------------------
*
*                              testcpp.cpp
*
*  this program tests the sgp4 propagator. an stk ephemeris file is generated
*  along with the test output. the code for this is left justified for easy
*  location.
*
*                          companion code for
*             fundamentals of astrodynamics and applications
*                                  2007
*                            by david vallado
*
*     (w) 719-573-2600, email dvallado@agi.com
*     *****************************************************************
*  current :
*             3 sep 08  david vallado
*                        add switch for afspc compatibility and improved operation
*  changes :
*            14 may 08  david vallado
*                        fixes for linux suggested by brian micek
*                        misc fixes noted by the community - manual operation,
*                        formats, char lengths
*            14 aug 06  david vallado
*                        update mfe for verification time steps, constants
*            20 jul 05  david vallado
*                         fixes for paper, corrections from paul crawford
*             7 jul 04  david vallado
*                         fix record file and get working
*            14 may 01  david vallado
*                         2nd edition baseline
*                   80  norad
*                         original baseline
*       ----------------------------------------------------------------      */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <fstream>

#ifdef _WIN32
#include <io.h>
#endif

//#include <conio.h>

#include "sgp4ext.h"
#include "sgp4unit.h"
#include "sgp4io.h"


int main()
{
	char str[2];
	char infilename[15];
	double ro[3];
	double vo[3];
        char typerun, typeinput, opsmode;
        gravconsttype  whichconst;
        int whichcon;
	FILE *infile, *outfile, *outfilee;

// ----------------------------  locals  -------------------------------
        double p, a, ecc, incl, node, argp, nu, m, arglat, truelon, lonper;
	double sec,  jd, rad, tsince, startmfe, stopmfe, deltamin;
        double tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2;
	int  year; int mon; int day; int hr; int min;
	char longstr1[130];
        typedef char str3[4];
        str3 monstr[13];
        char outname[64];
	char longstr2[130];
	elsetrec satrec;

        rad = 180.0 / pi;
// ------------------------  implementation   --------------------------
strcpy(monstr[1], "Jan");
strcpy(monstr[2], "Feb");
strcpy(monstr[3], "Mar");
strcpy(monstr[4], "Apr");
strcpy(monstr[5], "May");
strcpy(monstr[6], "Jun");
strcpy(monstr[7], "Jul");
strcpy(monstr[8], "Aug");
strcpy(monstr[9], "Sep");
strcpy(monstr[10], "Oct");
strcpy(monstr[11], "Nov");
strcpy(monstr[12], "Dec");

        printf("%s\n",SGP4Version );

        //opsmode = 'a' best understanding of how afspc code works
        //opsmode = 'i' imporved sgp4 resulting in smoother behavior
        printf("input operation mode a, i \n\n");
        opsmode = getchar();
        fflush(stdin);

        //typerun = 'c' compare 1 year of full satcat data
        //typerun = 'v' verification run, requires modified elm file with
        //              start, stop, and delta times
        //typerun = 'm' maunual operation- either mfe, epoch, or dayof yr also
        printf("input type of run c, v, m \n\n");
        typerun = getchar();
        fflush(stdin);

        //typeinput = 'm' input start stop mfe
        //typeinput = 'e' input start stop ymd hms
        //typeinput = 'd' input start stop yr dayofyr
        if ((typerun != 'v') && (typerun != 'c'))
          {
            printf("input mfe, epoch (YMDHMS), or dayofyr approach, m,e,d \n\n");
            typeinput = getchar();
          }
          else
            typeinput = 'e';

        printf("input which constants 721 72 84 \n");
        scanf( "%i",&whichcon );
        if (whichcon == 721) whichconst = wgs72old;
        if (whichcon == 72) whichconst = wgs72;
        if (whichcon == 84) whichconst = wgs84;

        getgravconst( whichconst, tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2 );

        // ---------------- setup files for operation ------------------
        // input 2-line element set file
        printf("input elset filename: \n");
        scanf( "%s",infilename );
        infile = fopen(infilename, "r");
        if (infile == NULL)
	  {
	    printf("Failed to open file: %s\n", infilename);
	    return 1;
	  }

        if (typerun == 'c')
            outfile = fopen("tcppall.out", "w");
          else
            {
            if (typerun == 'v')
                outfile = fopen("tcppver.out", "w");
              else
                outfile = fopen("tcpp.out", "w");
            }

//        dbgfile = fopen("sgp4test.dbg", "w");
//        fprintf(dbgfile,"this is the debug output\n\n" );

        // ----------------- test simple propagation -------------------
        while (feof(infile) == 0)
          {
            do
              {
                fgets( longstr1,130,infile);
                strncpy(str, &longstr1[0], 1);
                str[1] = '\0';
              } while ((strcmp(str, "#")==0)&&(feof(infile) == 0));

            if (feof(infile) == 0)
              {
                fgets( longstr2,130,infile);
                // convert the char string to sgp4 elements
                // includes initialization of sgp4
                twoline2rv( longstr1, longstr2, typerun, typeinput, opsmode, whichconst, 
                            startmfe, stopmfe, deltamin, satrec );
                fprintf(outfile, "%ld xx\n", satrec.satnum);
                printf(" %ld\n", satrec.satnum);
                // call the propagator to get the initial state vector value
                sgp4 (whichconst, satrec,  0.0, ro,  vo);

// generate .e files for stk
jd = satrec.jdsatepoch;
strncpy(outname,&longstr1[2],5);
outname[5]= '.';
outname[6]= 'e';
outname[7]= '\0';
invjday( jd, year,mon,day,hr,min, sec );
outfilee = fopen(outname, "w");
fprintf(outfilee,"stk.v.4.3 \n"); // must use 4.3...
fprintf(outfilee,"\n");
fprintf(outfilee,"BEGIN Ephemeris \n");
fprintf(outfilee," \n");
fprintf(outfilee,"NumberOfEphemerisPoints		146 \n");
fprintf(outfilee,"ScenarioEpoch	  %3i %3s%5i%3i:%2i:%12.9f \n",day,monstr[mon],
                  year,hr,min,sec );
fprintf(outfilee,"InterpolationMethod		Lagrange \n");
fprintf(outfilee,"InterpolationOrder		5 \n");
fprintf(outfilee,"CentralBody				Earth \n");
fprintf(outfilee,"CoordinateSystem			TEME \n");
fprintf(outfilee,"CoordinateSystemEpoch	%3i %3s%5i%3i:%2i:%12.9f \n",day,
                  monstr[mon],year,hr,min,sec );
fprintf(outfilee,"DistanceUnit			Kilometers \n");
fprintf(outfilee," \n");
fprintf(outfilee,"EphemerisTimePosVel \n");
fprintf(outfilee," \n");
fprintf(outfilee, " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f\n",
                 satrec.t,ro[0],ro[1],ro[2],vo[0],vo[1],vo[2]);

                fprintf(outfile, " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f\n",
                        satrec.t,ro[0],ro[1],ro[2],vo[0],vo[1],vo[2]);

                tsince = startmfe;
                // check so the first value isn't written twice
                if ( fabs(tsince) > 1.0e-8 )
                    tsince = tsince - deltamin;

                // ----------------- loop to perform the propagation ----------------
                while ((tsince < stopmfe) && (satrec.error == 0))
                  {
                   tsince = tsince + deltamin;

                   if(tsince > stopmfe)
                       tsince = stopmfe;

                   sgp4 (whichconst, satrec,  tsince, ro,  vo);

                   if (satrec.error > 0)
                       printf("# *** error: t:= %f *** code = %3d\n",
                               satrec.t, satrec.error);

                   if (satrec.error == 0)
                     {
                       if ((typerun != 'v') && (typerun != 'c'))
                         {
                            jd = satrec.jdsatepoch + tsince/1440.0;
                            invjday( jd, year,mon,day,hr,min, sec );

                            fprintf(outfile,
                                    " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f %5i%3i%3i %2i:%2i:%9.6f\n",
                                    tsince, ro[0],ro[1],ro[2],vo[0],vo[1],vo[2],year,mon,day,hr,min,sec );
//                            fprintf(outfile, " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f\n",
//                                           tsince,ro[0],ro[1],ro[2],vo[0],vo[1],vo[2]);
                         }
                       else
                         {
                            jd = satrec.jdsatepoch + tsince/1440.0;
                            invjday( jd, year,mon,day,hr,min, sec );

                            fprintf(outfilee, " %16.6f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f \n",
                                           tsince*60.0,ro[0],ro[1],ro[2],vo[0],vo[1],vo[2]);

                            fprintf(outfile, " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f",
                                           tsince,ro[0],ro[1],ro[2],vo[0],vo[1],vo[2]);

                            rv2coe(ro, vo, mu, p, a, ecc, incl, node, argp, nu, m, arglat, truelon, lonper );
                            fprintf(outfile, " %14.6f %8.6f %10.5f %10.5f %10.5f %10.5f %10.5f %5i%3i%3i %2i:%2i:%9.6f\n",
                                     a, ecc, incl*rad, node*rad, argp*rad, nu*rad,
                                     m*rad,year,mon,day,hr,min,sec);
                         }
                     } // if satrec.error == 0

                  } // while propagating the orbit

fprintf(outfilee," END Ephemeris \n");
fclose (outfilee);

              } // if not eof

          } // while through the input file


  return 0;
}  // end testcpp
