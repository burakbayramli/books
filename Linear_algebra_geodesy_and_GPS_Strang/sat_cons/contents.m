%GPS Toolbox
%Version 1.0 17-Oct-1997
%
%Directory: sat_cons
%
%FRGEOD   Subroutine to calculate Cartesian coordinates X,Y,Z given
%         geodetic coordinates latitude, longitude (east), and height
%         above reference ellipsoid along with reference ellipsoid
%         values semi-major axis a and the inverse of flattening finv.
%         The units of linear parameters h, a must agree (m, km, mi, etc).
%         The input units of angular quantities must be in decimal degrees.
%         The output units of X,Y,Z will be the same as the units of h and
%         a.
%
%GEOID    Script for Figure 14.6: The WGS84 geoid.
%         The data in heights can be found in Table 6.1 in Department of
%         Defense World Geodetic System 1984 Its Definition and
%         Relationships with Local Geodetic Systems.  DMA Technical Report,
%         Second Edition, 1 September 1991.
%         We have changed sign of heights(12,8) to get a contour map
%         similar to that officially published.
%
%RECPOS   Least-squares searching for receiver position.  Given 4 or more
%         pseudoranges and ephemerides.  Zoom on the plot to detect the
%         search pattern! Idea to the script originates from Clyde C. Goad
%
%
%RINEXE   Reads a RINEX Navigation Message file and reformats the data
%         into a matrix with 21 rows and a column for each satellite.
%         The matrix is stored in outputfile.
%         Typical call: rinexe('pta.96n','pta.nav')
%
%SATCONST Script for drawing GPS constellation in INERTIAL and ECEF frames
%
%SATPOS   Calculation of X,Y,Z coordinates at time t for given ephemeris
%         eph
%
%SATPOSIN Calculation of X,Y,Z coordinates in an INERTIAL reference frame
%         at time t for given ephemeris eph
%%%%%%%%%%%%%%% end contents.m  %%%%%%%%%%%%%%%%%%%%%%
