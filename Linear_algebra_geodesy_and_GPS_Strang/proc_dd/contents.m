%GPS Toolbox
%Version 1.1 09-Dec-1997
%
%Directory: proc_dd
%
%ACCUM0   Accumulates the contribution of observations from one epoch.  The
%         result is output under the same name.
%
%ANHEADER Analyzes the header of a RINEX file and outputs the list of
%         observation types and antenna offset.  End of file is flagged 1,
%         else 0.  Likewise for the types.
%         Typical call:  anheader('pta.96o')
%
%B_POINT  Prepares input to the Bancroft algorithm for finding a
%         preliminary position of a receiver.  The input is four or more
%         pseudoranges and the coordinates of the satellites.
%
%BACKTRAC backtrack in the search tree; used in SEARCH
%
%BANCROFT Calculation of preliminary coordinates for a GPS receiver
%         based on pseudoranges to 4 or more satellites. The ECEF
%         coordinates (see function e_r_corr) are the first three elements
%         of each row of B. The fourth element of each row of B contains
%         the observed pseudorange.  Each row pertains to one satellite.
%         The pseudorange in the first row of B is used to descriminate
%         between the two possible solutions.
%         Reference:  Bancroft, S.  (1985) An Algebraic Solution of the GPS
%         Equations, IEEE Trans.  Aerosp.  and Elec.  Systems, AES-21,
%         56--59
%
%CALL_LAM Call of the LAMBDA routines for integer estimation of the GPS
%         double difference ambiguities
%
%CHECK_T  repairs over- and underflow of GPS time
%
%CHISTART computes squared distance of partially rounded float vectors to
%         the float vector in the metric of the covariance matrix.
%
%COLLECTS collects integer vectors and corresponding squared distances
%
%DOUT     spools vector to a file
%
%DOY      Calculation of day number of year.  hour is split into hr, min,
%         and sec
%
%E_R_CORR Returns rotated satellite ECEF coordinates due to Earth rotation
%         during signal travel time
%
%ENU2XYZ  Transformation of [e;n;u] vector from local to geocentric system.
%         The local system has origin at (phi, lambda)
%
%FEPOCH_0 Finds the next epoch in an opened RINEX file with identification
%         fid. From the epoch line is produced time (in seconds of week),
%         number of sv.s, and a mark about end of file.  Only observations
%         with epoch flag 0 are delt with.
%
%FIND_EPH Finds the proper column in ephemeris array
%
%FOBS_TYP Returns column i of the observation matrix which contains
%         observation type "type"
%
%FRGEOD   Subroutine to calculate Cartesian coordinates X,Y,Z given
%         geodetic coordinates latitude, longitude (east), and height above
%         reference ellipsoid along with reference ellipsoid values
%         semi-major axis a and the inverse of flattening finv.
%         The units of linear parameters h,a must agree (m,km,mi,..etc).
%         The input units of angular quantities must be in decimal degrees.
%         The output units of X,Y,Z will be the same as the units of h
%         and a.
%
%GET_EPH  The ephemerides contained in ephemeridesfile are reshaped into a
%         matrix with 21 rows and as many columns as there are ephemerides.
%         Typical call eph = get_eph('rinex_n.dat')
%
%GET_RHO  Calculation of distance in ECEF system between satellite and
%         receiver at time tR_RAW given the ephemeris Eph.
%
%GPS_TIME Conversion of Julian Day number to GPS week and Seconds of Week
%         reckoned from Saturday midnight
%
%GRABDATA Positioned in a RINEX file at a selected epoch reads observations
%         of NoSv satellites
%
%INTOUT   spools integer vector to a file
%
%JULDAY   Conversion of date as given by
%                        y ... year (four digits)
%                        m ... month
%                        d ... day
%                        h ... hour and fraction hereof
%         The conversion is only valid in the time span from March 1900 to
%              February 2100.
%         See Hofmann-Wellenhof et al., p. 41--42
%
%L_INV    computes the inverse of a lower triangular matrix
%
%LAMBDA   integer estimation with the LAMBDA method. It is first
%         described in 
%
%         Teunissen P.J.G. (1993). Least-squares estimation of the integer
%         GPS ambiguities. Invited lecture. Section IV Theory and 
%         Methodology.  General Meeting of the International Association
%         of Geodesy. Beijing, China. August 1993.
%
%         Implementational aspects of the method are well described in
%
%         Jonge P.J. de and C.C.J.M. Tiberius (1996). The LAMBDA method 
%         for integer ambiguity estimation: implementation aspects.
%         Publication of the Delft Geodetic Computing Centre, LGR-series
%         No. 12. August 1996. 49 pp.
%         On Internet:     http://www.geo.tudelft.nl/mgp/
%         under 'Precise GPS positioning' (available as PostScript file)
%
%LOCATE   For a given iprn_value we find the component number iloc for the
%         satellite in the vector of unknowns
%
%LORENTZ  Calculates the Lorentz inner product of the two 4 by 1 vectors x
%         and y
%
%LTDL     factorization of Q into L^T D L
%
%PROC_DD  Processing of double differenced GPS data as read from RINEX
%         files
%         Typical call: proc_dd('site1.96o','site2.96o','site1.nav')
%
%RE_ORDER Computation of the Z-transformation matrix. The final
%         Z-transformation is constructed from a sequence of interchanges
%         of two neighbouring ambiguities (this function) and integer
%         Gauss transformations (function ztransi) that decorrelate the
%         ambiguities.
%
%RINEXE   Reads a RINEX Navigation Message file and reformats the data into
%         a matrix with 21 rows and a column for each satellite.  The
%         matrix is stored in outputfile.
%         Typical call: rinexe('pta.96n','pta.nav')
%
%SATPOS   Calculation of X,Y,Z coordinates at time t for given ephemeris
%         eph
%
%SEARCH   finds 'MaxCan' integer vectors whose distances to the real vector
%         'a' are minimal in the metric of Q = transpose(L) D L.  Only
%         integer vectors with a distance less than sqrt(Chic) are
%         regarded.
%
%         The search for gridpoints inside the ambiguity search ellipsoid
%         is a sequential conditional adjustment upon the ambiguities.
%         The search starts by conditioning the last ambiguity a_n to an
%         integer, then a_{n-1} etc., until either
%              1. the squared norm grows too large (out of the ellipsoid)
%              2. an integer for a_1 is found: a full integer vector is
%                 encountered (a gridpoint inside the ellipsoid)
%         If 1, the search goes back to some previous (towards a_n)
%         ambiguity and considers another integer.
%
%STORES   Stores candidates and corresponding distances
%
%SUM_NORM Sums normals for double differenced GPS data
%
%TOGEOD   Subroutine to calculate geodetic coordinates latitude, longitude,
%         height given Cartesian coordinates X,Y,Z, and reference ellipsoid
%         values semi-major axis a and the inverse of flattening finv.
%         The units of linear parameters X,Y,Z,a must all agree (m, km, mi,
%         ft, etc). The output units of angular quantities will be in
%         decimal degrees (15.5 degrees not 15 deg 30 min).  The output
%         units of h will be the same as the units of X,Y,Z,a.
%
%TOPOCENT Transformation of vector dx into topocentric coordinate system
%         with origin at X.  Both parameters are 3 by 1 vectors.
%               Output:  D   vector length in units like the input
%                       Az   azimuth from north positive clockwise, degrees
%                       El   elevation angle, degrees
%
%TROPO    Calculation of tropospheric correction.  The range correction
%         ddr in m is to be subtracted from pseudo-ranges and carrier
%         phases
%               sinel    sin of elevation angle of satellite
%               hsta     height of station in km
%               p        atmospheric pressure in mb at height hp
%               tkel     surface temperature in degrees Kelvin at height
%                        htkel
%               hum      humidity in % at height hhum
%               hp       height of pressure measurement in km
%               htkel    height of temperature measurement in km
%               hhum     height of humidity measurement in km
%         Reference: Goad, C.C.  & Goodman, L.  (1974) A Modified
%         Tropospheric Refraction Correction Model.  Paper presented at 
%         the American Geophysical Union Annual Fall Meeting, San 
%         Francisco, December 12-17
%
%TROPP    exhibits useful hints for handling graphics of axes, contour
%         labels and lines.  We have chosen the tropospheric refraction
%         delay for demonstration.
%
%ZTRANSI  Updates integral Z-transform for L; only column `first' until
%         `last'. The output is the inverse of Z transpose.
%%%%%%%%%%%%%%%%% end  contents.m  %%%%%%%%%%%%%%%%%%%%
