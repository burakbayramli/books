%GPS Toolbox
%Version 1.0 17-Oct-1997
%
%Directory:  bancroft
%
%ABS_POS  Script for calculating an absolute position of a receiver.
%         Input is 4 or more pseudoranges for the Bancroft algorithm,
%         a list of satellites and received time
%
%B_POINT  Prepares input to the Bancroft algorithm for finding a
%         preliminary position of a receiver. The input is four or more
%         pseudoranges and the coordinates of the satellites.
%
%BANCROFT Calculation of preliminary coordinates for a GPS receiver
%         based on pseudoranges to 4 or more satellites. The ECEF
%         coordinates (see function e_r_corr) are the first three
%         elements of each row of B. The fourth element of each
%         row of B contains the observed pseudorange. Each row pertains
%         to one satellite. The pseudorange in the first row of B is
%         used to discriminate between the two possible solutions.
%
%CHECK_T  repairs over- and underflow of GPS time
%
%E_R_CORR Returns rotated satellite ECEF coordinates due to Earth rotation
%         during signal travel time
%
%FIND_EPH Finds the proper column in ephemeris array
%
%GET_EPH  The ephemerides contained in ephemeridesfile are reshaped into a
%         matrix with 21 rows and as many columns as there are ephemerides.
%
%K_POINT  Prepares input to the Kalman algorithm for finding the final
%         position of a receiver. The inputs are preliminary station
%         coordinates calculated by a call of b_point (Bancroft algorithm),
%         pseudoranges, prn's, and measurement received time.
%
%KLEUS    Explicit method for computing preliminary receiver coordinates
%         from four pseudoranges and ECEF coordinates of four satellites
%
%LORENTZ  Calculates the Lorentz inner product of the two 4 by 1 vectors
%         x and y
%
%NORMALS  Accumulates the contribution of one observation equation and
%         adds it to the coefficient matrix AtA and the right side AtY.
%         The accumulated result is outputted under the same name.
%
%SATPOS   Calculation of X,Y,Z coordinates at time for given ephemeris
%         eph
%
%TOGEOD   Subroutine to calculate geodetic coordinates latitude, 
%         longitude, height given Cartesian coordinates X,Y,Z, and 
%         reference ellipsoid values semi-major axis a and the inverse 
%         of flattening finv.
%
%TOPOCENT Transformation of vector dx into topocentric coordinate
%         system with origin at X. Both vectors are 3 by 1.
%         Output: D     vector length in units like the input
%                Az     azimuth from north positive clockwise, degrees
%                El     elevation angle, degrees
%
%TROPO    Calculation of tropospheric correction. The range correction ddr
%         in meter is to be subtracted from pseudo-ranges and carrier
%         phases
%%%%%%%%%%%%%% end contents.m %%%%%%%%%%%%%%%%%%%%%%%%%%%%
