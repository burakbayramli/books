%GPS Toolbox
%Version 1.0 17-Oct-1997
%
%Directory: ash_base
%
%ASH_BASE Estimation of ambiguities by sequential least squares.
%         Goad's "60-77 Algorithm" determines the final values
%         of ambiguities. Finally, estimation of vector follows.
%         During estimation ionospheric delay is set to zero.
%
%ASH_DD   Arrangement and formatting of double differenced code
%         and phase observations.
%
%B_POINT  Prepares input to the Bancroft algorithm for finding
%         a preliminary position of a receiver. The input is
%         four or more pseudoranges and the coordinates of the
%         satellites.
%
%BDATA    Reorganization of binary P-code data as resulting
%         from Z-12 receiver. Input of b-files from master and rover.
%         Typical call: bdata('b0810a94.076','b0005a94.076')
%
%EDATA    Reads a binary ephemeris file and stores it in
%         a matrix with 21 rows; column number is the number of
%         ephemerides.
%         Typical call: edata('e0810a94.076')
%
%FIND_EPH Finds the proper column in ephemeris array
%
%GET_EPH  The ephemerides contained in ephemeridesfile
%         are reshaped into a matrix with 21 rows and
%         as many columns as there are ephemerides.
%
%GET_RHO  Calculation of distance in ECEF system between
%         satellite and receiver at time tR_RAW given the
%         the pertinent ephemeris Eph.
%
%K_DD3    Kalman Filter for Estimation of Ambiguities (with I = 0)
%         Double differenced code and phase observations
%         SV is the satellite to be differenced with ref. sat. 26.
%         The choices are: 2, 9, 16, 23, 27
%
%K_DD4    Kalman Filter for Estimation of Ambiguities
%         Double differenced code and phase observations
%
%SATPOS   Calculation of X,Y,Z coordinates at time t
%         for given ephemeris eph
%
%SDATA    Reading of antenna offsets. The 2 antenna heights are saved
%         as h = ["rover"; "master"]
%         Typical call: sdata('s0810a94.076','s0005a94.076')
%
%TOGEOD   Calculate geodetic latitude, longitude, height given
%         Cartesian X,Y,Z, and reference ellipsoid values
%         semi-major axis (a) and the inverse flattening (finv).
%
%TROPO    Calculate tropospheric correction. The range correction ddr
%         in meter is to be subtracted from pseudo-ranges and carrier
%         phases.
%%%%%%%%%%% end contents.m  %%%%%%%%%%%%%%%%%%%%%%%%
