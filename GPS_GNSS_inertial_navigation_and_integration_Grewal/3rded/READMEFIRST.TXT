This CD contains MATLAB software to accompany

Grewal, Andrews, and Bartone,
Global Navigation Satellite Systems, Inertial Navigation, and Integration,
3rd Edition,
published by John Wiley & Sons, 2013.

NOTICES

This software is intended for demonstration and instructional purposes only. The authors and publishers make no warranty of any kind, expressed or implied, that these routines meet any standards of mercantibility for commercial purposes. These routines should not be used as-is for any purpose or application that may result in loss or injury, and the publishers and authors shall not be liable in any event for incidental or consequential damages in connection with or arising out of the furnishing, performance, or use of these programs.

MATLAB is a registered tradmark of
The Math Works, Inc.
3 Apple Hill Drive
Natick, MA 01760 USA

SYSTEM REQUIREMENTS

These MATLAB scripts are designed for MATLAB environments. Information on MATLAB can be obtained from The Math Works, Inc. (http://www.mathworks.com/)

Software reference in the textbook include (by Chapter):

CHAPTER 1


CHAPTER 2


CHAPTER 3

fBortz.m	Computes the continuous linear dynamic model coefficient matrix  FB = df/drho for the Bortz attitude model
Euler2CTMat.m  Converts from roll-pitch-yaw Euler angles to coordinate transformation matrix from North-East-Down coordinates to body-fixed Roll-Pitch-Yaw axis coordinates.
CTMat2Euler.m
RotVec2CTMat.m
CTMat2RotVec.m
RotVec2Quat.m
Quat2RotVec.m
Quat2CTMat.m
CTMat2Quat.m

CHAPTER 4


CHAPTER 5


CHAPTER 6


CHAPTER 7

Klobuchar_fix.m 
Klobuchar(PRN) 
Iono_delay(PRN#) 

CHAPTER 8


CHAPTER 9


CHAPTER 10

osc_ekf.m
Damp2eval.m
Damp2Params.m
DAMP3Params.m
Fig8Mod1D.m
GPSTrackingDemo.m
Fig8TrackDemo.m
GPSTrackingDemo.m
SchmidtKalmanTest.m
shootout.m

CHAPTER 11

Fcore9.m
Fcore10Test1.m
F10CEPrate.m
DampingTest10part1.m
Fcore7.m
Fcore7Test1.m
Fcore7Test2.m
Fcore7Test3.m
Fcore7Test4.m
Fcore7Test5.m
Fcore7Test6.m
Fcore7Test7.m
CEPvsSensorNoise.m
CEPvsAccComp.m

CHAPTER 12

ClockModel2
Big8SimENU.m
GNSSonly.m
Big8TrackStats.m
INS7only0.m
GNSSINSInt0.m

If you have any problems or find errors, please contact gusandrews@roadrunner.com