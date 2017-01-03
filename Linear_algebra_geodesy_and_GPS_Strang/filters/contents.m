%GPS Toolbox
%Version 1.0 17-Oct-1997
%
%Directory: filters
%
%AUTOCORR Calculation of autocorrelation function for a given sequence of
%         observations a
%
%B_CLOCK  Reading of binary P-code data as resulting from Z-12 receiver.
%         Input of b-file from master.   
%         Typical call: bdata('b0810a94.076')
%
%B_ROW    Bayes update, one measurement per call Observation covariance R
%
%COMPTIME Reads the receiver clock offset from a binary Ashtech observation
%         file and plots the offset.
%
%FEPOCH_0 Finds the next epoch in an opened RINEX file with identification
%         fid. From the epoch line is produced time (in seconds of week),
%         number of sv.s, and a mark about end of file.  Only observations
%         with epoch flag 0 are delt with.
%
%FIG15_6  Script for plotting receiver clock offsets for Turbo-SII and Z-12
%         receivers.  Produces Figure 15.6
%
%FIG16_5  Script for Figure 16.5
%
%FIXED2   Solution to Example 10.1.  Solution to Example 12.4.   Solution 
%         to Example 12.7 Solution as descibed by equation (12.64). 
%         Solution obtained by filtering.
%
%FIXING1  Filter version of Examples 12.1, 12.2, and 12.3 Shows the impact on
%         introducing a constraint with zero variance for the observation
%
%FIXING2  Filter version of Examples 12.4 and 12.7.  Shows the impact on
%         introducing constraints as observations with zero variance
%
%GMPROC   Plots the autocorrelation and power spectral functions of a
%         Gauss-Markov process
%
%GRABDATA Positioned in a RINEX file at a selected epoch reads
%         observations of NoSv satellites
%
%INCORREC Random walk incorrectly modeled as a random constant.
%
%K_CLOCK  Prepares input to the Kalman algorithm for finding receiver 
%         clock offset.  The inputs are receiver coordinates calculated by
%         a call of b_point (Bancroft algorithm), pseudoranges, prn's, and
%         measurement received time.
%
%K_ROW    Kalman update, one measurement per call. Observation variance:
%         var
%
%K_SIMUL  Plots characteristics of a Kalman Filter and covariance matrices.
%
%K_UD     Same as K_ROW
%
%K_UPDATE Kalman update, one measurement per call Observation covariance R
%
%K_UPDATX Kalman update, one measurement per call
%                   Allows for system covariance Q
%                   Allows for observation covariance R
%
%KALCLOCK Estimates receiver clock offset and position as read from the
%         RINEX ofile.  A RINEX navigation navfile is also needed.
%         Extended filter is used, if extended_filter = 1
%
%KUD      Same as K_ROW
%
%MODEL    Receiver clock offset OS from kalclock is modeled; first by a
%         linear, next by a quadratic approximation.  The model is
%         subtrated from OS.  The autocorrelation function for the
%         residuals is plotted.
%
%MODEL_G  The data obs are modeled; obs is assumed to be a row vector!
%         first by a linear, next by a quadratic approximation.  The model
%         is subtrated from obs leaving residuals the autocorrelation
%         function of which is plotted
%
%OFFSET   Plots the difference between batch processing, Kalman filter and
%         extended Kalman filter
%
%ONE_WAY  Evaluation of one-way data. Observations from Z12 receiver taken at
%         master site -810 and rover site -005 on March 17, 1994
%
%ONE_WAYD Brute way to create files with one_way data
%
%ONEWAY_I Evaluation of one-way data.  Estimation of ambiguities followed
%         by an estimation of ionospheric delay Finally we plot I for
%         one-ways as measured at master and rover receivers, plot of
%         single differences and plot of double differences.
%
%OUTLIER  Detection of clock reset, 1 ms, of Ashtech receiver
%
%REC_LSQ  Recursive Least Squares A is the coefficient matrix, b the
%         observations and Sigma a vector containing the diagonal entries
%         of the covariance matrix for the problem.  For increasing i we
%         include one more observation
%
%RECCLOCK Estimation of receiver clock offset and position through batch
%         processing.  Data are read from the RINEX ofile.  The processing
%         is iterated three times due to non-linearity in the position
%         determination
%
%RTS      Calculation of filtered and smoothed estimates of covariances.
%         The observations and the state vector is of no concern in this
%         example. Covariance of system noise Q Covariance of observation
%         noise R.
%         Numerical examples from Rauch, H.  E., F.  Tung, and C.  T.
%         Striebel (1965) Maximum Likelihood Estimates of Linear Dynamic
%         Systems.  American Institute of Aeronautics ans Astronautics
%         Journal Vol. 3, pp. 1445--1450
%
%SMOOTHER Scalar steady model.   Forward filtering and smoothing of an
%         observation series b.  System noise covariance Q, observation
%         noise covariance R.
%
%WC       Filter implementation of impact of changing weights Script for
%         Example 11.12
%
%WHITENOI Plots the autocorrelation and power spectral functions of a
%         white noise process
%%%%%%%%%%%%%%%% end contents.m  %%%%%%%%%%%%%%%%%%%%%
