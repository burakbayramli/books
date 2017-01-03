%  Script for calculating an absolute position
%  of a receiver. Input is 4 or more pseudoranges
%  for the Bancroft algorithm, a list of
%  satellites and received time

%Kai Borre 11-24-96
%Copyright (c) by Kai Borre
%$Revison: 1.0 $  $Date: 1997/09/26  $

format bank
%pr = [ 22932343.11;	      	    % Pseudoranges
%  	  21463430.59;
%	     21438529.96;
%	     20470261.34;
%	     21327488.52];
%time = 574205;
%sv = [31; 18; 28; 29; 22];

pr = [ 21118201.853;		          % Pseudoranges
       22530017.037;
       23657091.196;
       20951074.039;
       20163869.804;
       24259531.044];
time = 165600;
sv = [23; 9; 5; 1; 21; 17];
pos = b_point(pr,sv,time)	       % Call of Bayes filter
pause(1)
pos = k_point(pos,pr,sv,time);    % Call of Kalman filter
%%%%%%%%%%%%%%%% end abs_pos.m %%%%%%%%%%%%%%%%%%%%%%%
