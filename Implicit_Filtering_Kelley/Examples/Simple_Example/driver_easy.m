function [x,histout]=driver_easy;
% DRIVER_EASY
% Minimize f_easy with imfil.m
%
% function [x,histout,complete_history]=driver_easy;
%

%
% Set the bounds, budget, and initial iterate.
bounds=[-1, 1; -1 1];
budget=40;
x0=[.5,.5]';
%
% Call imfil.
%
[x,histout]=imfil(x0,'f_easy',budget,bounds);
%
% Use the first two columns of the histout array to examine the
% progress of the iteration.
%
%histout(:,1:2)
histout(:,1:5)
