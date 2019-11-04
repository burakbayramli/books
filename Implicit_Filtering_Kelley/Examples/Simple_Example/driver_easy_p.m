function [x,histout]=driver_easy_p;
% DRIVER_EASY_P
% Minimize f_easy_p with imfil.m using the parallel option.
%
% function [x,histout,complete_history]=driver_easy_p;
%

%
% Set the bounds, budget, and initial iterate.
bounds=[-1, 1; -1 1];
budget=40;
x0=[.5,.5]';
%
% Turn the parallel option on.
%
options=imfil_optset('parallel',1);
%
% Call imfil.
%
[x,histout,complete_history]=imfil(x0,'f_easy_p',budget,bounds,options);
%
% Use the first three columns of the histout array to examine the
% progress of the iteration.
%
histout(:,1:2)
