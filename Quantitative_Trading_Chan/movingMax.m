function [mvmax] = movingMax(x, T)
% [mvmax]=movingMax(x, T). create moving maximum series over T days. mvavg
% has T-1 NaN in beginning. Ignore over days with NaN.
% File movingMax.m
% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

mvmax = smartMovingMin(-x, T);

mvmax = -mvmax;