function [newprob newutil]=sumpotID(prob,util,probvariables,decvariables,partialorder,varargin)
%SUMPOTID Return the summed probability and utility tables from an ID
% [newprob newutil]=sumpotID(prob,util,probvariables,decvariables,partialorder,<margover>)
% prob is a (set) of probability tables
% util is a (set) of utilty tables
% routine returns the marginal probability and utility tables having summed/maxed over 
% the probability variables and the decision variables
% partialoder is required, describing the summation/maximisation for all variables.
% if margover=1 then marginalise over variables -- otherwise marginalise
% over all except the specified variables.
% This routine maintains the optimal expected utility as a 
% normalised probability (conditoned on decisions) multiplied by a utility.
% Note that prob and util are assumed from the same ID and are multiplied and summed in the routine.
% See demoDecParty.m
if isempty(varargin); margover=1; else margover=varargin{1}; end
prob=multpots(prob);
% sum over the random variables only (not decision nodes):
newprob=maxsumpot(prob,[probvariables decvariables],partialorder,margover); 
newutil=maxsumpot(multpots([prob sumpots(util)]),[probvariables decvariables],partialorder,margover);
newutil=divpots(newutil,newprob);