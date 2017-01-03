function [newpot newvarinf]=changevar(pot,origvar,newvar,varargin)
%CHANGEVAR Change variable names in a potential
% newpot=changevar(pot,origvar,newvar,<varinf>)
% replace origvar(i) with newvar(i)
% See demoChangeVar.m
newpot=pot;
for p=1:length(pot)
	[k l]=ismember(pot(p).variables,origvar);
	newpot(p).variables=newvar(l);	
end
if ~isempty(varargin)
    oldvarinf=varargin{1};
    newvarinf(newvar)=oldvarinf(origvar);
end
