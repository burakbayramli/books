function [maxval maxstate]=maxNarray(x,maxover,N,varargin)
% maxNarray Find the N highest values and states by maximising an array over a set of dimensions
% [maxval maxstate]=maxNarray(x,maxover,N,{nstates})
% find the N highest values and corresponding states that maximize the multidimensional array x
% over the dimensions in maxover
% If called as maxarray(x,maxover,N,nstates), x can be in linear index form, and dimvars
% decribes the dimensions of the variables in the linear index 
%
% Example:
% x=randn(3,2,2,4); [maxval maxstate]=maxNarray(x,[4 2],5)
% This means that we wish to maximise over the 2nd and 4th dimensions of x.  
% maxval and maxstate are then 5 dimensional cells with
% maxval{1} and maxstate{1} containing the highest values and corresponding
% states. Then maxval{2}, maxstate{2} are the next highest, etc.
% In this case, maxval{1} is a 6x1 dimensional vector since, after
% maximising over the 2nd and 4th dimensions of x, we are left with the
% 1st and 3rd dimensions still, which contain 3x2=6 states. The joint
% states of all variables corresponding to the maximal values are in
% maxstate{1}.
% See also maxarray.m
if isempty(varargin)
	dimvars=size(x);
else
	dimvars=varargin{1};
	if length(dimvars)>1; x=reshape(x,dimvars); end
end
nvars=length(dimvars); left=setdiff(1:nvars,maxover);
dimleft = prod(dimvars(left)); dimover =prod(dimvars(maxover));
if length(dimvars)>1; xx=permute(x,[left maxover]); else xx=x; end
s = reshape(xx,dimleft,dimover);
[aa bb]=sort(s,2,'descend');
L=size(aa,2);
if L<N % there aren't enough states -- pad with zeros
	aaa=aa; bbb=bb;
	aa=zeros(size(aa,1),N); aa(:,1:L)=aaa;
	bb=zeros(size(aa,1),N); bb(:,1:L)=bbb;
end
for i=1:N
	c{i} = [ (1:dimleft)' bb(:,i)];
	maxstate{i} = ind2subv(dimvars([left maxover]),subv2ind([dimleft,dimover],c{i})); maxval{i} = aa(:,i);
	[d p]=sort([left maxover]); maxstate{i}=maxstate{i}(:,p);
end