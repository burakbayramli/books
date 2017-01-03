function [newpot A] = uniquepots(pot,varargin)
%UNIQUEPOTS Eliminate redundant potentials (those contained wholly within another) by multiplying redundant potentials
% [newpot A]= uniquepots(pot,<tables>)
% if tables=0 then just merge the variables
% The matrix has A(j,i)=1 if pot(i) is contained within pot(j)
tables=1; if nargin==2; tables=varargin{1}; end
% Find a tree of cliques by identifying a single parent clique j that contains clique i.
C=length(pot); r=zeros(1,C);
for i=1:C; r(i)=isempty(pot(i).variables); end
pot=pot(~r);C=length(pot);
A=sparse(C,C);
for i=1:C
    j=1;
    while j<=C
        A(j,i)=ischildpot(pot,i,j);
        if A(j,i);break;end
        j=j+1;
    end
end
newpot=pot;
% Now merge from bottom up
[tree elimset sched]=istree(A);
remove=zeros(1,C); 
if tables
    for s=1:size(sched,1)
        ch=sched(s,1); pa=sched(s,2);
        if ch~=pa
            newpot(pa)=multpots(newpot([pa ch]));
            remove(ch)=1;
        end
    end
else
    for s=1:size(sched,1)
        ch=sched(s,2); pa=sched(s,1);
        if ch~=pa
            newpot(pa).variables=union(newpot(ch).variables,newpot(pa).variables);
            remove(ch)=1;
        end
    end
end
newpot=newpot(remove==0);
function m = ischildpot(pot,i,j)
if i==j; m=0; return; end
m=all(ismember(pot(i).variables,pot(j).variables));