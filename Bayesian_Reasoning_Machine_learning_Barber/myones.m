function out=myones(numstates,varargin)
%MYONES sames as ones(x), but if x is a scalar, interprets as ones([x 1])
discretetable=1;
if nargin==2
    if ~isempty(varargin{1})
        out=feval(['unity' varargin{1}],numstates);
        out.type=varargin{1};
        discretetable=0;
    end
end
if discretetable
    if length(numstates)>1
        out=ones(numstates);
    else
        out=ones([numstates 1]);
    end
end