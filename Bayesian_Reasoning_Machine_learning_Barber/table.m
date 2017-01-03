function p=table(pot,varargin)
%TABLE Return the potential table
% p=table(pot,<lidx>)
% if lidx=1, the linear index array is returned
lidx=0; if ~isempty(varargin); lidx=varargin{1}; end
p = pot.table;
if lidx; p=p(:); end
end