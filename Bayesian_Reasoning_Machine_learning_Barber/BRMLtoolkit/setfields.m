function structure=setfields(structure,varargin)
%SETFIELDS sets the fields of a structure. Does not overwrite existing field values
% example structure = setfields(structure,'name','david','number',1)
% Use structure = setfields([],'name','david','number',1) to create a new structure
c=1; 
for i=1:floor(length(varargin)/2)
    if ~isfield(structure,varargin{c})
        f=varargin{c}; v=varargin{c+1};
        %structure=setfield(opts,varargin{c},varargin{c+1});
        structure.(f)=v;
    end
    c=c+2;
end