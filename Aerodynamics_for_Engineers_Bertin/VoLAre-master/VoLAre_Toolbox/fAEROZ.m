function varargout = fAEROZ(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
        
    case 'minRows'
        varargout{1} = 2;
    case 'maxRows'
        varargout{1} = 2;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

fields = {'AEROZ','ACSID','XZSYM','FLIP','FMMUNIT','FMLUNIT','REFC','REFB','REFS';
          [],'REFX','REFY','REFZ',[],[],[],[],[]};

dataType = {'str','int','str','int','int','int','int','int','int';
    [],'dec','dec','dec','dec','int','int','int',[];
    [],'dec','dec','dec','dec','int','int','int',[]};

default = {Inf,Inf,'YES',Inf,Inf,Inf,[],[],[];
Inf,[],[],[],Inf,Inf,Inf,Inf,Inf};



placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C) ;
% check inputs
if row == 1
    if isfield(placeHolder,'ACSID')
        error('Nondefault (not blank) ACSID on AEROZ not supported.')
    end
    switch placeHolder.XZSYM
        case 'YES'
        case 'NO'
        otherwise
            error('')
    end
end

end