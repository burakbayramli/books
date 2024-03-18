function varargout = fPAFOIL7(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
    case 'minRows'
        varargout{1} = 1;
    case 'maxRows'
        varargout{1} = 1;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

fields = {'PAFOIL7','ID','ITAX','ITHR','ICAMR','RADR','ITHT','ICAMT','RADT'};
dataType = {'str','int','int','int','dec','int','int','int','dec'};
default = {Inf,0,0,0,0.0,0,0,0,0.0};

placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C) ;
end