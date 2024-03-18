function varargout = fCAERO7(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
        
    case 'minRows'
        varargout{1} = 3;
    case 'maxRows'
        varargout{1} = 3;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

fields = {'CAERO7','WID','LABEL','ACOORD','NSPAN','NCHORD','LSPAN','ZTAIC','PAFOIL7';
[],'XRL','YRL','ZRL','RCH','LRCHD','ATTCHR','ACORDR',[];
[],'XTL','YTL','ZTL','TCH','LTCHD','ATTCHT','ACORDT',[]};

dataType = {'str','int','str','int','int','int','int','int','int';
    [],'dec','dec','dec','dec','int','int','int',[];
    [],'dec','dec','dec','dec','int','int','int',[]};

default = {Inf,[],[],0,[],[],0.0,Inf,[];
Inf,[],[],[],[],0,Inf,Inf,Inf;
Inf,[],[],[],[],0,Inf,Inf,Inf};

placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C) ;
end