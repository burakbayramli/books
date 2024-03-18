function varargout = fTRIM(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
        
    case 'minRows'
        varargout{1} = 4;
    case 'maxRows'
        varargout{1} = 4;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

fields = {'TRIM','TRIMID','IDMK','QINF','IDOBJ','IDCONS','RHOX','RHOY','RHOZ';
[],'WTMASS','WEIGHT','IXX','IXY','IYY','IXZ','IYZ','IZZ';
[],'TRNACC','NX','NY','NZ','PDOT','QDOT','RDOT','LOADSET';
[],'IDVAR1','VAL1','IDVAR2','VAL2','IDVAR3','VAL3','IDVAR4','VAL4'};

dataType = {'str','int','int','dec','int','int','dec','dec','dec';
[],'dec','dec','dec','dec','dec','dec','dec','dec';
[],'str','dec','dec','dec','dec','dec','dec','int';
[],'int','str','int','str','int','str','int','str'};

default = {Inf,[],[],[],Inf,Inf,[],[],[];
Inf,[],[],Inf,Inf,Inf,Inf,Inf,Inf;
Inf,[],[],[],[],Inf,[],Inf,Inf;
Inf,999,'999',999,'999',999,'999',999,'999'};

placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C);

end