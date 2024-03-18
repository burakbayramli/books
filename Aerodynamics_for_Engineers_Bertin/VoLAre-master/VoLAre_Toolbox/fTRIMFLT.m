function varargout = fTRIMFLT(varargin)
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

fields =   {'TRIMFLT','IDFLT','INPCFD','ALPHA','BETA','PRATE','QRATE','RRATE','THERMO'
            [],'LABEl1','VAL1','LABEL2','VAL2','LABEL3','VAL3','LABEL4','VAL4'};
dataType = {'str'    ,'int'  ,'int'   ,'dec'  ,'dec' ,'dec'  ,'dec'  ,'dec'  ,'int'
            [],'str','dec','str','dec',[],[],[],[]};
default =  {Inf      ,[]     ,Inf     ,[]     ,[]    ,Inf    ,Inf    ,Inf    ,Inf;
            Inf      ,[]     ,[]      ,Inf    ,Inf   ,Inf    ,Inf    ,Inf    ,Inf};
placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C);
end