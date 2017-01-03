function  [INTf,points,err]=adapt_smpsn(f,a,b,tol,varargin)
%Apply adaptive Simpson rule
INTf= smpsns(f,a,b,1,varargin{:});
[INTf,points,err]=adap_smpsn(f,a,b,INTf,tol,varargin{:});
