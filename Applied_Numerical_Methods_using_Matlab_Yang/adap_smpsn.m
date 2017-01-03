function  [INTf,points,err]=adap_smpsn(f,a,b,INTf,tol,varargin)
%adaptive Simpson rule
c=(a+b)/2;
INTf1= smpsns(f,a,c,1,varargin{:}); 
INTf2= smpsns(f,c,b,1,varargin{:});
INTf12= INTf1+INTf2;
err= abs(INTf12-INTf)/15; %63%/10
if isnan(err)|err<tol|tol<eps
   INTf= INTf12;
   points= [a c b];
else
   [INTf1,points1,err1]=adap_smpsn(f,a,c,INTf1,tol/2,varargin{:});
   [INTf2,points2,err2]=adap_smpsn(f,c,b,INTf2,tol/2,varargin{:});
   INTf= INTf1+INTf2;
   points= [points1 points2(2:length(points2))];
   err= err1+err2; 
end