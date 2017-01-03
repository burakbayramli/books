function sp_legend(label,vector,location)
%sp_legend(label,vector,location)
%
%This function creates a legend from a label and a vector.
%The label can be any string.
%The vector can be any colum or row vector.
%location can be N,NE, etc., or left blank for 'best'
%
%The following is an example:
%
%label: 'x=';   vector: [0.1, 3, -2]; location: 'SW'
%
%legend: x=0.1
%        x=3.0
%        x=2.0
%

v=vector(:);
Lv=length(v);
Ll=length(label);

%label array
A=char(ones(Lv,1)*label);

%legend
if v==round(v)
    legstr=[A,int2str(v)];
else
    legstr=[A,num2str(v)];
end
if nargin<3
    legend(legstr,'location','best')
else
    legend(legstr,'location',location)
end