function [fval,ifail,icost]=hceval(v,working_directory,pid)
% HCEVAL
% Function evaluation for the hydraulic capture problem.
% Discontinuous objective because the capital cost drops when
% a well shuts down.
%
% function [fval,ifail,icost]=hceval(x,working_directory,pid)
%
% If the pid argument is missing, we're running in serial.
%
if nargin == 2 
   pid = 0;
end
%output_method='binary';
output_method='ascii';
%
% Build the data structure and the configuration files.
%
hc_data=mk_data(working_directory,output_method,pid);
%
% Extract parameters from hc_data.
%
working_directory=hc_data.working_directory;
wellfile=hc_data.wellfile;
modflowcall=hc_data.modflowcall;
minrate=hc_data.minrate;
headfile=hc_data.headfile;
%
% Get the well locations and the pumping rates.
%
[x,y,wellrate]=vmap(v,minrate);
%
% Test the cheap constraints. These are the linear constraint on 
% the pumping rates and the no-overlap condition on the well locations.
%
ifail = cheap_test(x,y,wellrate,minrate);
%
%
if ifail == 0
%
% Write the well locations and pumping rates to the right file.
% Call modflow to read the well file, compute the heads, and write them
% to the head file. Read the head file.
%
   writewel(v,wellfile);
   system(modflowcall);
   head = readhead(headfile,output_method);
%
% Evaluate the objective function.
%
   icost=1;
   op_cost=cost_op(x,y,wellrate,head);
   cap_cost=cost_cap(wellrate,minrate);
   fval=op_cost + cap_cost;
   hflag = head_constraint(x,y,wellrate,head);
   if hflag==1
      ifail=1;
      fval = NaN;
%disp('head constraint violation')
   end
else
   icost=0;
   fval = NaN;
end

function [x,y,wellrate]=vmap(v,minrate)
%
% Convert v to useful variables.
%
x=zeros(4,1); y=zeros(4,1);
x=round((1000-v(2:2:8))/10);
y=round(v(1:2:7)/10);
wellrate=v(9:12);
wellrate=wellrate.*(abs(wellrate) >= minrate);

function op_cost=cost_op(x,y,wellrate,head)
%
% Compute the operating cost.
%
z=10*ones(4,1);
%
% too many constants
%
c2 = 0.00029;
c3 = 0.000145;
ZGS = 30.0;
%
TF=1.56d8;
op_cost=0;
for i=1:4
    if wellrate(i) < 0
%
% The well is extracting.
%
       op_cost = op_cost+c2*wellrate(i)*(head(x(i),y(i),z(i))-ZGS);
    else
%
% The well is injecting.
%
       op_cost = op_cost+c3*wellrate(i);
    end
end
%
% Scale by final time.
%
op_cost = op_cost*TF;

function cap_cost=cost_cap(wellrate,minrate)
%
% Compute the capital costs.
%
% List of constants.
%
B0 = 0.3;
B1 = 0.45;
B2 = 0.64;
C0 = 5500.0;
C1 = 5750.0;
HMIN = 10.0;
ZGS = 30.0;
temp2 = (ZGS - HMIN)^B2;
depth=ZGS*(abs(wellrate) >= minrate);
qm=zeros(4,1);
for i=1:4
    qm(i)=.0064;
    if wellrate(i) < 0
       qm(i) = -qm(i);
    end
end
%
cap_cost = 0;
%
% Drilling cost.
%
for i=1:4
    cap_cost = cap_cost + C0*depth(i)^B0;
end
%
% Add pump cost for an extracting well.
%
for i=1:4
    if(wellrate(i) < 0)
       cap_cost=cap_cost+C1*(abs(qm(i))^B1)*temp2;
    end
end


function ifail = cheap_test(x,y,wellrate,minrate);
%
% Test the cheap constraints and complain if they are violated.
%
ifail=0;
%
% Make sure no two wells are in same place.
%
wdist=welldist(x,y);
lin_flag = pump_test(wellrate,minrate);
if wdist == 0
   ifail = 1;
%disp('overlap wells')
end
%
% Test the linear constraints.
%
if lin_flag == 0
   ifail = 1;
%disp('linear constraint failure')
end
%

function wdist=welldist(x,y)
%
% Test for no-overlap constraint be evaluating the minimum distance 
% between wells. Zero is bad.
%
spots = [x y]';
[mw,nw]=size(spots);
wdist=1000;
for i=1:nw
    for j=i+1:nw
       tdist=norm(spots(:,i)-spots(:,j),inf);
       wdist=min(wdist,tdist);
    end
end   

function lin_flag = pump_test(wellrate,minrate)
%
% Linear constraint on the pumping rates.
%
qtmax=-0.031998;
qt=sum(wellrate);
lin_flag=(qtmax < qt);


function writewel(v,wellfile)
%
% WRITEWEL builds the HC.wel file in your favorite directory
%
minrate=1.d-6;
z=10*ones(4,1); x=zeros(4,1); y=zeros(4,1);
x=round((1000-v(2:2:8))/10);
y=round(v(1:2:7)/10);
wellrate=v(9:12);
wellrate=wellrate.*(abs(wellrate) >= minrate);
wfid=fopen(wellfile,'w');
fprintf(wfid,'%12d%12d\n',4,0);
fprintf(wfid,'%12d\n',4);
for i=1:4
  fprintf(wfid,'%3d%3d%3d%12.7f\n', z(i), x(i), y(i), wellrate(i));
end
fclose(wfid);

function head = readhead(filename,output_method)
head=zeros(100,100,10);
switch output_method
  case 'ascii'
     head=aread(filename);
  case 'binary'
     head=bread(filename);
end

function head = aread(filename)
head=zeros(100,100,10);
hcraw=load(filename);
for iz = 1:10
   irl=(iz-1)*100+1;
   irh=iz*100;
   head(:,:,iz)=hcraw(irl:irh,:);
end

function head=bread(headfile)
fid=fopen(headfile,'rb');
head=zeros(100,100,10);
for i=1:10
   xl=10000*(i-1) + 1;
   xh=10000*i;
   hr1=fread(fid,1,'int32');
   name=setstr(fread(fid,hr1,'uchar'))';
   hr1=fread(fid,1,'int32');
   hr2=fread(fid,1,'int32');
   head(:,:,i)=fread(fid,[100,100],'float32')';
   hr2=fread(fid,1,'int32');
end
fclose(fid);

function hflag = head_constraint(x,y,wellrate,head)
z=10*ones(4,1);
%
% Constants, many constants.
%
D = 0.0001;
ZGS = 30.0;
hmin = 10.0;
hmax = ZGS;
%
% hflag = 1 means one of the head constraints has been violated.
%
hflag=0;
%
% Are the heads within the bounds?
%
for i=1:4
    if abs(wellrate) > 0
       test_min=head(x(i),y(i),z(i))-hmin;
       if test_min < 0
          hflag = 1; 
       end
       test_max = hmax-head(x(i),y(i),z(i));
       if test_max < 0
          hflag = 1; 
       end
    end
end
if hflag == 1
%disp('heads out of bounds');
    return
end
%
% Do the head gradients point in the right direction?
%
grad(1) = head(27,18,4)-head(28,18,4);
grad(2) = head(23,24,4)-head(24,24,4);
grad(3) = head(26,33,4)-head(27,33,4);
grad(4) = head(35,39,4)-head(35,38,4);
grad(5) = head(46,38,4)-head(46,37,4);
gradmin=min(grad);
if gradmin < D
    hflag=1;
%disp('capture constraint failure');
end

function hc_data=mk_data(working_directory,output_method,pid)
% MK_DATA
% Build the modflow data structure for heval. This function
% winds up creating three files for each element of a parfor loop.
% This is one reason for having the working directory be different from
% the place where your .m files live.
%
% Do not mess with this function. You will only break stuff.
%
minrate=1.d-6;
iv = sprintf('%d',pid);
wellfile=[working_directory,'/HC.wel',iv];
headfile=[working_directory,'/HC.hed',iv];
mfnname=[working_directory,'/Config',iv,'.mfn'];
modflowcall=...
    ['echo ',mfnname,' | ','mf2k > /dev/null'];
hc_data=struct('working_directory',working_directory,'wellfile',wellfile,...
              'modflowcall',modflowcall,'headfile',headfile,...
              'minrate',minrate);

%
% Create the config file if it's not there already.
%
makemfn(hc_data,output_method,pid);

function makemfn(hc_data,output_method,pid)
% MAKEMFN
% Make the modflow config file. Do not touch this unless you are a
% modflow expert. 
% 
%
iv=sprintf('%d',pid);
workdir=hc_data.working_directory;
mfnname=['hc_tmp/Config',iv,'.mfn'];
wellmfn=['WEL           9 ',hc_data.wellfile];
switch output_method
   case 'ascii'
        headmfn=['DATA         30 ',hc_data.headfile];
        ocmfn  =['OC           15 DATA/B3.oc'];
   case 'binary'
        headmfn=['DATA(BINARY) 30 ',hc_data.headfile];
        ocmfn  =['OC           15 DATA/XX.oc'];
   otherwise
        error(' Output method is either ascii or binary.');
   end
isthere=exist(mfnname);
%
% Create the modflow config file if it's not there already.
%
if isthere == 0
   cid=fopen(mfnname,'w');
   fprintf(cid,'%s\n','LIST          2 /dev/null');
   fprintf(cid,'%s\n',headmfn);
   fprintf(cid,'%s\n','DIS          19 DATA/A3.dis');
   fprintf(cid,'%s\n','BAS6          3 DATA/A3.ba6');
   fprintf(cid,'%s\n','LPF           4 DATA/A3.lpf');
   fprintf(cid,'%s\n',ocmfn);
   fprintf(cid,'%s\n','RCH          16 DATA/A3.rch');
   fprintf(cid,'%s\n',wellmfn');
   fprintf(cid,'%s\n','CHD          13 DATA/A3.chd');
   fprintf(cid,'%s\n','PCG          14 DATA/A3.pcg');
   fclose(cid);
end


