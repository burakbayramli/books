function [t100,t500,cost100,cost500,sol100,sol500]=LRGV_driver(method)
% LRGV_DRIVER
% Use imfil to solve the water resources case study.
%
if nargin == 0
   method='bfgs';
end
%
% Comment out the calls to matlabpool if you're running this
% in only serial mode.
%
pool_state = matlabpool('size');
if pool_state > 0
   matlabpool close;
end
%
% Put randstream at its initial setting. You'll get reproducible,
% but slow, results in serial mode, but not with matlabpool turned on.
%
reset(RandStream.getDefaultStream);
%
% Set up the parameters, bounds, initial iterate for the solve.
%
NumberSims = 500; 
RELmin=.995;        
CVARmax=1.1;     
iRo = 1500000; 
ifri = 0.3; 
LRGV_str=struct('NumberSims',NumberSims,'RELmin',RELmin,...
        'CVARmax',CVARmax,'ifri',ifri,'iRo',iRo);
%
% 
%
budget = 200;
%
% This is the initial iterate from section 10.3
%
x0=[40000,10000,1.1,1.3,.85,1.10]';
%
% Here are some other initial iterates that give different results.
%
% x0=[40000,10000,1,1.1,.8,1.0]';
% x0=[40000,0,1.0,1.5,.8,1.0]';
bounds = [20000 40000; 0 10000; .7 2.2; .7 2.2; .7 2.2; .7 2.2];
options=imfil_optset('parallel',1,'scaledepth',10,'quasi',method,...
                     'add_new_directions',@lin_const);
%options=imfil_optset('parallel',1,'scaledepth',10,'quasi',method);
t500=zeros(2,1);
t100=zeros(2,1);
sol100=zeros(6,2);
sol500=zeros(6,2);
cost100=zeros(2,1);
cost500=zeros(2,1);
for ip=1:2
   tic
   [sol500(:,ip),histout500]...
          =imfil(x0,@LRGV_Parallel,budget,bounds,options,LRGV_str);
   t500(ip)=toc;
   reset(RandStream.getDefaultStream);
   LRGV_str.NumberSims=100;
   tic
   [sol100(:,ip),histout100]...
         =imfil(x0,@LRGV_Parallel,budget,bounds,options,LRGV_str);
   t100(ip)=toc;
   reset(RandStream.getDefaultStream);
   LRGV_str.NumberSims=100;
%
%
   figure(ip);
   p2=subplot(1,1,1);
   plot(histout500(:,1),histout500(:,2),'-',...
        histout100(:,1),histout100(:,2),'--','LineWidth',2,'Color','black');
   [m0,n0]=size(histout100);
   cost100(ip)=histout100(m0,2);
   [m0,n0]=size(histout500);
   cost500(ip)=histout500(m0,2);
   axis([0 200 7.d5 10.5d5]);
   if ip == 1
      legend('500 Simulations','100 Simulations');
      title('Serial Optimization',...
             'FontSize',12,'FontWeight','bold');
      matlabpool(8);
   else
      title('Parallel Optimization; 8 cores',...
            'FontSize',12,'FontWeight','bold');
   end
   xlabel('Model Calls','FontSize',12,'FontWeight','bold');
   ylabel('Function Value','FontSize',12,'FontWeight','bold');
   set(p2,'FontSize',14,'FontWeight','bold');
end

function vnew=lin_const(x,h,v)
% LIN_CONST
% Test the linear constraints and add tangent directions if the
% constraints are nearly violated.
%
[mv,nv]=size(v);
vnew=[];
flag1=0; flag2=0;
tangent1 = zeros(6,1); tangent1(3:4)=ones(2,1);
tangent2 = zeros(6,1); tangent1(5:6)=ones(2,1);
for i=1:nv
   x_trial=x+h*v(:,i);
   alpha1=x_trial(3); beta1=x_trial(4);
   if alpha1 > beta1 & flag1 == 0
      flag1=1;
      vnew=[vnew,tangent1,-tangent1];
   end
   alpha2=x_trial(5); beta2=x_trial(6);
   if alpha2 > beta2 & flag2 == 0
      flag2=1;
      vnew=[vnew,tangent2,-tangent2];
   end
end

