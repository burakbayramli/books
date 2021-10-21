%
% plotclaw1xt.m
%
% generic plotting routine for claw1d and amrclaw1d output in matlab
% R. J. LeVeque, 1999
%
% Various styles of plots are included, the routines prompts for type.
%
% plotclaw1 can be started before clawpack is finished with the computation
% It will pause and wait for the next output time as needed.
% For this to work, remove any old files fort.* before starting the 
% new computation.
%
%
% Various parameters are hardwired in but can be changed by setting
% them explicitly in matlab before calling plotclaw1:
%
%   MappedGrid == 1 means that a function mapc2p.m exists which maps 
%              computational points on rectangular grid to physical points.
%            
%------------------------------


qxt = [];
txt = [];

% general set-up:


plotstyle = '-';   % plot as a line by default


mqrange = input('Which component of q?  (default=1, 0 for user-defined variable) '); 
			% Determine which element(s)
			% should be plotted.
if isempty(mqrange)
  mqrange = 1;  % defaults to 1
  end

if mqrange==0
    UserVariable = 1;
    UserVariableFile = ...
	input('File name of function defining variable (in quotes) ');
  else
    UserVariable = 0;
  end


if exist('MappedGrid')~=1
  MappedGrid = 0;  % default is no coordinate mapping
  end

if exist('setprob')==2
   % the file setprob.m can be used to set up any necessary physical parameters
   % or desired values of plotting parameters for this particular problem.
   setprob
   end

clf


n = 0;  % frame counter
maxframes = 1000;



%=============================================
% MAIN LOOP ON FRAMES:
%=============================================

while n <= maxframes
 % read time and number of grids:
 n1 = n+10000;
 fname = ['fort.',num2str(n1)];
 fname(6) = 't';
 
 if ~exist(fname) 
     disp(' ');
     disp(['Frame ',num2str(n),' does not exist ***']);
     if n==0
	% no initial data to plot, go on to Frame 1:
	n = 1;
	fname(length(fname))='1';
	end
     end

 if exist(fname)

% start reading data, beginning with parameters for this frame:

 fid = fopen(fname);

 t = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
 meqn = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);
 ngrids = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);

 if ngrids==0 
   % flag that the computation has ended, in case nframes wasn't known
   % initially
   break
   end

 disp(' ')
 disp(['Frame ',num2str(n),' at time t = ',num2str(t)]);


 clf

 if exist('beforeframe')==2
    beforeframe  % make an m-file with this name for any other commands you
                % want executed before drawing each frame, for example
                 % if you want to use axes to specify exactly where the
                 % plot will be in the window, aspect ratio, etc.
    end


 % change the file name to read the q data:
 fname(6) = 'q';
 fid = fopen(fname);


 %=============================================
 % MAIN LOOP ON GRIDS FOR THIS FRAME:
 %=============================================

 for ng = 1:ngrids

   % read parameters for this grid:

   gridno = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);
   level = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);
   mx = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);

   xlow = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
   dx = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);

   % read q data:

   data = fscanf(fid,'%g',[meqn,mx]);
   data = data';


   if UserVariable==1
       % User has supplied a function to convert original q variables to
       % the variable which is to be plotted, e.g. Mach number, entropy.
       q = feval(UserVariableFile,data);
       end

   x = xlow + ((1:mx) - 0.5)*dx;

   if MappedGrid
      % coordinate mapping must be applied
      x = mapc2p(x);
      end

%=====================================================================
%  The plot command:
%=====================================================================

 if UserVariable==1
     plot(x,q,plotstyle)
     title([UserVariableFile,'   at time t = ', num2str(t)])
  else
   for  mq=mqrange
     subplot(length(mqrange),1,find(mqrange==mq))
     q = data(:,mq);
     plot(x,q,plotstyle)
     title(['q(',num2str(mq),')   at time t = ', num2str(t)])
     end  % loop on mq
  end 


 % done with this frame
 hold on

 if exist('afterframe')==2  
    afterframe  % make an m-file with this name for any other commands you
	        % want executed at the end of drawing each frame
                % for example to change the axes, or add something to the plot
    end
 hold off

 status = fclose(fid);

 % store data for x-t plot:
 qxt(:,n+1) = q;
 txt(:,n+1) = t;

 if n==0
    q0 = q;
    end


 end
 n = n+1;
end % if exist(fname)

if ~exist(fname)
  break
  end


 end % main loop on frames
 %=============================================

xlim = [-30 20];
tlim = [-1 20];
figure(1)
clf
cline = [-.05:.02:1];
contour(x,txt,qxt',cline)
colormap([0 0 1])
axis([xlim tlim])

hold on
% add contour from (0,0)
plot([0 -11.2], [0 12.4], 'b');
hold off

figure(3)
clf
hdensity = axes('position',[.1 .1 .8 .35]);
plot(x,q)
title(['density at time ', num2str(t)])
axis([xlim -.1 1.1])

figure(2)
clf
hdensity = axes('position',[.1 .1 .8 .35]);
plot(x,q0)
title('density at time 0')
axis([xlim -.1 1.1])

exno = 1;
figure(1)
eval(['print claw',num2str(exno),'xt -deps'])
figure(2)
eval(['print claw',num2str(exno),'rho0 -deps'])
figure(3)
eval(['print claw',num2str(exno),'rho1 -deps'])

