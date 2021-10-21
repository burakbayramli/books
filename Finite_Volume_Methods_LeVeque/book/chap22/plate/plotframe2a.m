
% plot a single frame of data from claw2
% modified to use dashed lines for negative values in contour plot

 % read time and number of grids:
 n1 = Frame+10000;
 fname = ['fort.',num2str(n1)];
 fname(6) = 't';
 
 if ~exist(fname) 
     disp(' ');
     disp(['Frame ',num2str(Frame),' does not exist ***']);
%     if Frame==0
%	% no initial data to plot, go on to Frame 1:
%	Frame = 1;
%	fname(length(fname))='1';
%	end
     end

if exist(fname)


% start reading data, beginning with parameters for this frame:

 fid = fopen(fname);

 t = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
 meqn = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);
 ngrids = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);

 fclose(fid);

 disp(' ')
 disp(['Frame ',num2str(Frame),' at time t = ',num2str(t)]);


 
 qmin = 1e6;
 qmax = -1e6;
 ncells = [];

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
   my = fscanf(fid,'%d',1);     fscanf(fid,'%s',1);   

   xlow = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
   ylow = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
   dx = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);
   dy = fscanf(fid,'%g',1);     fscanf(fid,'%s',1);

   GridInfo(ng,1) = gridno;
   GridInfo(ng,2) = level;
   GridInfo(ng,3) = mx;
   GridInfo(ng,4) = my;
   GridInfo(ng,5) = xlow;
   GridInfo(ng,6) = ylow;
   GridInfo(ng,7) = dx;
   GridInfo(ng,8) = dy;

   % read q data:

   data = fscanf(fid,'%g',[meqn,mx*my]);

   % if we're not plotting data at this level, skip to next grid
   if PlotData(level)==1 

   data = data';

   if UserVariable==1
       % User has supplied a function to convert original q variables to
       % the variable which is to be plotted, e.g. Mach number, entropy.
       qdata = feval(UserVariableFile,data);
       q = reshape(qdata,mx,my);
     else
       q = reshape(data(:,mq),mx,my);  
     end

   qmin = min(qmin,min(min(q)));  % minimum over all grids at this time
   qmax = max(qmax,max(max(q)));

   % keep count of how many cells at this refinement level:
   if length(ncells) < level
      ncells(level) = 0;
      end
   ncells(level) = ncells(level) + mx*my;

   %-----------------------------------------------------------
   if (PlotType<=3)
   %-----------------------------------------------------------
   % set x and y to lower left corners of cells and pad the 
   % q array with additional values since pcolor only uses a 
   % subgrid ignoring the last row and column.   These arrays are used for
   % pcolor and Schlieren plots and also in blanking out coarser grids for
   % contour plots.

   x = xlow + (0:mx)*dx;
   y = ylow + (0:my)*dy;

   oney = ones(1,length(y));
   onex = ones(length(x),1);
   xc = x' * oney;
   yc = onex * y;

   if MappedGrid
       % a coordinate mapping must be applied before plotting
       [xp,yp] = mapc2p(xc,yc);
     else
       % Cartesian grid
       xp = xc;
       yp = yc;
     end

   % augmented q array:
   qaug = ones(mx+1,my+1);
   qaug(1:mx,1:my) = q;
   qaug(1:mx,my+1) = q(:,my);
   qaug(mx+1,:) = qaug(mx,:);

   end % if PlotType<=3

    
   %--------------
   if PlotType==1
   %--------------
      % pseudo-color plot
      % plot data at different refinement levels at different z levels 
      % This will ensure that surf will properly overlay finer grid 
      % data on top of coarser.

      phandle = surf(xp',yp',0*qaug'+level-6,qaug');
      view(2)
      hold on
      if ng == 1 
          yrbcolormap    % put your favorite color map here
          caxis('auto')  
          %caxis([qmin, qmax])  % or other choice
          colorbar
          end

       if PlotGrid(level)==0
          % don't plot grid lines on this level:
          set(phandle,'EdgeColor','none')
          end

      end  % if PlotType==1

   %--------------
   if PlotType==3
   %--------------
      % for Schlieren plot, we compute gradient
      k = 5;
      k0 = 0.05;
      k1 = -0.001;
      [Ax,Ay] = gradient(qaug,dx,dy);
      qaug = sqrt(Ax.*Ax + Ay.*Ay);
      phandle = surf(xp',yp',0*qaug'+level-6,qaug');
      view(2)
      hold on
      if ng == 1 
          %shading('flat');
          colormap(flipud(gray(2048)).^10)
          end


       if PlotGrid(level)==0
          % don't plot grid lines on this level:
          set(phandle,'EdgeColor','none')
          end

      end  % if PlotType==3


   %-----------------------------------------------------------


   %-----------------------------------------------------------
   if PlotType==2
   %-----------------------------------------------------------
   % contour plot
         
   if level > 1
       % blank out contour lines from coarser grid in this region:
       [m1p,m2p] = size(xp);
       xpedge = [xp(1,:) xp(:,m2p)' fliplr(xp(m1p,:)) fliplr(xp(:,1)')];
       ypedge = [yp(1,:) yp(:,m2p)' fliplr(yp(m1p,:)) fliplr(yp(:,1)')];
       phandle = fill3(xpedge,ypedge,0*xpedge+level-6.5,'w');
       set(phandle,'EdgeColor','none')
       hold on
       end

    if PlotGrid(level)==1
       % plot grid lines:
       plot3(xp,yp,0*xp+level-6.5,'k')
       hold on
       plot3(xp',yp',0*xp'+level-6.5,'k')
       end

   % x and y are at cell centers:
   x = xlow + ((1:mx) - 0.5)*dx;
   y = ylow + ((1:my) - 0.5)*dy;

      if cauto
         c = contourc(x,y,q',nc);
                  % chooses nc contour lines for this grid.
		  % Note that lines may not match between grids!
        else
         c = contourc(x,y,q',cline);
        end

      % plot contour lines one at a time.  Plot them on appropriate level 
      % so they are above surface blanking out contour lines from coarser
      % grids.   If there's a coordinate mapping, apply to each line.

      i = 1;
      while i<size(c,2);
        npts = c(2,i);
        cx = c(1,(i+1):(i+npts));
        cy = c(2,(i+1):(i+npts));
        if MappedGrid
           % coordinate mapping must be applied
           [cx,cy] = mapc2p(cx,cy);
	   end
        if (c(1,i)>=0)
            plot3(cx,cy,0*cx+level-6,'k')
          else
            plot3(cx,cy,0*cx+level-6,'k--')
          end
        hold on
        i = i+npts+1;
        end


      hold on

   view(2)

   end  % if PlotType==2
   %-----------------------------------------------------------


   %-----------------------------------------------------------
   if PlotType==4
   %-----------------------------------------------------------
   % scatter plot
   % plot value of q vs. distance from cell center to "origin"
   %
   % determine distance r of each cell center from (x0,y0):


   x = xlow + ((1:mx) - 0.5)*dx;
   y = ylow + ((1:my) - 0.5)*dy;
   oney = ones(1,length(y));
   onex = ones(length(x),1);
   xc = x' * oney;
   yc = onex * y;

   if MappedGrid
       % a coordinate mapping must be applied before plotting
       [xp,yp] = mapc2p(xc,yc);
     else
       % Cartesian grid
       xp = xc;
       yp = yc;
     end

   r = sqrt((xp-x0).^2 + (yp-y0).^2)';

   % plot values of q vs. radius r,
   % use different colors depending on grid level:

   if level==1
      plot(r,q','ok')
      end
   if level==2
      plot(r,q','or')
      end
   if level==3
      plot(r,q','ob')
      end
   if level>=4
      plot(r,q','.k')
      end
   hold on

   end  % if PlotType==4
   %-----------------------------------------------------------


   if UserVariable==1
       title([UserVariableFile,'   at time t = ', num2str(t)])
     else
       title(['q(',num2str(mq),')   at time t = ', num2str(t)])
     end

   if (PlotGridEdges(level)==1 & PlotType~=4)
      % plot the edges of the grids:
      [m1p,m2p] = size(xp);
      plot(xp(1,:),yp(1,:),'k')
      plot(xp(m1p,:),yp(m1p,:),'k')
      plot(xp(:,1),yp(:,1),'k')
      plot(xp(:,m2p),yp(:,m2p),'k')
      end

   %drawnow
   %query     % Uncomment this line to pause after plotting each subgrid
	      % Useful if you want to examine data on some subgrid

   end %   if PlotData(level)==1 
   %=============================================
   end % loop on ng (plot commands for each grid)
   %=============================================

 % done with this frame

 if exist('afterframe')==2  
    afterframe  % make an m-file with this name for any other commands you
	        % want executed at the end of drawing each frame
                % for example to change the axes, or add a curve for a
                % boundary
    end

 hold off


 status = fclose(fid);
 end % if exist(fname)
