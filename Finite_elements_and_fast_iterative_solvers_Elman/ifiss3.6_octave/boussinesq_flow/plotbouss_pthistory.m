%PLOTBOUSS_PTHISTORY plots Boussinesq point history data 
% and generates  the file bouss_pthist (if the output switch is on)
%   IFISS scriptfile: DJS; 29 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.

if tout==2,
%% Point history data
   figure(1011); set(gcf,'Position',[0,450,380,360]);
   plot(ttch,uxh,'Color','blue');
   hold on
   title(['Velocity component u_x at the point (',num2str(xh1),',',num2str(yh1),')']);
   ylabel('u_x');
   xlabel('t');
   figure(1012); set(gcf,'Position',[390,450,420,360]);
   plot(ttch,uyh,'Color','red');
   hold on
   title(['Velocity component u_y at the point (',num2str(xh1),',',num2str(yh1),')']);
   ylabel('u_y');
   xlabel('t');
   figure(1013); set(gcf,'Position',[5,5,380,360]);
   plot(ttch,dph,'Color','green');
   hold on
   title(['Pressure difference between the points (',...
          num2str(xh1),',',num2str(yh1),') and (',num2str(xh4),',',num2str(yh4),')']);
   ylabel('Delta p');
   xlabel('t');
   figure(1014); set(gcf,'Position',[390,5,420,360]);
   plot(ttch,Th,'Color','black');
   hold on
   title(['Temperature at the point (',num2str(xh1),',',num2str(yh1),')']);
   ylabel('T');
   xlabel('t');
   %
   gohome, cd datafiles
   save bouss_pthist.mat ttch uxh uyh dph Th 
   fprintf('\npoint history data saved in bouss_pthist.mat\n');
end
return
