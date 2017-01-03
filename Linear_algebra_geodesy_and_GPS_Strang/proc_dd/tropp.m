% M-script exhibiting useful hints for handling graphics
% of axes, contour labels and lines.  We have chosen the
% tropospheric refraction delay for demonstration.

%Kai Borre 10-05-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/30 $

for incl = 0:1:10
   for temp = 1:10
      tt(1+incl,temp) = tropo(.1*incl,0,1010,250+5*temp,50,0,0,0);
   end
end

[c,cs] = contour(tt,[80 60 40 20 10 5 4 3 2.5]);
clabel(c);
set(cs,'MarkerSize',.2,'LineWidth',2);    
h1 = gca;
set(h1,'XLim',[1 10],'XTick',[1:2:9],...
       'XTickLabel',['-18';' -8';'  2';' 12';' 22'])
set(h1,'YLim',[1 11],'YTick',[1:2.5:11],...
       'YTickLabel',['0.00';'0.25';'0.50';'0.75';'1.00'])
set(get(gca,'xlabel'),'String',(['Temperature [' setstr(176) 'C]']),...       
       'FontName','Helvetica','FontWeight','Bold',...
       'FontSize',16,'Fontangle','oblique')
set(get(gca,'ylabel'),'String',('Sinus of Elevation Angle'),....    
       'FontName','Helvetica','FontSize',16,...
       'Fontangle','italic','VerticalAlignment','bottom')
set(gcf,'DefaultTextFontsize',12);      
set(h1,'Fontsize',16,'gridlinestyle','none')      
%Next the right y-label           
h2 = axes('Position',get(h1,'Position'));
set(h2,'YAxisLocation','right','Color','none','xtick',[],'XTickLabel',[])
set(h2,'XLim',get(h1,'XLim'),'Layer','top')
set(get(gca,'ylabel'),'String',('Elevation Angle'),'fontsize',16,...
        'verticalalignment','bottom','rotation',270)
set(h2, 'xdir','reverse','YLim', [1 11],'YTick', [1:2.5:11],...
        'YTickLabel', [' 0';'15';'45';'60';'90']);
set(get(h2,'Title'),'String',('Tropospheric Refraction Delay [m]'));
set(gcf,'numbertitle','off','name','Bonny')

print -deps tropp
%%%%%%%%%%%% end tropp.m %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

