
hold on

for i = 1:sz
split1 = y(i,1);
switch split1
   case 1
   split2 = y(i,2);
   switch split2
      case 1,
      split3 = y(i,3);
      switch split3
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'r.','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'k.','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'b.','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
      case 0,
      split3 = y(i,3);
      switch split3,
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'ro','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'ko','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'bo','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
      case -1,
      split3 = y(i,3);
      switch split3
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'rx','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'kx','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'bx','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
   end
   case 0
   split2 = y(i,2);
   switch split2
      case 1,
      split3 = y(i,3);
      switch split3
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'r+','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'k+','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'b+','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
      case 0,
      split3 = y(i,3);
      switch split3,
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'r*','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'k*','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'b*','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
      case -1,
      split3 = y(i,3);
      switch split3
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'rs','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'ks','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'bs','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
   end
   case -1
   split2 = y(i,2);
   switch split2
      case 1,
      split3 = y(i,3);
      switch split3
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'rd','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'kd','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'bd','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
      case 0,
      split3 = y(i,3);
      switch split3,
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'rv','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'kv','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'bv','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
      case -1,
      split3 = y(i,3);
      switch split3
        case 1,
        plot3(v(i,1),v(i,2),v(i,3),'r^','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case 0,
        plot3(v(i,1),v(i,2),v(i,3),'k^','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
        case -1,
        plot3(v(i,1),v(i,2),v(i,3),'b^','ButtonDownFcn',num2str(i));
	text(v(i,1),v(i,2),v(i,3),['  ' int2str(i) ],'FontSize',11);
      end
   end
end
end
