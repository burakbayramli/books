function [Lp,Rp]=CompLCT(left_s,left_e, right_s,right_e)
global edges; 
Lp=left_e; Rp=right_s;
[redge,p1]=LeftMostCCWNext(right_s); % get left-most node (right side)
[ledge,p2]=RightMostCWNext(left_e);  % get right-most node (left side)

while 1  
  if CCW(p1,Rp,Lp)>0
    Rp=p1;
    if edges(redge,1)==Rp
      redge=edges(redge,5);  
      p1=GetRayEndNode(p1,redge);
    else
      redge=edges(redge,7);  
      p1=GetRayEndNode(p1,redge);
    end
  else
    if CCW(p2,Rp,Lp)>0
      Lp=p2;
      if edges(ledge,1)==Lp
        ledge=edges(ledge,4); 
        p2=GetRayEndNode(p2,ledge);
      else
        ledge=edges(ledge,6);  
        p2=GetRayEndNode(p2,ledge);
      end
    else
      return;
    end
  end 
end 