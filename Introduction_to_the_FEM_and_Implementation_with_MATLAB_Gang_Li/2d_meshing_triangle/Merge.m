function Merge(Lp, Rp, left_s,left_e, right_s,right_e)
[Lp, Rp]=CheckColinear(Lp, Rp);   % get any collinear node of Lp or Rp
Connect(Rp,Lp);  % connect the two points

while 1 % continue until exit from inside
  % next 17 lines: get the candidate right side node for edge Lp-Rp
  [re1,rp1]=GetNbrRayEndNode(Rp,Lp,-1);
  if CCW(rp1,Lp,Rp)>0      
    invalid_rp1=0;
    [re2,rp2]=GetNbrRayEndNode(Rp,rp1,-1);
    while InCircle(Rp, rp2, rp1, Lp)
      if rp2>right_e || rp2<right_s
        break;
      end
      bad_edge=re1;
      re1=re2;
      rp1=rp2;
      [re2,rp2]=GetNbrRayEndNode(Rp,rp1,-1);
      DeleteEdge(bad_edge);
    end
  else
    invalid_rp1=1;
  end
  % next 17 lines: get the candidate left side node for edge Lp-Rp
  [le1,lp1]=GetNbrRayEndNode(Lp,Rp,1);
  if CCW(lp1,Lp,Rp)>0                
    invalid_lp1=0;
    [le2,lp2]=GetNbrRayEndNode(Lp,lp1,1);
    while InCircle(Lp, lp1, lp2, Rp)
      if lp2>left_e || lp2<left_s
        break;
      end
      bad_edge=le1;
      le1=le2;
      lp1=lp2;
      [le2,lp2]=GetNbrRayEndNode(Lp,lp1,-1);
      DeleteEdge(bad_edge);
    end
  else
    invalid_lp1=1;
  end
  % next line: if no candidate node for either side, exit
  if invalid_rp1 && invalid_lp1;  return; end;
  % if-block: create the new Lp-Rp edge
  if invalid_lp1==1 || (~invalid_rp1 && InCircle(lp1,Lp,Rp,rp1))
    Connect(Lp,rp1);
    Rp=rp1;
  else
    Connect(Rp,lp1);
    Lp=lp1;
  end
end