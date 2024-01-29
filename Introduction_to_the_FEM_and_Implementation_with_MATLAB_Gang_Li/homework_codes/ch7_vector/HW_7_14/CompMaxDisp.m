function [x y maxU]=CompMaxDisp(feU)

maxU=0;
x=0;
y=0;
for i=1:size(feU,1)
  %if feU(i,2)==0 && feU(i,3)==-0.2 
  %  continue;
  %end
  d=sqrt(feU(i,4)^2 + feU(i,5)^2);
  if maxU<d
    maxU=d;
    x=feU(i,2);
    y=feU(i,3);
  end
end
