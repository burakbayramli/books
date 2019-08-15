function VN = vnomal(e,V,SCLAND,SCISLAND)
%%% flow tangential at coast following Ninomija %%%%
VN = V;
for K = 1:size(SCLAND,2)
   JJ = SCLAND(1,K);
   VS = V(1,JJ)*SCLAND(2,K) + V(2,JJ)*SCLAND(3,K);
   VN(1,JJ) = VS*SCLAND(2,K); VN(2,JJ) = VS*SCLAND(3,K);
end
if ~isempty(SCISLAND)
   for K = 1:size(SCISLAND,2)
      JJ = SCISLAND(1,K);
      VS = V(1,JJ)*SCISLAND(2,K) + V(2,JJ)*SCISLAND(3,K);
      VN(1,JJ) = VS*SCISLAND(2,K); VN(2,JJ) = VS*SCISLAND(3,K);
   end
end
