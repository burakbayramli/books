function test02
% New example: Island in a Bay
clc
FF1 = 'bsp05'; FF2 = 'bsp05g';
Start = 100; 
while ~ismember(Start,[0,1])
   Start = input(' New start or restart? (1/0) ');
end
if Start == 1;
distance = 0.1;
[p,e,seg_out,seg_int,N1,N2] = feval(FF1); t = [];
bild01(p,e,t,seg_out,seg_int,distance)
GRAFIK = 0;
[t,FORMED] = mesh11(p,e,seg_out,seg_int,GRAFIK);
LT = size(t,2);
t = [t;ones(1,LT)];
J  = N2:-1:1;
RADIUSA = 2/3;
X  = RADIUSA*cos(2*pi*(J-1)/N2);
Y  = RADIUSA*sin(2*pi*(J-1)/N2);
p2 = [X;Y];
p(:,N1+1:end) = p2;
save daten9a p e t seg_out seg_int distance
end
load daten9a p e t seg_out seg_int distance
%bild01(p,e,t,seg_out,seg_int,distance);
%pause
OPTION_MESH = 1; REFINE = 2; % Number of uniform mesh refinements
% TODO: OPTION_MESH = 2; REFINE = 0; % Number of uniform mesh refinements
% -----------------------------
JIGGLE = 1;
for J = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t] = mesh01_t(FF2,p,e,t);
   if JIGGLE == 1
     % p       = mesh10(p,e,t,4); % Jigglemesh
      t       = mesh03(p,t,0);   % replace long edges
   end  
end

bild00(p,e,t), hold on
%N = find_number(p,t)   
pause
bsp05k(p);