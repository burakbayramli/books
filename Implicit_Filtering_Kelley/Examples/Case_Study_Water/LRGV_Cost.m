function [fout, ifail, icount] = LRGV_Cost(LRGV_VARS,LRGV_str);

%
% Model output by Brian Kirsch. Modified by C. T. Kelley in 2010.
% Readers of Implicit Filtering, SIAM 2011, by C. T. Kelley should
% treat this MATLAB function as a black box.
%

%
% Harvest the optimization variables from the input vector.
%
Nrt=LRGV_VARS(1);
No=LRGV_VARS(2);
A=LRGV_VARS(3);
B=LRGV_VARS(4);
A2=LRGV_VARS(5);
B2=LRGV_VARS(6);
%
% Gamma is a variable for a different problem from a different paper. 
%
gamma = 1.0;
%
% Get the model parameters from the LRGV_str structure.
%
NumberSims=LRGV_str.NumberSims;
RELmin=LRGV_str.RELmin;
CVARmax=LRGV_str.CVARmax;
ifri=LRGV_str.ifri;
iRo=LRGV_str.iRo;

%
% Test linear constraints for free.
%

if A > B
   ifail = 1;
   fout = NaN;
   icount = 0;
   return
end
if A2 > B2 
   ifail = 1;
   fout = NaN;
   icount = 0;
   return
end

%
% Tourists! Stop reading here.
%

DemGrowthFactor = .00;

global AprInf  AprLeasePriceListHigRes  AprLeasePriceListLowRes  AprLos   ...
AprNW AprResVar  AugInf  AugLeasePriceListHigRes  ...
 AugLeasePriceListLowRes  AugLos  AugNW  AugResVar  DecInf  ...
 DecLeasePriceListHigRes  DecLeasePriceListLowRes  DecLos  DecNW  ...
 DecResVar  FebInf  FebLeasePriceListHigRes  FebLeasePriceListLowRes  ...
 FebLos  FebNW  FebResVar  JanInf  JanLeasePriceListHigRes  ...
 JanLeasePriceListLowRes  JanLos  JanNW  JanResVar  JulInf  ...
 JulLeasePriceListHigRes  JulLeasePriceListLowRes  JulLos  JulNW  JulResVar  ...
 JunInf  JunLeasePriceListHigRes  JunLeasePriceListLowRes  JunLos  JunNW  ...
 JunResVar  MarInf  MarLeasePriceListHigRes  MarLeasePriceListLowRes  ...
 MarLos  MarNW  MarResVar  MayInf  MayLeasePriceListHigRes  ...
 MayLeasePriceListLowRes  MayLos  MayNW  MayResVar  NovInf  ...
 NovLeasePriceListHigRes  NovLeasePriceListLowRes  NovLos  NovNW  ...
 NovResVar  OctInf  OctLeasePriceListHigRes  OctLeasePriceListLowRes  ...
  OctLos  OctNW  OctResVar  SepInf  SepLeasePriceListHigRes  ...
 SepLeasePriceListLowRes  SepLos  SepNW  SepResVar SampledJanDem ...
 SampledFebDem SampledMarDem SampledAprDem SampledMayDem SampledJunDem ...
 SampledJulDem SampledAugDem SampledSepDem SampledOctDem SampledNovDem ...
 SampledDecDem Pohigh Polow

LRGV_datafile

Pr = 22.6;
Px = 15;
%Po = 13.26;
%Pohigh = 2.18;
%Polow = 15.98;
TWR = 1900000;
CityAvgAnWaterUse = 20885.6;
NumberYears = 1;
SimYear = NumberYears;
OExerciseMonth = 5;
cfl = 0.6;
Result = zeros(1,11);
PortfolioCostList = zeros(NumberYears, 1);
EndofYearWater = zeros(NumberYears, 1);
MonthlyDemandMean = [1569 1453.4 1679.2 1743.1 1958.5 1992.2 2181.1 1950.8 1704.8 1618.9 1481.3 1553.3]';
MonthlyDemandStdDev = [171.7 163.2 161.5 267.7 339.3 423.8 286.7 170.8 245.6 206.8 119.8 147.9]';
EJandem = 1569;
EFebdem = 1453.4;
EMardem = 1679.2;
EAprdem = 1743.1;
EMaydem = 1958.5;
EJundem = 1992.2;
EJuldem = 2181.1;
EAugdem = 1950.8;
ESepdem = 1704.8;
EOctdem = 1618.9;
ENovdem = 1481.3;
EDecdem = 1553.3;

%**************************
% Call pilot study
decisionvars = [Nrt No A B A2 B2 gamma]';
initial_conditions = [ifri iRo DemGrowthFactor]';
%inumber = 50;
%[cstar1, cstarR2, TotalAnnualCostsOut, ReliabilityOut, EstimatedVarRedux] = MasterPilotpreT0(decisionvars, initial_conditions, NumberYears, inumber);
%cstar = cstar1;

AnnualSupply = zeros(NumberYears, 1);      %Set annual supply as quantity of perm. rights fulfilled/yr (W/O DEC. VALUES!)
AnnualDemand = zeros(NumberYears, 1);      %Qty. of water consumed each year (W/O DEC. VALUES!)
AnnualNewWater = zeros(NumberYears, 1);    %Qty. of new water coming into reservoir/yr. (W/O DEC. VALUES)
OptionTracker = zeros(NumberYears, 1);
AnnualNlPl = zeros(NumberYears, 1);
Z1 = zeros(NumberYears, 1);
Z2 = zeros(NumberYears, 1);
Z3 = zeros(NumberYears, 1);
Z4 = zeros(NumberYears, 1);
Z5 = zeros(NumberYears, 1);
EarlyNewWater = zeros(NumberYears, 1);
LateNewWater = zeros(NumberYears, 1);
%EarlyDemand = zeros(NumberYears, 1);
LateDemand = zeros(NumberYears, 1);
ToLeasePrice = zeros(NumberYears, 1);
Pl5 = zeros(NumberYears, 1);

%Create yearly lists
YearlyPurhcasedLeasesList = zeros(NumberSims, NumberYears);
YearlyExercisedOptionsList = zeros(NumberSims, NumberYears);  %May not need this line any more
TotalMonthlyLeasesList = zeros(NumberYears,12,NumberSims);
TotalMonthlyExercisedOptionsList = zeros(NumberSims, NumberYears);
TotalMonthlyFailureList = 0;        %Check what this is for later in model
YearlyLeaseCostList = zeros(NumberYears, 12, NumberSims);
YearlyExOptionCostList = zeros(NumberSims, NumberYears);
TotalPoList = zeros(NumberSims, NumberYears);
DemandList = [0];
ResLevelTracker = zeros(NumberYears, 12, NumberSims);
EndofYrWaterTracker = zeros(NumberSims, NumberYears);    %Track water @ end of year
DroppedTransferTracker = zeros(NumberYears, 12, NumberSims);
%CF = 0;
%FailuresList = zeros(1,12);


MonthlyDemStdDevMean = MonthlyDemandMean./MonthlyDemandStdDev;
FutureMonthlyDemandMean = zeros(NumberYears, 12);
FutureMonthlyStdDev = zeros(NumberYears, 12);
FutureMonthlyStdDev(1,:) = MonthlyDemandStdDev;
FutureNrt = zeros(NumberYears, 1);
FutureNrt(1) = Nrt;

i = 1;
for i = 1:NumberYears
   FutureMonthlyDemandMean(i,:) = MonthlyDemandMean*(1 + DemGrowthFactor)^(i - 1);
   FutureMonthlyStdDev(i,:) = FutureMonthlyDemandMean(i,:)./FutureMonthlyDemandMean(1,:).*FutureMonthlyStdDev(1,:);
   if i >= 2
      FutureNrt(i) = FutureNrt(i-1)*gamma;         %Nrt growth is set up to be exponential
   end
   i = i + 1;
end

%Expected monthly allocations
ENr1 = zeros(NumberYears, 1);
ENr2 = zeros(NumberYears, 1);
ENr3 = zeros(NumberYears, 1);
ENr4 = zeros(NumberYears, 1);
ENr5 = zeros(NumberYears, 1);
ENr6 = zeros(NumberYears, 1);
ENr7 = zeros(NumberYears, 1);
ENr8 = zeros(NumberYears, 1);
ENr9 = zeros(NumberYears, 1);
ENr10 = zeros(NumberYears, 1);
ENr11 = zeros(NumberYears, 1);
ENr12 = zeros(NumberYears, 1);
                  
%Expected monthly allocations
ENr1 = mean(JanNW)*0.825*FutureNrt/TWR;
ENr2 = mean(FebNW)*0.825*FutureNrt/TWR;
ENr3 = mean(MarNW)*0.825*FutureNrt/TWR;
ENr4 = mean(AprNW)*0.825*FutureNrt/TWR;
ENr5 = mean(MayNW)*0.825*FutureNrt/TWR;
ENr6 = mean(JunNW)*0.825*FutureNrt/TWR;
ENr7 = mean(JulNW)*0.825*FutureNrt/TWR;
ENr8 = mean(AugNW)*0.825*FutureNrt/TWR;
ENr9 = mean(SepNW)*0.825*FutureNrt/TWR;
ENr10 = mean(OctNW)*0.825*FutureNrt/TWR;
ENr11 = mean(NovNW)*0.825*FutureNrt/TWR;
ENr12 = mean(DecNW)*0.825*FutureNrt/TWR;
TotalENr = zeros(NumberYears, 12);
TotalENr(:,1) = ENr1;
TotalENr(:,2) = ENr2;
TotalENr(:,3) = ENr3;
TotalENr(:,4) = ENr4;
TotalENr(:,5) = ENr5;
TotalENr(:,6) = ENr6;
TotalENr(:,7) = ENr7;
TotalENr(:,8) = ENr8;
TotalENr(:,9) = ENr9;
TotalENr(:,10) = ENr10;
TotalENr(:,11) = ENr11;
TotalENr(:,12) = ENr12;

RandomMonthlyDemand = zeros(SimYear, 12, NumberSims);
DemandList = zeros(NumberSims*SimYear, 12);
LossesList = zeros(NumberSims*SimYear, 12);
InflowsList = zeros(NumberSims*SimYear, 12);
ResVariationList = zeros(NumberSims*SimYear, 12);
LeasePriceListLowRes = zeros(NumberSims*SimYear, 12);
LeasePriceListHigRes = zeros(NumberSims*SimYear, 12);

j = 1;
for j = 1:NumberYears
   RandomMonthlyDemand(j,1,:) = randsample(SampledJanDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,2,:) = randsample(SampledFebDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,3,:) = randsample(SampledMarDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,4,:) = randsample(SampledAprDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,5,:) = randsample(SampledMayDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,6,:) = randsample(SampledJunDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,7,:) = randsample(SampledJulDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,8,:) = randsample(SampledAugDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,9,:) = randsample(SampledSepDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,10,:) = randsample(SampledOctDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,11,:) = randsample(SampledNovDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   RandomMonthlyDemand(j,12,:) = randsample(SampledDecDem, NumberSims, true)*(1+DemGrowthFactor)^(j-1);
   j = j + 1;
end


% j = 1;
% for j = 1:NumberSims
% 
%    RandomMonthlyDemand(:,:,j) = random('Normal', FutureMonthlyDemandMean, FutureMonthlyStdDev);  
%    j = j + 1;
% end
DemandList = RandomMonthlyDemand;
%RandomMonthlyDemand = random('Normal', MonthlyDemandMean, MonthlyDemandStdDev, NumberSims*SimYear, 12);
LossesList(:,1) = randsample(JanLos,NumberSims*SimYear,true);
LossesList(:,2) = randsample(FebLos,NumberSims*SimYear,true);
LossesList(:,3) = randsample(MarLos,NumberSims*SimYear,true);
LossesList(:,4) = randsample(AprLos,NumberSims*SimYear,true);
LossesList(:,5) = randsample(MayLos,NumberSims*SimYear,true);
LossesList(:,6) = randsample(JunLos,NumberSims*SimYear,true);
LossesList(:,7) = randsample(JulLos,NumberSims*SimYear,true);
LossesList(:,8) = randsample(AugLos,NumberSims*SimYear,true);
LossesList(:,9) = randsample(SepLos,NumberSims*SimYear,true);
LossesList(:,10) = randsample(OctLos,NumberSims*SimYear,true);
LossesList(:,11) = randsample(NovLos,NumberSims*SimYear,true);
LossesList(:,12) = randsample(DecLos,NumberSims*SimYear,true);
InflowsList(:,1) = randsample(JanInf,NumberSims*SimYear,true);
InflowsList(:,2) = randsample(FebInf,NumberSims*SimYear,true);
InflowsList(:,3) = randsample(MarInf,NumberSims*SimYear,true);
InflowsList(:,4) = randsample(AprInf,NumberSims*SimYear,true);
InflowsList(:,5) = randsample(MayInf,NumberSims*SimYear,true);
InflowsList(:,6) = randsample(JunInf,NumberSims*SimYear,true);
InflowsList(:,7) = randsample(JulInf,NumberSims*SimYear,true);
InflowsList(:,8) = randsample(AugInf,NumberSims*SimYear,true);
InflowsList(:,9) = randsample(SepInf,NumberSims*SimYear,true);
InflowsList(:,10) = randsample(OctInf,NumberSims*SimYear,true);
InflowsList(:,11) = randsample(NovInf,NumberSims*SimYear,true);
InflowsList(:,12) = randsample(DecInf,NumberSims*SimYear,true);
ResVariationList(:,1) = randsample(JanResVar,NumberSims*SimYear,true);
ResVariationList(:,2) = randsample(FebResVar,NumberSims*SimYear,true);
ResVariationList(:,3) = randsample(MarResVar,NumberSims*SimYear,true);
ResVariationList(:,4) = randsample(AprResVar,NumberSims*SimYear,true);
ResVariationList(:,5) = randsample(MayResVar,NumberSims*SimYear,true);
ResVariationList(:,6) = randsample(JunResVar,NumberSims*SimYear,true);
ResVariationList(:,7) = randsample(JulResVar,NumberSims*SimYear,true);
ResVariationList(:,8) = randsample(AugResVar,NumberSims*SimYear,true);
ResVariationList(:,9) = randsample(SepResVar,NumberSims*SimYear,true);
ResVariationList(:,10) = randsample(OctResVar,NumberSims*SimYear,true);
ResVariationList(:,11) = randsample(NovResVar,NumberSims*SimYear,true);
ResVariationList(:,12) = randsample(DecResVar,NumberSims*SimYear,true);
LeasePriceListLowRes(:,1) = randsample(JanLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,2) = randsample(FebLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,3) = randsample(MarLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,4) = randsample(AprLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,5) = randsample(MayLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,6) = randsample(JunLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,7) = randsample(JulLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,8) = randsample(AugLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,9) = randsample(SepLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,10) = randsample(OctLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,11) = randsample(NovLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListLowRes(:,12) = randsample(DecLeasePriceListLowRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,1) = randsample(JanLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,2) = randsample(FebLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,3) = randsample(MarLeasePriceListHigRes,NumberSims*SimYear,true);

LeasePriceListHigRes(:,4) = randsample(AprLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,5) = randsample(MayLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,6) = randsample(JunLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,7) = randsample(JulLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,8) = randsample(AugLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,9) = randsample(SepLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,10) = randsample(OctLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,11) = randsample(NovLeasePriceListHigRes,NumberSims*SimYear,true);
LeasePriceListHigRes(:,12) = randsample(DecLeasePriceListHigRes,NumberSims*SimYear,true);

TotalFailures = zeros(NumberYears, 12, NumberSims);
TotalCFailures = zeros(NumberYears, 12, NumberSims);
TotalNxTracker = zeros(NumberSims, NumberYears);
BigTime = NumberSims*NumberYears;

EarlyNewWater = zeros(NumberSims, NumberYears);
LateNewWater = zeros(NumberSims, NumberYears);
ToLeasePrice = zeros(NumberSims, NumberYears);
EarlyDemand = zeros(NumberSims, NumberYears);
LateDemand = zeros(NumberSims, NumberYears);
Pl5 = zeros(NumberSims, NumberYears);


i = 1;
j = 1;
for i = 1:NumberSims
   for j = 1:NumberYears
      EarlyDemand(i,j) = DemandList(j,1,i) + DemandList(j,2,i) + DemandList(j,3,i) + DemandList(j,4,i) + DemandList(j,5,i);
      LateDemand(i,j) = DemandList(j,6,i) + DemandList(j,7,i) + DemandList(j,8,i) + DemandList(j,9,i) + DemandList(j,10,i) + DemandList(j,11,i) + DemandList(j,12,i); 
      
   end
end


for i = 1:NumberSims
   
   CurrentSimYear = 1;
   SimNumber = i;
   Ro = iRo;
   fri = ifri;
   CurrentResLevel = Ro;
   OldReservoirLevel = CurrentResLevel;
   %Allocation at the beginning of the year
   Nro = fri*Nrt;
   To = Nro;
   NewMonthAllocationList = [Nro];
   LeaseCost = zeros(NumberYears, 12);
   Failures = zeros(NumberYears, 12);
   CFailures = zeros(NumberYears, 12);
   NumberPurchasedLeasesList = zeros(NumberYears, 12);
   NxTracker = zeros(NumberYears, 1);
   PoList = zeros(1,NumberYears);
   TransferTracker = zeros(13,1);
   DroppedTransfers = 0;
   %************
%    ENr1 = mean(JanNW)*0.825*Nrt/TWR;
%    ENr2 = mean(FebNW)*0.825*Nrt/TWR;
%    ENr3 = mean(MarNW)*0.825*Nrt/TWR;
%    ENr4 = mean(AprNW)*0.825*Nrt/TWR;
   %************
   
   RoDiff = 1430000 - Ro;
   
   if RoDiff < -505137
      Po = Pohigh;
   elseif RoDiff > -505137 & RoDiff <= -422334
      Po = 0.95*Pohigh + 0.05*Polow;
   elseif RoDiff > -422334 & RoDiff <= -377877
      Po = 0.9*Pohigh + 0.1*Polow;
   elseif RoDiff > -377877 & RoDiff <= -345215
      Po = 0.85*Pohigh + 0.15*Polow;
   elseif RoDiff > -345215 & RoDiff <= -318614
      Po = 0.8*Pohigh + 0.2*Polow;
   elseif RoDiff > -318614 & RoDiff <= -294232
      Po = 0.75*Pohigh + 0.25*Polow;
   elseif RoDiff > -294232 & RoDiff <= -272259
      Po = 0.7*Pohigh + 0.3*Polow;
   elseif RoDiff > -272259 & RoDiff <= -251333
      Po = 0.65*Pohigh + 0.35*Polow;
   elseif RoDiff > -251333 & RoDiff <= -231228
      Po = 0.6*Pohigh + 0.4*Polow;
   elseif RoDiff > -231228 & RoDiff <= -211446
      Po = 0.55*Pohigh + 0.45*Polow;
   elseif RoDiff > -211446 & RoDiff <= -191284
      Po = 0.5*Pohigh + 0.5*Polow;
   elseif RoDiff > -191284 & RoDiff <= -171483
      Po = 0.45*Pohigh + 0.55*Polow;
   elseif RoDiff > -171483 & RoDiff <= -150459
      Po = 0.4*Pohigh + 0.6*Polow;
   elseif RoDiff > -150459 & RoDiff <= -129043
      Po = 0.35*Pohigh + 0.65*Polow;
   elseif RoDiff > -129043 & RoDiff <= -105301
      Po = 0.3*Pohigh + 0.7*Polow;
   elseif RoDiff > -105301 & RoDiff <= -79374
      Po = 0.25*Pohigh + 0.75*Polow;
   elseif RoDiff > -79374 & RoDiff <= -49800
      Po = 0.2*Pohigh + 0.8*Polow;
   elseif RoDiff > -49800 & RoDiff <= -11274
      Po = 0.15*Pohigh + 0.85*Polow;
   elseif RoDiff > -11274 & RoDiff <= 45751
      Po = 0.1*Pohigh + 0.9*Polow;
   elseif RoDiff > 45751 & RoDiff <= 179620
      Po = 0.05*Pohigh + 0.95*Polow;
   elseif RoDiff > 179620
      Po = Polow;
   end

   PoList(CurrentSimYear) = Po;
   
   if (ENr1(1)+ENr2(1)+ENr3(1)+ENr4(1)) > (Nrt-Nro)
      ENrt = Nrt - Nro;
   else
      ENrt = ENr1(1) + ENr2(1) + ENr3(1) + ENr4(1);
   end
                   
   %Total expected available water
   Te1 = To + ENrt;% + No;
   
   t = 0;
   
   % Control variate monitoring
   
   %AnnualSupplyi(1) = Nro;          %Initial fulfillment of rights
   AnnualNewWateri = zeros(11,1);
                     
   if CurrentResLevel <= 1430000                               %This if-statement used to be inside the next if-statement
       Plo = LeasePriceListLowRes((SimNumber-1)*NumberYears+CurrentSimYear, 12);
   else
       Plo = LeasePriceListHigRes((SimNumber-1)*NumberYears+CurrentSimYear, 12);
   end
   
   ToLeasePrice(SimNumber, CurrentSimYear) = Plo;
   
   d1 = [FutureMonthlyDemandMean(CurrentSimYear,1)+FutureMonthlyDemandMean(CurrentSimYear,2)+FutureMonthlyDemandMean(CurrentSimYear,3)+FutureMonthlyDemandMean(CurrentSimYear,4)+FutureMonthlyDemandMean(CurrentSimYear,5)];
                     
   if Te1/d1 <= A2
                        
       if (B2*d1 - Te1) > 0
             N1o = (B2*d1 - Te1);
       else
             N1o = 0;
       end
       LeaseCost(CurrentSimYear) = LeaseCost(CurrentSimYear, 1) + N1o*Plo;
   else
       N1o = 0;
   end
   
   TransferTracker(1) = N1o;
   %Put in option Price here!!!!!!!!!!!!!!!!!!!!!!!
   
   
   %**********************************************
    
   
    %ToLeasePrice(j) = Plo;
    T1 = N1o + Nro;
    AvWaterList(1) = T1;
    t = 1;
                     
    NumberPurchasedLeasesList(CurrentSimYear, t) = N1o;         %Change this index!!!!!!
    %AnnualSupplyi = zeros(NumberYears, 12)
    
    
    %**********************************************************************
   
   %Handle t=0 stuff here
   for CurrentSimYear = 1:NumberYears
      
      ExpAlloRestYearList = [ENr2(CurrentSimYear)+ENr3(CurrentSimYear)+ENr4(CurrentSimYear)+ENr5(CurrentSimYear)+ENr6(CurrentSimYear)+ENr7(CurrentSimYear)+ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr3(CurrentSimYear)+ENr4(CurrentSimYear)+ENr5(CurrentSimYear)+ENr6(CurrentSimYear)+ENr7(CurrentSimYear)+ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr4(CurrentSimYear)+ENr5(CurrentSimYear)+ENr6(CurrentSimYear)+ENr7(CurrentSimYear)+ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr5(CurrentSimYear)+ENr6(CurrentSimYear)+ENr7(CurrentSimYear)+ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr6(CurrentSimYear)+ENr7(CurrentSimYear)+ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr7(CurrentSimYear)+ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr8(CurrentSimYear)+ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr9(CurrentSimYear)+ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr10(CurrentSimYear)+ENr11(CurrentSimYear); ENr11(CurrentSimYear)]'; 
      ExpYearDemandList = [FutureMonthlyDemandMean(CurrentSimYear,2)+FutureMonthlyDemandMean(CurrentSimYear,3)+FutureMonthlyDemandMean(CurrentSimYear,4)+FutureMonthlyDemandMean(CurrentSimYear,5)+FutureMonthlyDemandMean(CurrentSimYear,6)+FutureMonthlyDemandMean(CurrentSimYear,7)+FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,3)+FutureMonthlyDemandMean(CurrentSimYear,4)+FutureMonthlyDemandMean(CurrentSimYear,5)+FutureMonthlyDemandMean(CurrentSimYear,6)+FutureMonthlyDemandMean(CurrentSimYear,7)+FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,4)+FutureMonthlyDemandMean(CurrentSimYear,5)+FutureMonthlyDemandMean(CurrentSimYear,6)+FutureMonthlyDemandMean(CurrentSimYear,7)+FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,5)+FutureMonthlyDemandMean(CurrentSimYear,6)+FutureMonthlyDemandMean(CurrentSimYear,7)+FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,6)+FutureMonthlyDemandMean(CurrentSimYear,7)+FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,7)+FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,8)+FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,9)+FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,10)+FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,11)+FutureMonthlyDemandMean(CurrentSimYear,12); FutureMonthlyDemandMean(CurrentSimYear,12)]';     
      Nx = 0;
      AnnualSupplyi = zeros(12,1);  %Not interested in Dec. demand or supply
      
      
      for t = 1:12
      
          if t == 1
             MonthlyWaterUsageList = zeros(12,1);
          end
          N1 = 0;
          %Failures = 0;
          %Select monthly values
          AvWater = AvWaterList(t);
          Inflows = InflowsList((SimNumber - 1)*NumberYears + CurrentSimYear, t);
          Demand = DemandList(CurrentSimYear, t, SimNumber);
          Losses = LossesList((SimNumber - 1)*NumberYears + CurrentSimYear, t);
          ResVariation = ResVariationList((SimNumber - 1)*NumberYears + CurrentSimYear, t);
          %If monthly dmeand exceeds the available water
          %register a failure for that month
          if AvWater >= Demand
             u = Demand;
          else
             Failures(CurrentSimYear, t) = Failures(CurrentSimYear, t) + 1;
             u = AvWater;
          end
          if (AvWater/Demand) < cfl
             CFailures(CurrentSimYear, t) = CFailures(CurrentSimYear,t) + 1;                          %Check into CF system
          end
          %Transfer tracking section, roll the vector
          nu = 1;
          for nu=12:-1:1
             TransferTracker(nu+1) = TransferTracker(nu);
          end
          TransferTracker(1) = 0;
          %Utilize transfers**********************************************
          if AvWater <= Demand              
             TransferTracker = zeros(13,1);
          end
          if AvWater > Demand && (AvWater - sum(TransferTracker)) < Demand
             NeededTransfers = Demand - (AvWater - sum(TransferTracker));
             nu = 13;
             while NeededTransfers > 0 && nu >= 1
                if TransferTracker(nu) ~= 0
                   if TransferTracker(nu) >= NeededTransfers
                      TransferTracker(nu) = TransferTracker(nu) - NeededTransfers;
                      NeededTransfers = 0;
                   else
                      NeededTransfers = NeededTransfers - TransferTracker(nu);
                      TransferTracker(nu) = 0;
                   end
                end
                
                nu = nu - 1;
                
             end
          end
          DroppedTransfers = TransferTracker(13);
          DroppedTransferTracker(CurrentSimYear, t, SimNumber) = DroppedTransfers;
          %End transfer utilization section********************************
          %Register Monthly Water Use
          MonthlyWaterUsageList(t) = u;
          %Non-negativity restrictions
          if (OldReservoirLevel+ResVariation) < 0
             CurrentResLevel = 0;
          else
             CurrentResLevel = OldReservoirLevel + ResVariation; 
          end
          ReservoirList(t) = CurrentResLevel;
          ResLevelTracker(CurrentSimYear, t, SimNumber) = CurrentResLevel;
          if (Inflows - Losses) > 0
             NewWater = Inflows - Losses;
          else
             NewWater = 0;
          end
          %Calculate Monthly allocation based on inflow - No
          %allocation if number of permanent rights have
          %already been allocated or f the reservoir level is
          %below its "lower limit"
          if CurrentResLevel >= 10000
             if (NewWater*0.825*FutureNrt(CurrentSimYear)/TWR) <= (FutureNrt(CurrentSimYear) - mean(NewMonthAllocationList)*length(NewMonthAllocationList))
                Nr = NewWater*0.825*FutureNrt(CurrentSimYear)/TWR;
             else
                Nr = Nrt - mean(NewMonthAllocationList)*length(NewMonthAllocationList);
             end
          else
             Nr = 0;
          end
          %Register monthly allocation
          NewMonthAllocationList(t) = Nr;
          AnnualSupplyi(t) = Nr;
          AnnualNewWateri(t) = NewWater;
                        
          %Market specific section
                        
          %If the expected available water is less than A% of
          %the demand, then water should be leased and/or
          %optioned (enough to meet B% f the unmet demand)
          %depending on the month in question, the option
          %exercise price and lease prices -- No allocation
          %is considered in November --
          
          
          
          
   
          Nro = Nro - DroppedTransfers;              
          if t <= OExerciseMonth
             NextMonthAvWater = Nro + sum(NewMonthAllocationList) + sum(NumberPurchasedLeasesList(CurrentSimYear,:)) - sum(MonthlyWaterUsageList);
          else
             NextMonthAvWater = Nro + sum(NewMonthAllocationList) + sum(NumberPurchasedLeasesList(CurrentSimYear,:)) - sum(MonthlyWaterUsageList) + Nx;
          end
                        
          if t <= (OExerciseMonth - 1)
             NextMonthExpAvWater = NextMonthAvWater + ExpAlloRestYearList(t) + No;
          else
             if t < 11
                NextMonthExpAvWater = NextMonthAvWater + ExpAlloRestYearList(t);
             end
          end         
          
          
          if t < OExerciseMonth
             RestSpringExpAvWater = NextMonthExpAvWater - ExpAlloRestYearList(OExerciseMonth) - No;
             if (RestSpringExpAvWater/(ExpYearDemandList(t) - ExpYearDemandList(OExerciseMonth))) <= A2
                if CurrentResLevel < 1430000
                   Pl = LeasePriceListLowRes((SimNumber - 1)*NumberYears + CurrentSimYear, t);
                else
                   Pl = LeasePriceListHigRes((SimNumber - 1)*NumberYears + CurrentSimYear, t);
                end
                if (B2*(ExpYearDemandList(t)-ExpYearDemandList(OExerciseMonth)) - RestSpringExpAvWater) > 0
                   N1 = B2*(ExpYearDemandList(t) - ExpYearDemandList(OExerciseMonth)) - RestSpringExpAvWater;
                   NextMonthAvWater = NextMonthAvWater + N1;
                   TransferTracker(1) = N1;
                else
                   N1 = 0;
                end
                LeaseCost(CurrentSimYear, t+1) = LeaseCost(CurrentSimYear, t+1) + N1*Pl;
             end
          end
                        
          if t >= OExerciseMonth & t < 12
             if CurrentResLevel < 1430000
                Pl = LeasePriceListLowRes((SimNumber - 1)*NumberYears + CurrentSimYear, t);
             else
                Pl = LeasePriceListHigRes((SimNumber - 1)*NumberYears + CurrentSimYear, t);
             end
             if t == OExerciseMonth
                Pl5(SimNumber, CurrentSimYear) = Pl;
             end
             if (NextMonthExpAvWater/ExpYearDemandList(t)) <= A    %change this line!?
                
                %Pl5(j) = Pl;               %Store May lease price           
                if t == OExerciseMonth
                   
                   if Px < Pl
                      if (B*ExpYearDemandList(t) - NextMonthExpAvWater) <= No
                         if (B*ExpYearDemandList(t) - NextMonthExpAvWater) > 0
                            Nx = B*ExpYearDemandList(t) - NextMonthExpAvWater;
                            NextMonthAvWater = NextMonthAvWater + Nx;
                         else
                            Nx = 0;
                         end
                         
                         %OptionCost = OptionCost + Nx*Px;
                      else
                         Nx = No;
                         NextMonthAvWater = NextMonthAvWater + Nx;
                         %Next few  lines (3)are being tried out
                         NextMonthExpAvWater = NextMonthAvWater + ExpAlloRestYearList(t);
                         if (B*ExpYearDemandList(t) - NextMonthExpAvWater) > 0
                            N1 = B*ExpYearDemandList(t) - NextMonthExpAvWater;
                            %if (B*ExpYearDemandList(t) - sum(NewMonthAllocationList) - sum(NumberPurchasedLeasesList) + sum(MonthlyWaterUsageList) - Nx + ExpAlloRestYearList(t)) > 0
                                %N1 = B*ExpYearDemandList(t) - sum(NewMonthAllocationList) - sum(NumberPurchasedLeasesList) + sum(MonthlyWaterUsageList) - Nx + ExpAlloRestYearList(t);
                            NextMonthAvWater = NextMonthAvWater + N1;
                         else
                            N1 = 0;
                         end
                         LeaseCost(CurrentSimYear, t+1) = LeaseCost(CurrentSimYear, t+1) + N1*Pl;
                         %OptionCost = OptionCost + Nx*Px;
                        
                      end
                      NxTracker(CurrentSimYear) = Nx;
                   else
                      if (B*ExpYearDemandList(t) - NextMonthExpAvWater) > 0
                         N1 = B*ExpYearDemandList(t) - NextMonthExpAvWater;
                         NextMonthAvWater = NextMonthAvWater + N1;
                      else
                         N1 = 0;
                      end
                      LeaseCost(CurrentSimYear, t+1) = LeaseCost(CurrentSimYear, t+1) + N1*Pl;
                   end
                   TransferTracker(1) = Nx + N1;
                else
                   if (B*ExpYearDemandList(t) - NextMonthExpAvWater) > 0
                      N1 = B*ExpYearDemandList(t) - NextMonthExpAvWater;
                      NextMonthAvWater = NextMonthAvWater + N1;
                   else
                      N1 = 0;
                   end
                   LeaseCost(CurrentSimYear, t+1) = LeaseCost(CurrentSimYear, t+1) + N1*Pl;
                   TransferTracker(1) = N1;
                end
             end
          end
          
          if t < 12
             AvWaterList(t+1) = NextMonthAvWater;   %b/c NextMonthAvWater accounts for dropped transfers, AvWater is all set
             NumberPurchasedLeasesList(CurrentSimYear, t+1) = N1;
          end

      
         if t == 12
            NowTime = SimNumber*CurrentSimYear;
            
            %if NowTime ~= BigTime
            if CurrentSimYear ~= NumberYears
               
               RoDiff = 1430000 - CurrentResLevel;
   
               if RoDiff < -505137
                  Po = Pohigh;
               elseif RoDiff > -505137 & RoDiff <= -422334
                  Po = 0.95*Pohigh + 0.05*Polow;
               elseif RoDiff > -422334 & RoDiff <= -377877
                  Po = 0.9*Pohigh + 0.1*Polow;
               elseif RoDiff > -377877 & RoDiff <= -345215
                  Po = 0.85*Pohigh + 0.15*Polow;
               elseif RoDiff > -345215 & RoDiff <= -318614
                  Po = 0.8*Pohigh + 0.2*Polow;
               elseif RoDiff > -318614 & RoDiff <= -294232
                  Po = 0.75*Pohigh + 0.25*Polow;
               elseif RoDiff > -294232 & RoDiff <= -272259
                  Po = 0.7*Pohigh + 0.3*Polow;
               elseif RoDiff > -272259 & RoDiff <= -251333
                  Po = 0.65*Pohigh + 0.35*Polow;
               elseif RoDiff > -251333 & RoDiff <= -231228
                  Po = 0.6*Pohigh + 0.4*Polow;
               elseif RoDiff > -231228 & RoDiff <= -211446
                  Po = 0.55*Pohigh + 0.45*Polow;
               elseif RoDiff > -211446 & RoDiff <= -191284
                  Po = 0.5*Pohigh + 0.5*Polow;
               elseif RoDiff > -191284 & RoDiff <= -171483
                  Po = 0.45*Pohigh + 0.55*Polow;
               elseif RoDiff > -171483 & RoDiff <= -150459
                  Po = 0.4*Pohigh + 0.6*Polow;
               elseif RoDiff > -150459 & RoDiff <= -129043
                  Po = 0.35*Pohigh + 0.65*Polow;
               elseif RoDiff > -129043 & RoDiff <= -105301
                  Po = 0.3*Pohigh + 0.7*Polow;
               elseif RoDiff > -105301 & RoDiff <= -79374
                  Po = 0.25*Pohigh + 0.75*Polow;
               elseif RoDiff > -79374 & RoDiff <= -49800
                  Po = 0.2*Pohigh + 0.8*Polow;
               elseif RoDiff > -49800 & RoDiff <= -11274
                  Po = 0.15*Pohigh + 0.85*Polow;
               elseif RoDiff > -11274 & RoDiff <= 45751
                  Po = 0.1*Pohigh + 0.9*Polow;
               elseif RoDiff > 45751 & RoDiff <= 179620
                  Po = 0.05*Pohigh + 0.95*Polow;
               elseif RoDiff > 179620
                  Po = Polow;
               end

               PoList(CurrentSimYear+1) = Po;
               To = NextMonthAvWater;       %These lines account for dropped transfers for t=12 -demand+allocation
               Nro = NextMonthAvWater;
               NewMonthAllocationList = [Nro];
               EndofYrWaterTracker(SimNumber, CurrentSimYear) = AvWater - DroppedTransfers;
               if (ENr1(CurrentSimYear+1)+ENr2(CurrentSimYear+1)+ENr3(CurrentSimYear+1)+ENr4(CurrentSimYear+1)) > (FutureNrt(CurrentSimYear+1)-Nro)
                  ENrt = FutureNrt(CurrentSimYear+1) - Nro;
               else
                  ENrt = ENr1(CurrentSimYear+1) + ENr2(CurrentSimYear+1) + ENr3(CurrentSimYear+1) + ENr4(CurrentSimYear+1);
               end
                   
               %Total expected available water
               Te1 = To + ENrt;           
      
               if CurrentResLevel <= 1430000                               %This if-statement used to be inside the next if-statement
                  Plo = LeasePriceListLowRes((SimNumber-1)*NumberYears+CurrentSimYear+1, 12);
               else
                  Plo = LeasePriceListHigRes((SimNumber-1)*NumberYears+CurrentSimYear+1, 12);
               end
               
               ToLeasePrice(SimNumber, CurrentSimYear+1) = Plo;
               
               d1 = [FutureMonthlyDemandMean(CurrentSimYear+1,1)+FutureMonthlyDemandMean(CurrentSimYear+1,2)+FutureMonthlyDemandMean(CurrentSimYear+1,3)+FutureMonthlyDemandMean(CurrentSimYear+1,4)+FutureMonthlyDemandMean(CurrentSimYear+1,5)];
                     
               if Te1/d1 <= A2
                        
                  if (B2*d1 - Te1) > 0
                     N1o = (B2*d1 - Te1);
                  else
                     N1o = 0;
                  end
                  LeaseCost(CurrentSimYear+1, 1) = LeaseCost(CurrentSimYear+1,1) + N1o*Plo;
               else
                  N1o = 0;
               end
               TransferTracker(1) = N1o;
                     
               %ToLeasePrice(j) = Plo;
               T1 = N1o + Nro;
               AvWaterList(1) = T1;
               t = 1;
                     
               NumberPurchasedLeasesList(CurrentSimYear+1, 1) = N1o;         %Change this index!!!!!!
            end
      
            OldReservoirLevel = CurrentResLevel;
            
         
         end               %closes 'if t==12' conditional
      
      
         
      
      
      end            %Closes month 1:12 loop
      
      %YearlyLeaseCostList(SimNumber,:) = LeaseCost';
      EarlyNewWater(SimNumber, CurrentSimYear) = AnnualSupplyi(1) + AnnualSupplyi(2) + AnnualSupplyi(3) + AnnualSupplyi(4) + AnnualSupplyi(5);
      LateNewWater(SimNumber, CurrentSimYear) = AnnualSupplyi(6) + AnnualSupplyi(7) + AnnualSupplyi(8) + AnnualSupplyi(9) + AnnualSupplyi(10) + AnnualSupplyi(11) + AnnualSupplyi(12);
      
      CurrentSimYear = CurrentSimYear + 1;
      
      
   end              %Loops back to start the next year of realization
   
   
   YearlyLeaseCostList(:,:,SimNumber) = LeaseCost;
   TotalMonthlyLeasesList(:,:,SimNumber) = NumberPurchasedLeasesList;
   TotalFailures(:,:,SimNumber) = Failures;
   TotalCFailures(:,:,SimNumber) = CFailures;
   TotalNxTracker(SimNumber, :) = NxTracker;
   TotalPoList(SimNumber, :) = PoList;
   
   %YearlyExercisedOptionsList(SimNumber, :) = 
   
   
   
   
   
end                %Closes big loop to repeat another realization


% Tally failures
FailureRecord = zeros(NumberYears, 12);
CFailureRecord = zeros(NumberYears, 12);
i = 1;
for i = 1:NumberYears
   for j = 1:12
      FailureRecord(i,j) = mean(TotalFailures(i,j,:));
      CFailureRecord(i,j) = mean(TotalCFailures(i,j,:));
      j = j + 1;
   end
   i = i + 1;
end

%Calculate reliabilities
Reliability = zeros(NumberYears, 1);
CReliability = zeros(NumberYears, 1);
i = 1;
for i = 1:NumberYears
   Reliability(i) = 1 - sum(FailureRecord(i,:))/12;
   CReliability(i) = 1 - sum(CFailureRecord(i,:))/12;
   i = i + 1;
end
meanReliability = mean(Reliability);
minReliability = min(Reliability);
%Calculate costs
AverageMonthlyLeaseCost = zeros(NumberYears, 12);
AverageYearlyLeaseCost = zeros(NumberYears, 1);


i = 1;
j = 1;
for i = 1:NumberYears
   for j = 1:12
      AverageMonthlyLeaseCost(i,j) = mean(YearlyLeaseCostList(i,j,:));
      j = j + 1;
   end
   AverageYearlyLeaseCost(i) = sum(AverageMonthlyLeaseCost(i,:));
   i = i + 1;
end

TotalYearlyLeaseCostList = zeros(NumberSims, NumberYears);  %Contains total lease cost for each year
i = 1;
for i = 1:NumberSims
   TempCostVector = sum(YearlyLeaseCostList(:,:,i), 2);
   TotalYearlyLeaseCostList(i,:) = TempCostVector;
   i = i + 1;
end

TotalAnnualCosts = zeros(NumberSims, NumberYears);
PermRightCosts = zeros(NumberSims, NumberYears);
PermRightCosts(1,:) = FutureNrt'*Pr;
i = 1;
for i = 2:NumberSims
   PermRightCosts(i,:) = PermRightCosts(1,:);
   i = i + 1;
end
TotalAnnualCosts = PermRightCosts + TotalPoList*No + ...
                   TotalNxTracker*Px + TotalYearlyLeaseCostList;
AverageAnnualCosts = sum(TotalAnnualCosts, 1)/NumberSims;
UltimateTotalAvgCost = sum(AverageAnnualCosts);
StdDevCost = std(TotalAnnualCosts, 0, 1);
SortedTotalAnnualCosts = sort(TotalAnnualCosts, 1);
nthpercentile = ceil(NumberSims*0.95);
uqpercentile = ceil(NumberSims*0.75);
lqpercentile = ceil(NumberSims*0.25);
varvector = zeros(NumberSims - nthpercentile, NumberYears);
i = 0;
for i = 0:(NumberSims - nthpercentile - 1)
   varvector(NumberSims - nthpercentile - i,:) = SortedTotalAnnualCosts(NumberSims-i,:);
   i = i + 1;
end
VAR = varvector(1,:);
CVAR = zeros(1,NumberYears);
CVAR = mean(varvector);
VARmean = CVAR./AverageAnnualCosts;
meanVARmean = mean(VARmean);

Z1 = EarlyNewWater - EarlyDemand;
Z2 = LateNewWater - LateDemand;
Z3 = ToLeasePrice;
Z4 = Pl5;
%Et08 = [26.44661017	25.98185966	25.76294857	25.58366073	25.47892248	25.42510149	25.39655318	24.36226807	25.38678746	25.39641054];
%Et015 = [16.25806452	21.85277833	22.34376944	22.68068427	22.94317686	23.16368246	23.35180377	23.52302737	23.67716478	23.81166887];
%Et022 = [16.25806452	16.40903839	17.39153003	18.35698133	19.12199847	19.74300561	20.25608549	20.68505383	21.06708354	21.3973809];
Et08 = mean(DecLeasePriceListLowRes);
Et015 = mean(DecLeasePriceListHigRes);
Et022 = mean(DecLeasePriceListHigRes);

if iRo == 800000
   ToLeaseMean = Et08;
elseif iRo == 1500000
   ToLeaseMean = Et015;
elseif iRo == 2200000
   ToLeaseMean = Et022;
end

EarlyDemandMean = zeros(NumberYears,1);
LateDemandMean = zeros(NumberYears, 1);
i = 1;
for i = 1:NumberYears
   EarlyDemandMean(i) = FutureMonthlyDemandMean(i,1) + FutureMonthlyDemandMean(i,2) + FutureMonthlyDemandMean(i,3) + FutureMonthlyDemandMean(i,4) + FutureMonthlyDemandMean(i,5);
   LateDemandMean(i) = FutureMonthlyDemandMean(i,6) + FutureMonthlyDemandMean(i,7) + FutureMonthlyDemandMean(i,8) + FutureMonthlyDemandMean(i,9) + FutureMonthlyDemandMean(i,10) + FutureMonthlyDemandMean(i,11) + FutureMonthlyDemandMean(i,12);
   i = i + 1;
end

%meanZ1 = (511598.65 - 161582.8)*0.825*FutureNrt/TWR - EarlyDemandMean;
%meanZ2 = (1158457.5 - 278566.3)*0.825*FutureNrt/TWR - LateDemandMean;      
meanZ3 = 21.226;
meanZ4 = 23.5546;
meanFutureNrt = mean(FutureNrt);
meanEarlyDemandMean = mean(EarlyDemandMean);
meanLateDemandMean = mean(LateDemandMean);
meanZ1 = (511598.65 - 161582.8)*0.825*meanFutureNrt/TWR - meanEarlyDemandMean;
meanZ2 = (1158457.5 - 278566.3)*0.825*meanFutureNrt/TWR - meanLateDemandMean; 
%meanZ1 = (511598.65 - 161582.8)*0.825*FutureNrt/TWR - EarlyDemandMean;
%meanZ2 = (1158457.5 - 278566.3)*0.825*FutureNrt/TWR - LateDemandMean; 

meantheta = zeros(NumberYears,1);
theta = zeros(NumberSims, NumberYears);
vartheta = zeros(NumberYears, 1);
varcost = zeros(NumberYears, 1);
%*************************************************************************
% if YearlyLeaseCostList(1,1,1) == 0
%    theta(:,1) = TotalAnnualCosts(:,1) - cstar(1,1)*(Z1(:,1) - meanZ1(1));
%    meantheta(1) = mean(theta(:,1));
%    vartheta(1) = var(theta(:,1));               %Variance of theta
%    varcost(1) = var(TotalAnnualCosts(:,1)); 
% else
%    theta(:,1) = TotalAnnualCosts(:,1) + cstar(1,1)*(Z1(:,1) - meanZ1(1)) + cstar(1,2)*(Z3(:,1) - ToLeaseMean(1));
%    meantheta(1) = mean(theta(:,1));
%    vartheta(1) = var(theta(:,1));               %Variance of theta
%    varcost(1) = var(TotalAnnualCosts(:,1));     %Variance of portfolio costs
% end
%    
% i = 2;
% for i = 2:NumberYears
%    theta(:,i) = TotalAnnualCosts(:,i) + cstar(i,1)*(Z1(:,i) - meanZ1) + cstar(i,2)*(Z2(:,i-1) - meanZ2);
%    meantheta(i) = mean(theta(:,i));
%    vartheta(i) = var(theta(:,i));               %Variance of theta
%    varcost(i) = var(TotalAnnualCosts(:,i));     %Variance of portfolio costs
%    i = i + 1;
% end
% 
% [R1, P1] = corrcoef(TotalAnnualCosts);
% [R2, P2] = corrcoef(theta);
% 
% % theta(:,1) = TotalAnnualCosts(:,1) + cstar(1)*(Z1(:,1) - meanZ1(1));% + cstar(2)*(Z2(:,1) - meanZ2(1));% + cstar(3)*(Z3(:,1) - meanZ3);% + cstar(4)*(Z4(:,1) - meanZ4);
% % meantheta(1) = mean(theta(:,1));
% % vartheta(1) = var(theta(:,1));               %Variance of theta
% % varcost(1) = var(TotalAnnualCosts(:,1));  
% % i = 1;
% % for i = 2:NumberYears
% %    %theta(:,i) = TotalAnnualCosts(:,i) + cstar(1)*(Z1(:,i) - meanZ1(i)) + cstar(2)*(Z2(:,i) - meanZ2(i)) + cstar(3)*(Z3(:,i) - meanZ3) + cstar(4)*(Z4(:,i) - meanZ4) + cstar(5)*(Z2(:,i-1) - meanZ2(i-1));%+ cstar(i,5)*(Z1(:,i-1) - meanZ1(i-1)) + cstar(i,6)*(Z2(:,i-1) - meanZ2(i-1));
% %    %theta(:,i) = TotalAnnualCosts(:,i) + cstar(1)*(Z1(:,i) - meanZ1(i)) + cstar(2)*(Z2(:,i-1) - meanZ2(i-1));%+ cstar(2)*(Z2(:,i) - meanZ2(i)) %cstar(3)*(Z3(:,i) - meanZ3) %+ cstar(4)*(Z4(:,i) - meanZ4) %+ cstar(i,5)*(Z1(:,i-1) - meanZ1(i-1)) + cstar(i,6)*(Z2(:,i-1) - meanZ2(i-1));
% %    theta(:,i) = TotalAnnualCosts(:,i) + cstar(1)*(Z1(:,i) - meanZ1) + cstar(2)*(Z2(:,i-1) - meanZ2);
% %    meantheta(i) = mean(theta(:,i));
% %    vartheta(i) = var(theta(:,i));               %Variance of theta
% %    varcost(i) = var(TotalAnnualCosts(:,i));     %Variance of portfolio costs
% %    i = i + 1;
% % end
% %**************************************************************************
% summeantheta = sum(meantheta);
% varredux = (varcost - vartheta)./varcost*100;
% meanvarredux = mean(varredux);

% storeaway = [summeantheta Nrt No A B A2 B2 gamma meanvarredux];
% dlmwrite('varred4.xls', storeaway, '-append');
% dlmwrite('varred4.txt', storeaway, '-append');

% cattheta = theta(:,1);
% catTotalAnnualCosts = TotalAnnualCosts(:,1);
% catZ1 = Z1(:,1);
% catZ2 = Z2(:,1);
% i = 2;
% for i = 2:NumberYears
%    cattheta = cat(1,cattheta, theta(:,i));
%    catTotalAnnualCosts = cat(1,catTotalAnnualCosts, TotalAnnualCosts(:,i));
%    catZ1 = cat(1,catZ1,Z1(:,i));
%    catZ2 = cat(1,catZ2,Z2(:,i));
%    i = i + 1;
% end
% varcatcosts = var(catTotalAnnualCosts);
% varcattheta = var(cattheta);
% varcatredux = (varcatcosts - varcattheta)/varcatcosts*100;
% PCvar = var(sum(TotalAnnualCosts,2));
% thetavar = var(sum(theta,2));
% PCvarredux = (PCvar - thetavar)/PCvar*100;
% BigCovMatrix3(:,1) = catZ1;
% BigCovMatrix3(:,2) = catZ2;
% BigCovMatrix3(:,3) = ones(size(catZ1));
% [B3, BINT,R,RINT, stats3] = regress(catTotalAnnualCosts, BigCovMatrix3);

% Experimental region from pilot study ************************************
%PrevSDinitial = 0;

% BigNumber = NumberYears*NumberSims;
% EarlySD = zeros(BigNumber,1);
% LateSD = zeros(BigNumber,1);
% ToPrice = zeros(BigNumber,1);
% T5LeasePrice = zeros(BigNumber,1);
% PrevLateSD = zeros(BigNumber,1);
% SinglePCList = zeros(BigNumber,1);
% i = 1;
% j = 1;
% for i = 1:NumberSims
%    for j = 1:NumberYears
%       EarlySD((i-1)*NumberYears+j) = Z1(i,j);
%       LateSD((i-1)*NumberYears+j) = Z2(i,j);
%       ToPrice((i-1)*NumberYears+j) = Z3(i,j);
%       T5LeasePrice((i-1)*NumberYears+j) = Z4(i,j);
%       SinglePCList((i-1)*NumberYears+j) = TotalAnnualCosts(i,j);
%       if j == 1
%          PrevLateSD((i-1)*NumberYears+j) = PrevSDinitial;
%       else
%          PrevLateSD((i-1)*NumberYears+j) = Z2(i, j-1);
%       end
%    j = j + 1;
%    end
%    i = i + 1;
% end
% 
% BigCovMatrix2 = zeros(NumberYears*NumberSims, 3);
% BigCovMatrix2(:,1) = EarlySD;
% %BigCovMatrix2(:,2) = LateSD;
% %BigCovMatrix2(:,3) = ToPrice;
% %BigCovMatrix2(:,4) = T5LeasePrice;
% BigCovMatrix2(:,2) = PrevLateSD;
% BigCovMatrix2(:,3) = ones(size(EarlySD));
% [B4, BINT,R,RINT, stats4] = regress(SinglePCList, BigCovMatrix2);
% cstar4 = zeros(3,1);
% cstar4(1) = -B4(1);
% cstar4(2) = -B4(2);
% cstar4(3) = -B4(3);


% End experimental pilot study section ************************************

%Sendout = [UltimateTotalAvgCost summeantheta PCvarredux EstimatedVarRedux Nrt No A2 B2 A B];
%dlmwrite('Difference2xb',Sendout,'-append')

%dlmwrite('R2d2',cstarR2,'-append')
%setheta = (thetavar)^.5/NumberSims^0.5;
%sePC = PCvar^0.5/(NumberSims+inumber)^0.5;

cstarR2 = 0;

% if cstarR2 == 1
%    fout = summeantheta;
%    appendTotalAnnualCosts = cat(1, TotalAnnualCosts, TotalAnnualCostsOut);
%    PCvar = var(sum(appendTotalAnnualCosts,2));
%    sePC = (PCvar)^0.5/(NumberSims+inumber)^0.5;
%    AverageAnnualCosts = sum(appendTotalAnnualCosts, 1)/(NumberSims+inumber);
%    UltimateTotalAvgCost = sum(AverageAnnualCosts);
%    StdDevCost = std(appendTotalAnnualCosts, 0, 1);
%    SortedTotalAnnualCosts = sort(appendTotalAnnualCosts, 1);
%    nthpercentile = ceil((NumberSims+inumber)*0.95);
%    uqpercentile = ceil((NumberSims+inumber)*0.75);
%    lqpercentile = ceil((NumberSims+inumber)*0.25);
%    varvector = zeros(NumberSims + inumber - nthpercentile, NumberYears);
%    i = 0;
%    for i = 0:(NumberSims + inumber - nthpercentile - 1)
%       varvector(NumberSims + inumber - nthpercentile - i,:) = SortedTotalAnnualCosts(NumberSims + inumber -i,:);
%       i = i + 1;
%    end
%    VAR = varvector(1,:);
%    CVAR = zeros(1,NumberYears);
%    CVAR = mean(varvector);
%    VARmean = CVAR./AverageAnnualCosts;
%    meanVARmean = mean(VARmean);
%    newReliability = Reliability*(NumberSims/(NumberSims+inumber)) + ReliabilityOut*(inumber/(NumberSims+inumber));
%    minReliability = min(newReliability);
% else
%    appendTotalAnnualCosts = cat(1, TotalAnnualCosts, TotalAnnualCostsOut);
%    AverageAnnualCosts = sum(appendTotalAnnualCosts, 1)/(NumberSims+inumber);
%    UltimateTotalAvgCost = sum(AverageAnnualCosts);
%    PCvar = var(sum(appendTotalAnnualCosts,2));
%    sePC = (PCvar)^0.5/(NumberSims+inumber)^0.5;
%    StdDevCost = std(appendTotalAnnualCosts, 0, 1);
%    SortedTotalAnnualCosts = sort(appendTotalAnnualCosts, 1);
%    nthpercentile = ceil((NumberSims+inumber)*0.95);
%    uqpercentile = ceil((NumberSims+inumber)*0.75);
%    lqpercentile = ceil((NumberSims+inumber)*0.25);
%    varvector = zeros(NumberSims + inumber - nthpercentile, NumberYears);
%    i = 0;
%    for i = 0:(NumberSims + inumber - nthpercentile - 1)
%       varvector(NumberSims + inumber - nthpercentile - i,:) = SortedTotalAnnualCosts(NumberSims + inumber -i,:);
%       i = i + 1;
%    end
%    VAR = varvector(1,:);
%    CVAR = zeros(1,NumberYears);
%    CVAR = mean(varvector);
%    VARmean = CVAR./AverageAnnualCosts;
%    meanVARmean = mean(VARmean);
%    fout = UltimateTotalAvgCost;
%    newReliability = Reliability*(NumberSims/(NumberSims+inumber)) + ReliabilityOut*(inumber/(NumberSims+inumber));
%    minReliability = min(newReliability);
%    fout = UltimateTotalAvgCost;
% end
%fout = summeantheta;
fout = UltimateTotalAvgCost;
%if minReliability >= 0.99 & meanVARmean <= 5
%if minReliability >= 0.01 & meanVARmean <= 5
if minReliability >= RELmin & meanVARmean <= CVARmax
   ifail = 0;
   icount = 1;    %exp
else
   ifail = 1;
   fout = NaN;
   icount = 1;    %exp
end

%kickout = [PCvarredux fout];
%dlmwrite('Actualvarredux3', kickout, '-append');

% Perform post-simulation data analysis
