% agricultural data on 24,473 zip code areas
% land usage and animal inventories

% variables are:
% col1 =  latitude centroid
% col2 =  longitude centroid
% col3 =  zip code
% col4 =  state code, 1 to 48 alphabetical order
% col5 =  acres harvested, total farms
% col6 =  acres in Conservation Reserve or Wetlands Reserve Programs total farms
% col7 =  acres of Cropland idle, total farms
% col8 =  pasture, Cropland used for pasture or grazing total farms
% col9 =  rangeland, Pasture and rangeland other than cropland or woodland pastured total farms
% col10 = woodland, Total woodland total farms
% col11 = soilimprove, Cropland in cover crops legumes and soil improvement
%                      grasses not harvested and not pastured total farms
% col12 = failed, Cropland on which all crops failed total farms
% col13 = fallow, Cropland in cultivated summer fallow total farms
% col14 = otherland,  All other land total farms
% col15 = farms, all farms
% col16 = farms_small, 1 to 49 acres
% col17 = farms_medium, 50 to 999 acres
% col18 = farms_large, 1000 acres or more
% col19 = beef_cows, Beef cow inventory total farms
% col20 = milk_cows, Milk cow inventory total farms
% col21 = hogs_pigs, Hogs and pigs inventory total farms
% col22 = sheep_lambs, Sheep and lambs inventory total farms
% col23 = hens_pullets, Hens & pullets laying age inventory total farms
% col24 = horses_ponies, Horses and ponies of  all ages inventory total farms
% col25 = owner, Full owners
% col26 = pop, total zip-code area population
% col27 = rural_pop, population census classified as rural
% col28 = farm_pop, population census classified as farm

load agriculture.data;

vnames = strvcat('harvested','constant','conservation','idle','pasture','rangeland', ...
'woodland','soilimprove','failed','fallow','otherland','farms','beef_cows','milk_cows', ...
'hogs_pigs','sheep_lambs','hens_pullets','horses_ponies','owner','rural_pop','farm_pop');


latt = agriculture(:,1);
long = agriculture(:,2);
zip  = agriculture(:,3);
st_code = agriculture(:,4);

xdata = [agriculture(:,5:15) agriculture(:,19:end-4) agriculture(:,end-2:end)];
% pitches farms_small, farms_medium, farms_large and pop
tmp = xdata(:,1:10);
acres = sum(tmp');
total_acres = acres';

% find non-zero total acres zip-code areas
nzip = find(total_acres > 0);
dat  = xdata(nzip,:);


y = log(dat(:,1)+1); % acres harvested, add unity to avoid log(0)
n = length(y);

xtmp = dat(:,2:end);
xmat = log(xtmp + ones(size(xtmp))); % add unity to avoid log(0)

x = [ones(n,1) xmat ];

[nobs nvars] = size(x);

result = ols(y,x);
prt(result,vnames);

