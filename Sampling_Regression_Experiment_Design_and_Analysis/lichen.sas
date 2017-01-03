/* Using SAS to analyzed */
 
title 'Lichen densities';
options nodate noovp;
 
data lichen;
   input site $ method $ plot basal biomass_total biomass1 biomass2 section;
   datalines;
stratton      ST      93      16.2663      150      75      75      1
stratton      ST      94      31.6163      357.5      157.75      119.75      1
stratton      ST      95      20.7037      325      110      215      1
stratton      ST      96      20.5242      30      9.25      20.75      1
stratton      ST      97      15.8897      152.5      76      76.5      1
stratton      ST      98      24.5245      117.5      52.5      122.5      1
stratton      ST      99      7.0585      680      246.25      433.5      1
stratton      ST      100      40.2118      455      226.75      228.25      1
stratton      UC      101      11.6801      32.5      23.75      8.75      2
stratton      UC      102      21.2972      127.5      48.5      79      2
stratton      UC      103      26.9828      52.5      28.25      24.25      2
stratton      UC      104      45.2538      375      112.5      262.5      2
stratton      UC      105      52.4154      280      89.25      190.75      2
stratton      UC      106      46.5947      1532.5      432.25      1100.25      2
stratton      UC      107      62.072      2052.5      1103.5      949      2
stratton      IU      108      22.6882      2727.5      2587.5      140      3
stratton      IU      109      35.0276      1005      918.5      86.5      3
stratton      IU      110      4.1411      452.5      452.25      0.25      3
stratton      IU      111      25.9441      1102.5      937.5      165      3
stratton      IU      112      11.4148      27.5      27.25      0.25      3
stratton      IU      113      12.3528      800      720      80      3
stratton      IU      114      36.8608      675      587.5      87.5      3
Avola      ST      56      8.106      5      2.25      2.75      4
Avola      ST      57      41.892      55      26.75      28.25      4
Avola      ST      58      29.8659      60      26.25      33.75      4
Avola      ST      59      3.4722      2.5      1      1.5      4
Avola      ST      60      14.5027      180      57      123      4
Avola      ST      61      24.2841      202.5      61      141.5      4
Avola      ST      62      8.2774      25      7.5      17.5      4
Avola      ST      63      22.6829      2.5      0.75      1.75      4
Avola      UC      81      68.5897      502.5      135.5      367      5
Avola      UC      82      109.4751      677.5      205.75      471.75      5
Avola      UC      83      69.8325      1062.5      403.75      658.75      5
Avola      UC      84      79.7091      865      287.75      577.25      5
Avola      UC      85      10.2026      160      65.75      94.25      5
Avola      UC      86      0.1      0      0      0      5
Avola      UC      87      33.0648      232.5      69.75      162.75      5
Avola      UC      88      15.8446      752.5      165.75      586.75      5
Avola      UC      89      17.5977      27.5      8.25      19.25      5
Avola      UC      90      60.7468      42.5      13.25      29.25      5
Avola      UC      91      52.3089      527.5      188.25      339.25      5
Avola      UC      92      75.7852      115      32.575      82.425      5
Avola      ST      64      13.1435071      77.5      33.25      44.25      6
Avola      ST      65      3.465      30      12.5      17.5      6
Avola      ST      66      3.53131429      185      73.5      111.5      6
Avola      ST      67      23.7533      287.5      194.25      93.25      6
Avola      ST      68      31.3409      107.5      52.5      55      6
Avola      ST      69      47.7305      255      112.5      142.5      6
Avola      ST      70      59.4102      140      45.25      94.75      6
Avola      ST      71      60.4825      362.5      145      217.5      6
Avola      ST      72      48.646      15      4.75      10.25      6
Avola      IU      73      33.299      375      337.5      37.5      7
Avola      IU      74      2.4923      0      0      0      7
Avola      IU      75      7.9489      0      0      0      7
Avola      IU      76      1.1491      0      0      0      7
Avola      IU      77      17.3351      775      697.5      77.5      7
Avola      IU      78      8.1123      2.5      2.5      0      7
Avola      IU      79      23.2318      75      62.5      12.5      7
Avola      IU      80      60.7334      3715      3298.5      416.5      7
Berry      ST      115      7.6349      2.5      1.25      1.25      8
Berry      ST      116      31.9478      1412.5      664.5      748      8
Berry      ST      117      29.4745      510      345.5      164.5      8
Berry      ST      118      21.9232      2.5      1.25      1.25      8
Berry      ST      119      37.9685      1077.5      355.75      721.75      8
Berry      ST      120      22.2517      157.5      62.75      94.75      8
Berry      ST      121      37.9685      805      301.75      503.25      8
Berry      ST      122      4.0131      150      90      60      8
Berry      ST      123      22.3494      57.5      31.25      26.25      8
Berry      UC      124      36.6333      655      214.75      440.25      9
Berry      UC      125      55.8582      1725      407.5      1317.5      9
Berry      UC      126      40.5348      640      130.25      509.75      9
Berry      UC      127      63.8907      1702.5      495.75      1206.75      9
Berry      UC      128      56.5676      830      254      576      9
Raft-Berry      ST      1      24.6116      305      195.5      109.5      10
Raft-Berry      ST      2      20.0646      752.5      347.5      405      10
Raft-Berry      ST      3      42.0752      265      99.25      165.75      10
Raft-Berry      ST      4      22.637      482.5      127      355.5      10
Raft-Berry      ST      5      3.3084      2.5      2.5      0      10
Raft-Berry      ST      6      6.0553      7.5      2.25      5.25      10
Raft-Berry      ST      7      1.3443      5      1      4      10
Raft-Berry      ST      8      11.7972      32.5      9.75      22.75      10
Raft-Berry      ST      9      3.9229      5      1.5      3.5      10
Raft-Berry      ST      10      43.9624      690      207      483      10
Raft-Berry      ST      11      31.6843      225      90      135      10
Raft-Berry      ST      12      45.0427      1200      360      840      10
Raft-Berry      ST      13      29.1385      502.5      150.75      351.75      10
Raft-Berry      ST      14      28.2794      62.5      18.75      43.75      10
Raft-Berry      ST      15      28.9133      77.5      28.25      49.25      10
Raft-Berry      ST      16      4.1154      5      2.25      2.75      10
Raft-Berry      ST      17      17.9807      5      1.5      3.5      10
Raft-Berry      ST      18      25.9843      232.5      84.75      147.75      10
Raft-Berry      IU      19      13.475      315      219      96      11
Raft-Berry      IU      20      10.5603      32.5      28.75      3.75      11
Raft-Berry      IU      21      21.8095      202.5      162      40.5      11
Raft-Berry      IU      22      1.4515      0      0      0      11
Raft-Berry      IU      23      1.0931      0      0      0      11
Raft-Berry      UC      24      100.3188      192.5      38.5      154      12
Raft-Berry      UC      25      29.0907      152.5      45.75      106.75      12
Raft-Berry      UC      26      24.1408      35      10.5      24.5      12
Raft-Berry      UC      27      98.8113      1005      301.5      703.5      12
Raft-Berry      UC      28      108.6666      805      256.5      548.5      12
Raft-Berry      UC      29      38.6974      1502.5      450.75      1051.75      12
Raft-Berry      IU      130      12.9233      0      0      0      13
Raft-Berry      IU      131      13.9698      800      720      80      13
Raft-Berry      IU      132      43.453      452.5      407.25      45.25      13
Raft-Berry      IU      133      24.1104      25      22.5      2.5      13
Raft-Berry      IU      134      19.4397      2.5      2.25      0.25      13
Raft-Berry      IU      135      41.1109      1075      970      105      13
Paturages      ST      30      36.3857      57.5      27.75      29.75      14
Paturages      ST      31      41.7569      652.5      288.75      363.75      14
Paturages      ST      32      28.5907      252.5      123.5      129      14
Paturages      ST      33      17.4326      925      327.5      597.5      14
Paturages      ST      34      2.1712      2.5      1.5      1      14
Paturages      ST      35      10.9137      27.5      13.5      14      14
Paturages      ST      36      12.8377      177.5      71      106.5      14
Paturages      ST      37      31.366      1425      445      980      14
Paturages      ST      38      32.2165      30      11.75      18.25      14
Paturages      IU      39      4.0658      2.5      2      0.5      15
Paturages      IU      40      57.5848      2727.5      2189.5      538      15
Paturages      IU      41      1.9494      0      0      0      15
Paturages      IU      42      11.7229      450      360      90      15
Paturages      IU      43      4.7591      0      0      0      15
Paturages      IU      44      22.4198      802.5      642      160.5      15
Paturages      IU      45      6.3006      25      20      5      15
Paturages      IU      46      10.8448      5      4      1      15
Paturages      IU      47      6.2468      0      0      0      15
Paturages      IU      48      12.2896      852.5      762.25      90.25      15
Paturages      IU      49      4.7496      0      0      0      15
Paturages      UC      50      76.5194      1160      411.25      748.75      16
Paturages      UC      51      68.3276      85      45.25      39.75      16
Paturages      UC      52      38.6245      45      11.5      33.5      16
Paturages      UC      53      33.2978      55      16.5      38.5      16
Paturages      UC      54      19.6458      15      6      9      16
Paturages      UC      55      48.1945      955      361.75      593.25      16
;;;;

proc print data=lichen(obs=40);
   title2 'raw data';

proc tabulate data=lichen;
   title2 'basic summary statistics';
   class site method section;
   var basal;
   table site , method*basal*(n*f=6.0 mean*f=7.1);

*---------------------- Approximate analysis on means ---------------------------;

proc sort data=lichen; by section;
proc means data=lichen noprint;
   by section;
   var basal;
   output out=mbasal mean=mbasal std=sbasal;
   id site method;

proc print data=mbasal;
   title2 'average over the pseudo-replicates';

proc mixed data=mbasal;
   class site method section;
   model mbasal = site method / ddfm =satterth;
   random site*method;
   lsmeans method / cl adjust=tukey;  /* requests lsmeans */
 



*---------------------- Exact analysis on the individual plots -------------------;
 
proc mixed data=lichen;
   title2 'Analysis using the individual plots';
   class site method section plot;
   model basal = site method / ddfm=satterth;
   random site*method section(site*method);
   lsmeans method / cl adjust=tukey;






*---------------------- Exact analysis on the individual plots -------------------;
*---------------------- Incorporate additional information from intra-block analysis ;

proc mixed data=lichen;
   title2 'Analysis using the individual plots -- Intra block information now included';
   class site method section plot;
   model basal = method  / ddfm=satterth;
   random site site*method section(site*method);
   lsmeans method / cl adjust=tukey;


