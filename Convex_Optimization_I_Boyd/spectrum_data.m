% data for spectrum analysis problem.

% code for plotting qmax and qmin.
% figure; hold on;
% for i = 1:n
%     plot([i,i],[qmax(i),qmin(i)],'o-');
% end
% axis([0,11,0,1]);

n = 10;
m = 250;
S = [...
   0.0000   0.0000   0.0011   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0000   0.0014   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0001   0.0000 ...
   ;
   0.0000   0.0000   0.0019   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0001   0.0000 ...
   ;
   0.0000   0.0000   0.0024   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0002   0.0000 ...
   ;
   0.0000   0.0000   0.0031   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0003   0.0000 ...
   ;
   0.0000   0.0000   0.0040   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0005   0.0000 ...
   ;
   0.0000   0.0000   0.0052   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0007   0.0000 ...
   ;
   0.0000   0.0000   0.0066   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0011   0.0000 ...
   ;
   0.0000   0.0000   0.0085   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0016   0.0000 ...
   ;
   0.0000   0.0000   0.0108   0.0000   0.0000 ...
   0.0000   0.0000   0.0001   0.0024   0.0000 ...
   ;
   0.0000   0.0000   0.0140   0.0000   0.0000 ...
   0.0000   0.0000   0.0001   0.0035   0.0000 ...
   ;
   0.0000   0.0000   0.0182   0.0000   0.0000 ...
   0.0000   0.0000   0.0002   0.0050   0.0000 ...
   ;
   0.0000   0.0000   0.0240   0.0000   0.0000 ...
   0.0000   0.0000   0.0003   0.0071   0.0000 ...
   ;
   0.0000   0.0000   0.0319   0.0000   0.0000 ...
   0.0000   0.0000   0.0004   0.0099   0.0000 ...
   ;
   0.0000   0.0000   0.0429   0.0000   0.0000 ...
   0.0000   0.0000   0.0007   0.0136   0.0000 ...
   ;
   0.0000   0.0000   0.0581   0.0000   0.0000 ...
   0.0000   0.0000   0.0011   0.0184   0.0000 ...
   ;
   0.0000   0.0000   0.0790   0.0000   0.0000 ...
   0.0000   0.0000   0.0016   0.0246   0.0000 ...
   ;
   0.0000   0.0000   0.1072   0.0000   0.0000 ...
   0.0000   0.0000   0.0024   0.0325   0.0000 ...
   ;
   0.0000   0.0000   0.1442   0.0000   0.0000 ...
   0.0000   0.0000   0.0036   0.0423   0.0000 ...
   ;
   0.0000   0.0000   0.1917   0.0000   0.0000 ...
   0.0000   0.0000   0.0052   0.0544   0.0000 ...
   ;
   0.0000   0.0000   0.2505   0.0000   0.0000 ...
   0.0000   0.0000   0.0074   0.0689   0.0000 ...
   ;
   0.0000   0.0000   0.3207   0.0000   0.0000 ...
   0.0000   0.0000   0.0105   0.0862   0.0000 ...
   ;
   0.0000   0.0000   0.4014   0.0000   0.0000 ...
   0.0000   0.0000   0.0144   0.1064   0.0000 ...
   ;
   0.0000   0.0000   0.4900   0.0000   0.0000 ...
   0.0000   0.0000   0.0196   0.1295   0.0000 ...
   ;
   0.0000   0.0000   0.5826   0.0000   0.0000 ...
   0.0000   0.0000   0.0262   0.1555   0.0000 ...
   ;
   0.0000   0.0000   0.6740   0.0000   0.0000 ...
   0.0000   0.0000   0.0345   0.1843   0.0000 ...
   ;
   0.0000   0.0000   0.7585   0.0000   0.0000 ...
   0.0000   0.0000   0.0447   0.2154   0.0000 ...
   ;
   0.0000   0.0000   0.8301   0.0000   0.0000 ...
   0.0000   0.0000   0.0569   0.2485   0.0000 ...
   ;
   0.0000   0.0000   0.8836   0.0000   0.0000 ...
   0.0000   0.0000   0.0713   0.2828   0.0000 ...
   ;
   0.0000   0.0000   0.9156   0.0000   0.0000 ...
   0.0000   0.0000   0.0880   0.3176   0.0000 ...
   ;
   0.0000   0.0000   0.9246   0.0001   0.0000 ...
   0.0000   0.0000   0.1067   0.3518   0.0000 ...
   ;
   0.0000   0.0000   0.9112   0.0005   0.0000 ...
   0.0000   0.0000   0.1274   0.3845   0.0000 ...
   ;
   0.0000   0.0000   0.8785   0.0016   0.0000 ...
   0.0000   0.0000   0.1496   0.4147   0.0000 ...
   ;
   0.0000   0.0000   0.8311   0.0046   0.0000 ...
   0.0000   0.0000   0.1729   0.4413   0.0000 ...
   ;
   0.0000   0.0000   0.7742   0.0122   0.0000 ...
   0.0000   0.0000   0.1966   0.4633   0.0000 ...
   ;
   0.0000   0.0000   0.7135   0.0290   0.0000 ...
   0.0000   0.0000   0.2200   0.4799   0.0000 ...
   ;
   0.0000   0.0000   0.6539   0.0620   0.0000 ...
   0.0000   0.0000   0.2422   0.4905   0.0000 ...
   ;
   0.0000   0.0000   0.5992   0.1192   0.0000 ...
   0.0001   0.0000   0.2623   0.4948   0.0000 ...
   ;
   0.0000   0.0000   0.5517   0.2062   0.0000 ...
   0.0002   0.0000   0.2795   0.4925   0.0000 ...
   ;
   0.0000   0.0000   0.5125   0.3203   0.0000 ...
   0.0003   0.0000   0.2931   0.4838   0.0000 ...
   ;
   0.0000   0.0000   0.4814   0.4473   0.0000 ...
   0.0005   0.0000   0.3024   0.4692   0.0000 ...
   ;
   0.0000   0.0000   0.4575   0.5614   0.0000 ...
   0.0009   0.0000   0.3070   0.4491   0.0000 ...
   ;
   0.0000   0.0000   0.4391   0.6333   0.0000 ...
   0.0015   0.0000   0.3066   0.4246   0.0000 ...
   ;
   0.0000   0.0000   0.4248   0.6420   0.0000 ...
   0.0026   0.0000   0.3013   0.3965   0.0000 ...
   ;
   0.0000   0.0000   0.4128   0.5849   0.0000 ...
   0.0041   0.0000   0.2914   0.3659   0.0000 ...
   ;
   0.0000   0.0000   0.4020   0.4789   0.0000 ...
   0.0066   0.0000   0.2772   0.3341   0.0001 ...
   ;
   0.0000   0.0000   0.3912   0.3524   0.0000 ...
   0.0103   0.0000   0.2595   0.3020   0.0001 ...
   ;
   0.0000   0.0000   0.3798   0.2331   0.0000 ...
   0.0157   0.0000   0.2390   0.2708   0.0001 ...
   ;
   0.0000   0.0000   0.3672   0.1385   0.0000 ...
   0.0235   0.0000   0.2166   0.2413   0.0001 ...
   ;
   0.0000   0.0000   0.3533   0.0740   0.0000 ...
   0.0343   0.0000   0.1932   0.2143   0.0002 ...
   ;
   0.0000   0.0000   0.3381   0.0355   0.0000 ...
   0.0492   0.0000   0.1695   0.1905   0.0003 ...
   ;
   0.0000   0.0000   0.3217   0.0153   0.0000 ...
   0.0691   0.0000   0.1463   0.1704   0.0004 ...
   ;
   0.0000   0.0000   0.3041   0.0059   0.0000 ...
   0.0949   0.0000   0.1243   0.1542   0.0006 ...
   ;
   0.0000   0.0000   0.2858   0.0021   0.0000 ...
   0.1278   0.0000   0.1038   0.1422   0.0008 ...
   ;
   0.0000   0.0000   0.2668   0.0006   0.0000 ...
   0.1684   0.0000   0.0854   0.1344   0.0012 ...
   ;
   0.0000   0.0000   0.2475   0.0002   0.0000 ...
   0.2173   0.0000   0.0691   0.1308   0.0017 ...
   ;
   0.0000   0.0000   0.2282   0.0000   0.0000 ...
   0.2745   0.0000   0.0550   0.1312   0.0024 ...
   ;
   0.0000   0.0000   0.2090   0.0000   0.0000 ...
   0.3397   0.0000   0.0431   0.1355   0.0033 ...
   ;
   0.0000   0.0000   0.1902   0.0000   0.0000 ...
   0.4116   0.0000   0.0332   0.1434   0.0047 ...
   ;
   0.0001   0.0000   0.1720   0.0000   0.0000 ...
   0.4884   0.0000   0.0252   0.1547   0.0067 ...
   ;
   0.0001   0.0000   0.1545   0.0000   0.0000 ...
   0.5674   0.0000   0.0188   0.1691   0.0094 ...
   ;
   0.0001   0.0000   0.1379   0.0000   0.0000 ...
   0.6456   0.0000   0.0138   0.1863   0.0131 ...
   ;
   0.0002   0.0000   0.1223   0.0000   0.0000 ...
   0.7193   0.0000   0.0100   0.2059   0.0183 ...
   ;
   0.0003   0.0000   0.1078   0.0000   0.0000 ...
   0.7847   0.0000   0.0071   0.2278   0.0254 ...
   ;
   0.0004   0.0000   0.0944   0.0000   0.0000 ...
   0.8383   0.0000   0.0049   0.2516   0.0350 ...
   ;
   0.0006   0.0000   0.0821   0.0000   0.0000 ...
   0.8769   0.0000   0.0034   0.2768   0.0477 ...
   ;
   0.0009   0.0000   0.0710   0.0000   0.0000 ...
   0.8983   0.0000   0.0023   0.3032   0.0644 ...
   ;
   0.0013   0.0000   0.0610   0.0000   0.0000 ...
   0.9011   0.0000   0.0015   0.3304   0.0859 ...
   ;
   0.0018   0.0000   0.0520   0.0000   0.0000 ...
   0.8851   0.0000   0.0010   0.3579   0.1132 ...
   ;
   0.0026   0.0000   0.0441   0.0000   0.0000 ...
   0.8514   0.0000   0.0006   0.3854   0.1472 ...
   ;
   0.0036   0.0000   0.0372   0.0000   0.0000 ...
   0.8019   0.0000   0.0004   0.4123   0.1888 ...
   ;
   0.0049   0.0000   0.0311   0.0000   0.0000 ...
   0.7396   0.0000   0.0003   0.4384   0.2387 ...
   ;
   0.0067   0.0000   0.0259   0.0000   0.0000 ...
   0.6680   0.0000   0.0002   0.4630   0.2974 ...
   ;
   0.0091   0.0000   0.0214   0.0000   0.0000 ...
   0.5908   0.0000   0.0001   0.4859   0.3650 ...
   ;
   0.0122   0.0000   0.0176   0.0001   0.0000 ...
   0.5117   0.0000   0.0001   0.5065   0.4410 ...
   ;
   0.0161   0.0000   0.0143   0.0001   0.0000 ...
   0.4339   0.0000   0.0000   0.5246   0.5247 ...
   ;
   0.0211   0.0000   0.0116   0.0001   0.0000 ...
   0.3604   0.0000   0.0000   0.5397   0.6145 ...
   ;
   0.0274   0.0000   0.0094   0.0002   0.0000 ...
   0.2930   0.0000   0.0000   0.5517   0.7084 ...
   ;
   0.0352   0.0000   0.0075   0.0002   0.0000 ...
   0.2334   0.0000   0.0000   0.5601   0.8039 ...
   ;
   0.0448   0.0000   0.0060   0.0003   0.0000 ...
   0.1820   0.0000   0.0000   0.5650   0.8981 ...
   ;
   0.0565   0.0000   0.0047   0.0004   0.0000 ...
   0.1389   0.0000   0.0000   0.5661   0.9879 ...
   ;
   0.0704   0.0000   0.0037   0.0005   0.0000 ...
   0.1039   0.0000   0.0000   0.5635   1.0702 ...
   ;
   0.0870   0.0000   0.0029   0.0007   0.0000 ...
   0.0761   0.0000   0.0001   0.5572   1.1422 ...
   ;
   0.1063   0.0000   0.0022   0.0010   0.0000 ...
   0.0545   0.0000   0.0001   0.5473   1.2014 ...
   ;
   0.1287   0.0000   0.0017   0.0013   0.0000 ...
   0.0383   0.0000   0.0001   0.5341   1.2461 ...
   ;
   0.1542   0.0000   0.0013   0.0017   0.0000 ...
   0.0263   0.0000   0.0002   0.5177   1.2751 ...
   ;
   0.1830   0.0000   0.0010   0.0023   0.0000 ...
   0.0177   0.0000   0.0002   0.4986   1.2883 ...
   ;
   0.2149   0.0000   0.0008   0.0030   0.0000 ...
   0.0117   0.0000   0.0003   0.4769   1.2864 ...
   ;
   0.2499   0.0000   0.0006   0.0038   0.0000 ...
   0.0075   0.0000   0.0004   0.4533   1.2706 ...
   ;
   0.2878   0.0000   0.0004   0.0049   0.0000 ...
   0.0048   0.0000   0.0006   0.4279   1.2429 ...
   ;
   0.3280   0.0000   0.0003   0.0063   0.0000 ...
   0.0030   0.0000   0.0008   0.4013   1.2055 ...
   ;
   0.3702   0.0000   0.0002   0.0080   0.0000 ...
   0.0018   0.0000   0.0010   0.3739   1.1611 ...
   ;
   0.4136   0.0000   0.0002   0.0100   0.0000 ...
   0.0011   0.0000   0.0014   0.3460   1.1121 ...
   ;
   0.4575   0.0000   0.0001   0.0126   0.0000 ...
   0.0006   0.0000   0.0018   0.3181   1.0606 ...
   ;
   0.5010   0.0000   0.0001   0.0156   0.0000 ...
   0.0004   0.0001   0.0023   0.2906   1.0087 ...
   ;
   0.5433   0.0000   0.0001   0.0192   0.0000 ...
   0.0002   0.0001   0.0030   0.2636   0.9577 ...
   ;
   0.5832   0.0000   0.0000   0.0236   0.0000 ...
   0.0001   0.0001   0.0039   0.2376   0.9087 ...
   ;
   0.6198   0.0000   0.0000   0.0287   0.0000 ...
   0.0001   0.0002   0.0050   0.2128   0.8623 ...
   ;
   0.6521   0.0000   0.0000   0.0346   0.0000 ...
   0.0000   0.0002   0.0063   0.1892   0.8185 ...
   ;
   0.6793   0.0000   0.0000   0.0415   0.0000 ...
   0.0000   0.0003   0.0079   0.1672   0.7773 ...
   ;
   0.7007   0.0000   0.0000   0.0494   0.0000 ...
   0.0000   0.0004   0.0099   0.1468   0.7383 ...
   ;
   0.7154   0.0000   0.0000   0.0585   0.0000 ...
   0.0000   0.0006   0.0123   0.1280   0.7010 ...
   ;
   0.7233   0.0000   0.0000   0.0687   0.0000 ...
   0.0000   0.0008   0.0152   0.1109   0.6650 ...
   ;
   0.7239   0.0000   0.0000   0.0801   0.0000 ...
   0.0000   0.0010   0.0186   0.0954   0.6296 ...
   ;
   0.7173   0.0000   0.0000   0.0927   0.0000 ...
   0.0000   0.0013   0.0226   0.0816   0.5946 ...
   ;
   0.7037   0.0000   0.0000   0.1066   0.0000 ...
   0.0000   0.0016   0.0274   0.0693   0.5597 ...
   ;
   0.6835   0.0000   0.0000   0.1217   0.0000 ...
   0.0000   0.0021   0.0330   0.0584   0.5248 ...
   ;
   0.6573   0.0000   0.0000   0.1380   0.0000 ...
   0.0000   0.0026   0.0394   0.0490   0.4897 ...
   ;
   0.6258   0.0000   0.0000   0.1554   0.0000 ...
   0.0000   0.0033   0.0468   0.0408   0.4547 ...
   ;
   0.5899   0.0000   0.0000   0.1737   0.0000 ...
   0.0000   0.0040   0.0552   0.0337   0.4198 ...
   ;
   0.5505   0.0000   0.0000   0.1929   0.0000 ...
   0.0000   0.0049   0.0647   0.0277   0.3853 ...
   ;
   0.5086   0.0000   0.0000   0.2127   0.0000 ...
   0.0001   0.0059   0.0753   0.0226   0.3515 ...
   ;
   0.4653   0.0000   0.0000   0.2329   0.0000 ...
   0.0001   0.0070   0.0872   0.0183   0.3186 ...
   ;
   0.4214   0.0000   0.0000   0.2532   0.0000 ...
   0.0001   0.0083   0.1002   0.0148   0.2869 ...
   ;
   0.3779   0.0000   0.0000   0.2733   0.0000 ...
   0.0002   0.0097   0.1144   0.0118   0.2566 ...
   ;
   0.3355   0.0000   0.0000   0.2930   0.0000 ...
   0.0002   0.0113   0.1298   0.0094   0.2280 ...
   ;
   0.2950   0.0000   0.0000   0.3119   0.0000 ...
   0.0003   0.0129   0.1463   0.0074   0.2013 ...
   ;
   0.2569   0.0000   0.0000   0.3298   0.0000 ...
   0.0004   0.0146   0.1639   0.0058   0.1764 ...
   ;
   0.2216   0.0000   0.0000   0.3462   0.0000 ...
   0.0006   0.0164   0.1823   0.0045   0.1536 ...
   ;
   0.1893   0.0000   0.0000   0.3609   0.0000 ...
   0.0008   0.0183   0.2015   0.0035   0.1328 ...
   ;
   0.1604   0.0000   0.0000   0.3736   0.0000 ...
   0.0011   0.0201   0.2214   0.0027   0.1140 ...
   ;
   0.1348   0.0000   0.0000   0.3840   0.0000 ...
   0.0014   0.0219   0.2415   0.0021   0.0972 ...
   ;
   0.1125   0.0000   0.0000   0.3920   0.0000 ...
   0.0019   0.0236   0.2619   0.0016   0.0824 ...
   ;
   0.0934   0.0000   0.0000   0.3973   0.0000 ...
   0.0024   0.0251   0.2821   0.0012   0.0693 ...
   ;
   0.0774   0.0000   0.0000   0.3999   0.0000 ...
   0.0032   0.0265   0.3018   0.0009   0.0579 ...
   ;
   0.0644   0.0000   0.0000   0.3998   0.0000 ...
   0.0041   0.0277   0.3209   0.0007   0.0480 ...
   ;
   0.0541   0.0000   0.0000   0.3968   0.0000 ...
   0.0052   0.0287   0.3390   0.0005   0.0396 ...
   ;
   0.0463   0.0000   0.0000   0.3911   0.0000 ...
   0.0065   0.0294   0.3558   0.0004   0.0324 ...
   ;
   0.0409   0.0000   0.0000   0.3828   0.0000 ...
   0.0082   0.0298   0.3709   0.0003   0.0263 ...
   ;
   0.0377   0.0000   0.0000   0.3720   0.0000 ...
   0.0101   0.0299   0.3843   0.0002   0.0212 ...
   ;
   0.0367   0.0000   0.0000   0.3590   0.0000 ...
   0.0124   0.0297   0.3955   0.0001   0.0170 ...
   ;
   0.0377   0.0001   0.0000   0.3441   0.0000 ...
   0.0151   0.0293   0.4044   0.0001   0.0135 ...
   ;
   0.0407   0.0002   0.0000   0.3275   0.0000 ...
   0.0182   0.0286   0.4109   0.0001   0.0107 ...
   ;
   0.0458   0.0005   0.0000   0.3095   0.0000 ...
   0.0218   0.0278   0.4147   0.0000   0.0084 ...
   ;
   0.0528   0.0010   0.0000   0.2905   0.0000 ...
   0.0258   0.0267   0.4159   0.0000   0.0065 ...
   ;
   0.0621   0.0020   0.0000   0.2707   0.0000 ...
   0.0302   0.0257   0.4144   0.0000   0.0051 ...
   ;
   0.0736   0.0039   0.0000   0.2505   0.0000 ...
   0.0351   0.0246   0.4102   0.0000   0.0039 ...
   ;
   0.0874   0.0072   0.0000   0.2302   0.0000 ...
   0.0404   0.0236   0.4034   0.0000   0.0030 ...
   ;
   0.1038   0.0128   0.0000   0.2100   0.0000 ...
   0.0461   0.0228   0.3942   0.0000   0.0023 ...
   ;
   0.1227   0.0219   0.0000   0.1903   0.0000 ...
   0.0521   0.0223   0.3826   0.0000   0.0017 ...
   ;
   0.1445   0.0358   0.0000   0.1713   0.0000 ...
   0.0583   0.0223   0.3691   0.0000   0.0013 ...
   ;
   0.1692   0.0560   0.0000   0.1530   0.0000 ...
   0.0647   0.0227   0.3536   0.0000   0.0009 ...
   ;
   0.1968   0.0841   0.0000   0.1358   0.0000 ...
   0.0711   0.0238   0.3367   0.0000   0.0007 ...
   ;
   0.2275   0.1208   0.0000   0.1196   0.0000 ...
   0.0775   0.0255   0.3185   0.0000   0.0005 ...
   ;
   0.2611   0.1665   0.0000   0.1047   0.0000 ...
   0.0836   0.0281   0.2993   0.0000   0.0004 ...
   ;
   0.2976   0.2197   0.0000   0.0910   0.0000 ...
   0.0894   0.0314   0.2794   0.0000   0.0003 ...
   ;
   0.3369   0.2779   0.0000   0.0785   0.0000 ...
   0.0947   0.0354   0.2592   0.0000   0.0002 ...
   ;
   0.3787   0.3368   0.0000   0.0672   0.0000 ...
   0.0994   0.0402   0.2389   0.0000   0.0001 ...
   ;
   0.4228   0.3911   0.0000   0.0572   0.0000 ...
   0.1035   0.0456   0.2187   0.0000   0.0001 ...
   ;
   0.4686   0.4352   0.0000   0.0483   0.0000 ...
   0.1066   0.0514   0.1990   0.0000   0.0001 ...
   ;
   0.5157   0.4640   0.0000   0.0405   0.0000 ...
   0.1089   0.0575   0.1798   0.0000   0.0000 ...
   ;
   0.5636   0.4741   0.0000   0.0338   0.0000 ...
   0.1102   0.0636   0.1615   0.0000   0.0000 ...
   ;
   0.6116   0.4642   0.0000   0.0279   0.0000 ...
   0.1105   0.0694   0.1441   0.0000   0.0000 ...
   ;
   0.6591   0.4354   0.0000   0.0230   0.0001 ...
   0.1098   0.0747   0.1277   0.0000   0.0000 ...
   ;
   0.7052   0.3914   0.0000   0.0187   0.0001 ...
   0.1081   0.0792   0.1125   0.0000   0.0000 ...
   ;
   0.7492   0.3372   0.0000   0.0152   0.0002 ...
   0.1054   0.0826   0.0984   0.0000   0.0000 ...
   ;
   0.7903   0.2783   0.0000   0.0122   0.0004 ...
   0.1019   0.0847   0.0855   0.0000   0.0000 ...
   ;
   0.8279   0.2201   0.0000   0.0097   0.0006 ...
   0.0975   0.0856   0.0739   0.0000   0.0000 ...
   ;
   0.8611   0.1668   0.0000   0.0077   0.0011 ...
   0.0925   0.0849   0.0634   0.0000   0.0000 ...
   ;
   0.8893   0.1211   0.0000   0.0061   0.0017 ...
   0.0870   0.0829   0.0540   0.0000   0.0000 ...
   ;
   0.9121   0.0843   0.0000   0.0048   0.0027 ...
   0.0810   0.0795   0.0458   0.0000   0.0000 ...
   ;
   0.9288   0.0562   0.0000   0.0037   0.0041 ...
   0.0748   0.0750   0.0385   0.0000   0.0000 ...
   ;
   0.9391   0.0359   0.0000   0.0029   0.0062 ...
   0.0684   0.0696   0.0322   0.0000   0.0000 ...
   ;
   0.9429   0.0220   0.0000   0.0022   0.0093 ...
   0.0620   0.0634   0.0267   0.0000   0.0000 ...
   ;
   0.9400   0.0129   0.0000   0.0017   0.0136 ...
   0.0556   0.0568   0.0221   0.0000   0.0000 ...
   ;
   0.9306   0.0073   0.0000   0.0013   0.0195 ...
   0.0495   0.0500   0.0181   0.0000   0.0000 ...
   ;
   0.9147   0.0039   0.0000   0.0009   0.0276 ...
   0.0436   0.0433   0.0147   0.0000   0.0000 ...
   ;
   0.8928   0.0020   0.0000   0.0007   0.0383 ...
   0.0381   0.0368   0.0119   0.0000   0.0000 ...
   ;
   0.8653   0.0010   0.0000   0.0005   0.0523 ...
   0.0330   0.0308   0.0096   0.0000   0.0000 ...
   ;
   0.8327   0.0005   0.0000   0.0004   0.0702 ...
   0.0283   0.0253   0.0077   0.0000   0.0000 ...
   ;
   0.7957   0.0003   0.0000   0.0003   0.0925 ...
   0.0240   0.0204   0.0061   0.0000   0.0000 ...
   ;
   0.7550   0.0002   0.0000   0.0002   0.1198 ...
   0.0202   0.0162   0.0048   0.0000   0.0000 ...
   ;
   0.7113   0.0002   0.0000   0.0001   0.1525 ...
   0.0169   0.0127   0.0038   0.0000   0.0000 ...
   ;
   0.6655   0.0002   0.0000   0.0001   0.1907 ...
   0.0139   0.0097   0.0029   0.0000   0.0000 ...
   ;
   0.6182   0.0003   0.0000   0.0001   0.2343 ...
   0.0114   0.0073   0.0023   0.0000   0.0000 ...
   ;
   0.5702   0.0005   0.0000   0.0001   0.2829 ...
   0.0093   0.0054   0.0017   0.0000   0.0000 ...
   ;
   0.5223   0.0006   0.0000   0.0000   0.3357 ...
   0.0074   0.0039   0.0013   0.0000   0.0000 ...
   ;
   0.4750   0.0009   0.0000   0.0000   0.3914 ...
   0.0059   0.0028   0.0010   0.0000   0.0000 ...
   ;
   0.4289   0.0012   0.0000   0.0000   0.4485 ...
   0.0047   0.0020   0.0008   0.0000   0.0000 ...
   ;
   0.3846   0.0017   0.0000   0.0000   0.5050 ...
   0.0037   0.0014   0.0006   0.0000   0.0000 ...
   ;
   0.3425   0.0023   0.0000   0.0000   0.5589 ...
   0.0028   0.0009   0.0004   0.0000   0.0000 ...
   ;
   0.3028   0.0031   0.0000   0.0000   0.6080 ...
   0.0022   0.0006   0.0003   0.0000   0.0000 ...
   ;
   0.2658   0.0042   0.0000   0.0000   0.6501 ...
   0.0017   0.0004   0.0002   0.0000   0.0000 ...
   ;
   0.2317   0.0056   0.0000   0.0000   0.6835 ...
   0.0012   0.0003   0.0002   0.0000   0.0000 ...
   ;
   0.2006   0.0074   0.0000   0.0000   0.7067 ...
   0.0009   0.0002   0.0001   0.0000   0.0000 ...
   ;
   0.1724   0.0096   0.0000   0.0000   0.7188 ...
   0.0007   0.0001   0.0001   0.0000   0.0000 ...
   ;
   0.1471   0.0125   0.0000   0.0000   0.7194 ...
   0.0005   0.0001   0.0001   0.0000   0.0000 ...
   ;
   0.1247   0.0161   0.0000   0.0000   0.7091 ...
   0.0004   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.1049   0.0206   0.0000   0.0000   0.6889 ...
   0.0003   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0876   0.0261   0.0000   0.0000   0.6603 ...
   0.0002   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0727   0.0328   0.0000   0.0000   0.6256 ...
   0.0001   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0599   0.0408   0.0000   0.0000   0.5871 ...
   0.0001   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0490   0.0505   0.0000   0.0000   0.5473 ...
   0.0001   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0398   0.0619   0.0000   0.0000   0.5089 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0321   0.0752   0.0000   0.0000   0.4742 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0257   0.0907   0.0000   0.0000   0.4452 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0204   0.1085   0.0000   0.0000   0.4235 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0161   0.1287   0.0000   0.0000   0.4104 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0127   0.1515   0.0000   0.0000   0.4065 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0099   0.1767   0.0000   0.0000   0.4120 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0076   0.2045   0.0000   0.0000   0.4268 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0058   0.2347   0.0000   0.0000   0.4503 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0045   0.2672   0.0000   0.0000   0.4816 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0034   0.3017   0.0000   0.0000   0.5194 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0025   0.3379   0.0000   0.0000   0.5626 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0019   0.3753   0.0000   0.0000   0.6095 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0014   0.4134   0.0000   0.0000   0.6586 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0010   0.4516   0.0000   0.0000   0.7083 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0008   0.4893   0.0000   0.0000   0.7571 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0005   0.5259   0.0000   0.0000   0.8033 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0004   0.5605   0.0000   0.0000   0.8455 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0003   0.5926   0.0000   0.0000   0.8825 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0002   0.6213   0.0000   0.0000   0.9130 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0001   0.6461   0.0000   0.0000   0.9360 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0001   0.6664   0.0000   0.0000   0.9509 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0001   0.6816   0.0000   0.0000   0.9572 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6916   0.0000   0.0000   0.9547 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6959   0.0000   0.0000   0.9433 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6945   0.0000   0.0000   0.9234 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6874   0.0000   0.0000   0.8955 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6748   0.0000   0.0000   0.8604 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6570   0.0000   0.0000   0.8190 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6345   0.0000   0.0000   0.7723 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.6077   0.0000   0.0000   0.7215 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.5773   0.0000   0.0000   0.6678 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.5439   0.0000   0.0000   0.6123 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.5082   0.0000   0.0000   0.5562 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.4710   0.0000   0.0000   0.5005 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.4329   0.0000   0.0000   0.4462 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.3947   0.0000   0.0000   0.3941 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.3568   0.0000   0.0000   0.3448 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.3200   0.0000   0.0000   0.2989 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.2846   0.0000   0.0000   0.2567 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.2511   0.0000   0.0000   0.2184 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.2197   0.0000   0.0000   0.1841 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.1906   0.0000   0.0000   0.1537 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.1640   0.0000   0.0000   0.1272 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.1400   0.0000   0.0000   0.1042 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.1185   0.0000   0.0000   0.0846 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0995   0.0000   0.0000   0.0681 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0829   0.0000   0.0000   0.0542 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0685   0.0000   0.0000   0.0428 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0561   0.0000   0.0000   0.0335 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0456   0.0000   0.0000   0.0259 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0367   0.0000   0.0000   0.0199 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0293   0.0000   0.0000   0.0151 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0233   0.0000   0.0000   0.0114 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0183   0.0000   0.0000   0.0085 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0143   0.0000   0.0000   0.0063 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
   0.0000   0.0110   0.0000   0.0000   0.0046 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   ;
];
l = [...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.1200 ...
   0.1200   0.2400   0.2400   0.3600   0.3600 ...
   0.4800   0.4800   0.6000   0.6000   0.6000 ...
   0.6000   0.6000   0.6000   0.6000   0.6000 ...
   0.6000   0.4800   0.4800   0.4800   0.4800 ...
   0.4800   0.4800   0.4800   0.4800   0.3600 ...
   0.3600   0.3600   0.3600   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.2400   0.2400   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
   0.0000   0.0000   0.0000   0.0000   0.0000 ...
]';
u = [...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.2400 ...
   0.2400   0.3600   0.3600   0.4800   0.4800 ...
   0.6000   0.6000   0.7200   0.7200   0.7200 ...
   0.7200   0.7200   0.7200   0.7200   0.7200 ...
   0.7200   0.6000   0.6000   0.6000   0.6000 ...
   0.6000   0.6000   0.6000   0.6000   0.4800 ...
   0.4800   0.4800   0.4800   0.3600   0.3600 ...
   0.3600   0.3600   0.3600   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.2400   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.2400   0.2400   0.2400   0.2400   0.2400 ...
   0.3600   0.3600   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.3600   0.3600 ...
   0.3600   0.3600   0.3600   0.3600   0.3600 ...
   0.3600   0.3600   0.3600   0.3600   0.3600 ...
   0.3600   0.3600   0.3600   0.3600   0.3600 ...
   0.3600   0.3600   0.2400   0.2400   0.2400 ...
   0.2400   0.2400   0.2400   0.2400   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
   0.1200   0.1200   0.1200   0.1200   0.1200 ...
]';

figure; plot(S);
% print('-depsc','spect_spectra.eps');
figure; plot(l,'b'); hold on;
plot(u,'r'); hold off;
% print('-depsc','spect_lu.eps');