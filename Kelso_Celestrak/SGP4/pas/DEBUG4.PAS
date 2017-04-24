    IF Help='Y' THEN
     BEGIN
       WriteLn( SGP4File, ' ------------------After DSPACE :--------------- ':84 );
       WriteLn( SGP4File,  '    Inputs : ' );
       WriteLn( SGP4File, 'IRez':7,IRez:13,'D2201':7,D2201:15:9,'D2211':7,D2211:15:9,'D3210':7,
                           D3210:15:9,'D3222':7,D3222:15:9,'D4410':7,D4410:15:9 );
       WriteLn( SGP4File, 'D4422':7,D4422:15:9,'D5220':7,D5220:15:9,'D5232':7,D5232:15:9,'D5421':7,
                           D5421:15:9,'D5433':7,D5433:15:9,'Dedt':7,Dedt:15:9 );
       WriteLn( SGP4File, 'Del1':7,Del1:15:9,'Del2':7,Del2:15:9,'Del3':7,Del3:15:9,'Didt':7,Didt:15:9,
                           'Dmdt':7,Dmdt:15:9,'Dnodt':7,Dnodt:15:9 );
       WriteLn( SGP4File, 'Domdt':7,Domdt:15:9,'Argpo':7,Argpo:15:9,'ArgpDot':7,ArgpDot:15:9,'T':7,T:15:9,
                           'TC':7,TC:15:9,'GSTo':7,GSTo:15:9 );
       WriteLn( SGP4File, 'Xfact':7,Xfact:15:9,'Xlamo':7,Xlamo:15:9,'No':7,No:15:9 );
       WriteLn( SGP4File,  '    In / Out : ' );
       WriteLn( SGP4File, 'Atime':7,Atime:15:9,'EM':7,EM:15:9,'Argpm':7,Argpm:15:9,'Inclm':7,
                           Inclm:15:9,'Xli':7,XLi:15:9,'Mm':7,Mm:15:9 );
       WriteLn( SGP4File, 'Xni':7,Xni:15:9,'nodem':7,nodem:15:9 );
       WriteLn( SGP4File,  '    Outputs : ' );
       WriteLn( SGP4File, 'Dndt':7,Dndt:15:9,'Nm':7,Nm:15:9 );
     END;


