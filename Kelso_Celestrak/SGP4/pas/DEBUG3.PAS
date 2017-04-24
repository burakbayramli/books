    IF Help='Y' THEN
     BEGIN
       WriteLn( SGP4File, ' ------------------After DSINIT : --------------- ':84 );
       WriteLn( SGP4File,  '    Inputs : ' );
       WriteLn( SGP4File,'Cosim':7,Cosim:15:9,'Emsq':7,Emsq:15:9,'Argpo':7,Argpo:15:9,'S1':7,S1:15:9,
                          'S2':7,S2:15:9,'S3':7,S3:15:9 );
       WriteLn( SGP4File,'S4':7,S4:15:9,'S5':7,S5:15:9,'Sinim':7,Sinim:15:9,'Ss1':7,Ss1:15:9,'Ss2':7,Ss2:15:9,
                          'Ss3':7,Ss3:15:9 );
       WriteLn( SGP4File,'Ss4':7,Ss4:15:9,'Ss5':7,Ss5:15:9,'Sz1':7,Sz1:15:9,'Sz3':7,Sz3:15:9,'Sz11':7,Sz11:15:9,
                          'Sz13':7,Sz13:15:9 );
       WriteLn( SGP4File,'Sz21':7,Sz21:15:9,'Sz23':7,Sz23:15:9,'Sz31':7,Sz31:15:9,'Sz33':7,Sz33:15:9,'T':7,
                          T:15:9,'Tc':7,Tc:15:9 );
       WriteLn( SGP4File,'GSTo':7,GSTo:15:9,'Mo':7,Mo:15:9,'MDot':7,MDot:15:9,'No':7,No:15:9,
                          'nodeo':7,nodeo:15:9,'OmgaDot':7,nodeDot:15:9 );
       WriteLn( SGP4File,'XPIDOT':7,XPIDOT:15:9,'Z1':7,Z1:15:9,'Z3':7,Z3:15:9,'Z11':7,Z11:15:9,'Z13':7,Z13:15:9,
                          'Z21':7,Z21:15:9 );
       WriteLn( SGP4File,'Z23':7,Z23:15:9,'Z31':7,Z31:15:9,'Z33':7,Z33:15:9 );
       WriteLn( SGP4File,  '    In / Out : ' );
       WriteLn( SGP4File,'EM':7,EM:15:9,'Argpm':7,Argpm:15:9,'Inclm':7,Inclm:15:9,'Mm':7,Mm:15:9,
                          'Nm':7,Nm:15:9,'nodem':7,nodem:15:9 );
       WriteLn( SGP4File,  '    Outputs : ' );
       WriteLn( SGP4File,'IREZ':7,IREZ:13,'Atime':7,Atime:15:9,'D2201':7,D2201:15:9,'D2211':7,
                          D2211:15:9,'D3210':7,D3210:15:9,'D3222':7,D3222:15:9 );
       WriteLn( SGP4File,'D4410':7,D4410:15:9,'D4422':7,D4422:15:9,'D5220':7,D5220:15:9,'D5232':7,
                          D5232:15:9,'D5421':7,D5421:15:9,'D5433':7,D5433:15:9 );
       WriteLn( SGP4File,'Dedt':7,Dedt:15:9,'Didt':7,Didt:15:9,'DMDT':7,DMDT:15:9,'DNDT':7,DNDT:15:9,
                          'DNODT':7,DNODT:15:9,'DOMDT':7,DOMDT:15:9 );
       WriteLn( SGP4File,'Del1':7,Del1:15:9,'Del2':7,Del2:15:9,'Del3':7,Del3:15:9,'Xfact':7,Xfact:15:9,
                          'Xlamo':7,Xlamo:15:9,'Xli':7,Xli:15:9 );
       WriteLn( SGP4File,'Xni':7,Xni:15:9 );
     END;

