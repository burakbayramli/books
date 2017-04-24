    IF Help='Y' THEN
     BEGIN
       WriteLn( SGP4File, ' ------------------After SGP4Init :------------- ':84 );
       WriteLn( SGP4File,  '    Inputs  : ' );
       WriteLn( SGP4File,'Satn':7,Satn:13,'Yr':7,' ':13,'SatRec.bstar':7,SatRec.bstar:15:9,
                         'SatRec.Ecco':7,SatRec.Ecco:15:9,'Epoch':7,
                          Epoch:15:9,'SatRec.argpo':7,SatRec.argpo:15:9 );
       WriteLn( SGP4File,'SatRec.inclo':7,SatRec.inclo:15:9,'SatRec.mo':7,SatRec.mo:15:9 );
       WriteLn( SGP4File, ' In and Out variables ' );
       WriteLn( SGP4File, 'SatRec.No':7,SatRec.No:15:9 );
       WriteLn( SGP4File,  '    Outputs  : ' );
       WriteLn( SGP4File,'INIT':7,satrec.INIT:13,'SatRec.isimp':7,SatRec.isimp:13,'SatRec.method':7,
                          SatRec.method:13,
                         'SatRec.aycof':7,SatRec.aycof:15:9 );
       WriteLn( SGP4File,'SatRec.Con41':7,SatRec.Con41:15:9,'SatRec.cc1':7,SatRec.cc1:15:9,'Cc4':7,
                          satrec.Cc4:15:9 );
       WriteLn( SGP4File,'SatRec.cc5':7,SatRec.cc5:15:9,'SatRec.d2':7,SatRec.d2:15:9,'SatRec.d3':7,
                          SatRec.d3:15:9,'SatRec.d4':7,SatRec.d4:15:9,'SatRec.delmo':7,SatRec.delmo:15:9 );
       WriteLn( SGP4File,'SatRec.Eta':7,SatRec.Eta:15:9,'SatRec.argpdot':7,SatRec.argpdot:15:9,
                          'SatRec.omgcof':7,SatRec.omgcof:15:9 );
       WriteLn( SGP4File,'SatRec.sinmao':7,SatRec.sinmao:15:9,'SatRec.t2cof':7,SatRec.t2cof:15:9,
                          'SatRec.t3cof':7,SatRec.t3cof:15:9 );
       WriteLn( SGP4File,'SatRec.t4cof':7,SatRec.t4cof:15:9,'SatRec.t5cof':7,SatRec.t5cof:15:9,
                          'SatRec.gsto':7,SatRec.gsto:15:9,'SatRec.x1mth2':7,
                          SatRec.x1mth2:15:9,'SatRec.x7thm1':7,SatRec.x7thm1:15:9,'SatRec.xlcof':7,
                          SatRec.xlcof:15:9 );
       WriteLn( SGP4File,'SatRec.xmcof':7,SatRec.xmcof:15:9,'SatRec.mdot':7,SatRec.mdot:15:9,
                         'nodeCF':7,satrec.nodeCF:15:9,'OmgaDot':7,
                          SatRec.nodedot:15:9 );
       WriteLn( SGP4File,  '   IN and Outputs from Deep space satellites : ' );
       WriteLn( SGP4File,'T':7,satrec.T:15:9,'SatRec.nodeo':7,SatRec.nodeo:15:9 );
       WriteLn( SGP4File,'Irez':7,satrec.Irez:13,'Atime':7,satrec.Atime:15:9,
                          'D2201':7,satrec.D2201:15:9,'D2211':7,satrec.D2211:15:9,'D3210':7,satrec.D3210:15:9 );
       WriteLn( SGP4File,'D3222':7,satrec.D3222:15:9,'D4410':7,satrec.D4410:15:9,'D4422':7,satrec.D4422:15:9,'D5220':7,
                          satrec.D5220:15:9,'D5232':7,satrec.D5232:15:9,'D5421':7,satrec.D5421:15:9 );
       WriteLn( SGP4File,'D5433':7,satrec.D5433:15:9,'Dedt':7,satrec.Dedt:15:9,'Del1':7,satrec.Del1:15:9,
                          'Del2':7,satrec.Del2:15:9,'Del3':7,satrec.Del3:15:9 );
       WriteLn( SGP4File,'Didt':7,satrec.Didt:15:9,'Dmdt':7,satrec.Dmdt:15:9,'Dnodt':7,satrec.Dnodt:15:9,
                          'Domdt':7,satrec.Domdt:15:9,'SatRec.e3':7,SatRec.e3:15:9 );
       WriteLn( SGP4File,'SatRec.ee2':7,SatRec.ee2:15:9,'Peo':7,satrec.Peo:15:9,'Pgho':7,satrec.Pgho:15:9,
                          'Pho':7,satrec.Pho:15:9,'Pinco':7,satrec.Pinco:15:9,'Plo':7,satrec.Plo:15:9 );
       WriteLn( SGP4File,'Se2':7,satrec.Se2:15:9,'Se3':7,satrec.Se3:15:9,'Sgh2':7,satrec.Sgh2:15:9,'Sgh3':7,
                          satrec.Sgh3:15:9,'Sgh4':7,
                          satrec.Sgh4:15:9,'Sh2':7,satrec.Sh2:15:9 );
       WriteLn( SGP4File,'Sh3':7,satrec.Sh3:15:9,'Si2':7,satrec.Si2:15:9,'Si3':7,satrec.Si3:15:9,'Sl2':7,satrec.Sl2:15:9,
                          'Sl3':7,satrec.Sl3:15:9,
                          'Sl4':7,satrec.Sl4:15:9 );
       WriteLn( SGP4File,'Xfact':7,satrec.Xfact:15:9,'Xgh2':7,satrec.Xgh2:15:9,'Xgh3':7,satrec.Xgh3:15:9,'Xgh4':7,
                          satrec.Xgh4:15:9,'Xh2':7,satrec.Xh2:15:9 );
       WriteLn( SGP4File,'Xh3':7,satrec.Xh3:15:9,'Xi2':7,satrec.Xi2:15:9,'Xi3':7,satrec.Xi3:15:9,'Xl2':7,
                          satrec.Xl2:15:9,'Xl3':7,satrec.Xl3:15:9 );
       WriteLn( SGP4File,'Xl4':7,satrec.Xl4:15:9,'Xli':7,satrec.Xli:15:9,'Xlamo':7,satrec.Xlamo:15:9,'Xni':7,satrec.Xni:15:9,
                          'Zmol':7,satrec.Zmol:15:9,'Zmos':7,satrec.Zmos:15:9 );
     END;

