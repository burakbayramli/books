    IF Help='Y' THEN
     BEGIN
       WriteLn( SGP4File, ' ------------------After SGP4   :--------------- ':84 );
       WriteLn( SGP4File,  '    Inputs : ' );
       WriteLn( SGP4File, 'SatRec.isimp':7,SatRec.isimp:13,'SatRec.method':7,SatRec.method:13,
                          'SatRec.aycof':7,SatRec.aycof:15:9,'SatRec.bstar':7,
                          SatRec.bstar:15:9,'SatRec.Con41':7,SatRec.Con41:15:9,'SatRec.cc1':7,
                          SatRec.cc1:15:9 );
       WriteLn( SGP4File,'satrec.cc4':7,satrec.cc4:15:9,'SatRec.cc5':7,SatRec.cc5:15:9,'SatRec.d2':7,
                          SatRec.d2:15:9,'SatRec.d3':7,SatRec.d3:15:9,'SatRec.d4':7,SatRec.d4:15:9,
                          'SatRec.delmo':7,
                          SatRec.delmo:15:9 );
       WriteLn( SGP4File,'SatRec.Ecco':7,SatRec.Ecco:15:9,'SatRec.Eta':7,SatRec.Eta:15:9,
                          'SatRec.argpo':7,SatRec.argpo:15:9,'SatRec.argpdot':7,SatRec.argpdot:15:9,
                          'SatRec.omgcof':7,SatRec.omgcof:15:9,'SatRec.sinmao':7,SatRec.sinmao:15:9 );
       WriteLn( SGP4File,'SatRec.t':7,SatRec.t:15:9,'SatRec.t2cof':7,SatRec.t2cof:15:9,
                          'SatRec.t3cof':7,SatRec.t3cof:15:9,'SatRec.t4cof':7,SatRec.t4cof:15:9,
                          'SatRec.t5cof':7,SatRec.t5cof:15:9,'SatRec.x1mth2':7,SatRec.x1mth2:15:9 );
       WriteLn( SGP4File,'SatRec.x7thm1':7,SatRec.x7thm1:15:9,'SatRec.inclo':7,SatRec.inclo:15:9,
                          'SatRec.mo':7,SatRec.mo:15:9,'SatRec.mdot':7,
                          SatRec.mdot:15:9,'XNO':7,SatRec.No:15:9,'SatRec.nodeo':7,SatRec.nodeo:15:9 );
       WriteLn( SGP4File,'OmgaDot':7,SatRec.nodedot:15:9,'SatRec.xlcof':7,SatRec.xlcof:15:9,
                          'SatRec.xmcof':7,SatRec.xmcof:15:9,'SatRec.nodecf':7,
                          SatRec.nodecf:15:9 );
       WriteLn( SGP4File,  '    Outputs : ' );
       WriteLn( SGP4File,'Error':7,Error:13,'X':7,r[1]:15:9,'Y':7,r[2]:15:9,'Z':7,r[3]:15:9,
                         'XDOT':7,v[1]:15:9,'YDOT':7,v[2]:15:9 );
       WriteLn( SGP4File,'ZDOT':7,v[3]:15:9 );
       WriteLn( SGP4File,  '    Extra Inputs for DS : ' );
       WriteLn( SGP4File, 'IRez':7,satrec.IRez:13,'D2201':7,satrec.d2201:15:9,'D2211':7,satrec.d2211:15:9,
                          'D3210':7,satrec.d3210:15:9,'D3222':7,satrec.d3222:15:9 );
       WriteLn( SGP4File,'D4410':7,satrec.d4410:15:9,'D4422':7,satrec.d4422:15:9,'D5220':7,satrec.d5220:15:9,'D5232':7,
                          satrec.D5232:15:9,'D5421':7,satrec.d5421:15:9,'D5433':7,satrec.d5433:15:9 );
       WriteLn( SGP4File,'Dedt':7,satrec.dedt:15:9,'Del1':7,satrec.del1:15:9,'Del2':7,satrec.del2:15:9,'Del3':7,
                          satrec.del3:15:9,
                          'Didt':7,satrec.didt:15:9,'Dmdt':7,satrec.dmdt:15:9 );
       WriteLn( SGP4File,'Dnodt':7,satrec.dnodt:15:9,'Domdt':7,satrec.domdt:15:9,'SatRec.e3':7,SatRec.e3:15:9,
                          'SatRec.ee2':7,SatRec.ee2:15:9,'Peo':7,
                          satrec.Peo:15:9,'Pgho':7,satrec.Pgho:15:9 );
       WriteLn( SGP4File,'Pho':7,satrec.Pho:15:9,'Pinco':7,satrec.Pinco:15:9,'Plo':7,satrec.Plo:15:9,'Se2':7,
                          satrec.Se2:15:9,'Se3':7,
                          satrec.Se3:15:9,'Sgh2':7,satrec.sgh2:15:9 );
       WriteLn( SGP4File,'Sgh3':7,satrec.sgh3:15:9,'Sgh4':7,satrec.sgh4:15:9,'Sh2':7,satrec.sh2:15:9,'Sh3':7,
                          satrec.sh3:15:9,'Si2':7,
                          satrec.Si2:15:9,'Si3':7,satrec.si3:15:9 );
       WriteLn( SGP4File,'Sl2':7,satrec.sl2:15:9,'Sl3':7,satrec.sl3:15:9,'Sl4':7,satrec.sl4:15:9,'SatRec.gsto':7,
                          SatRec.gsto:15:9,'Xfact':7,
                          satrec.xfact:15:9,'Xgh2':7,satrec.xgh2:15:9 );
       WriteLn( SGP4File,'Xgh3':7,satrec.xgh3:15:9,'Xgh4':7,satrec.xgh4:15:9,'Xh2':7,satrec.xh2:15:9,'Xh3':7,
                          satrec.xh3:15:9,'Xi2':7,
                          satrec.xi2:15:9,'Xi3':7,satrec.xi3:15:9 );
       WriteLn( SGP4File,'Xl2':7,satrec.xl2:15:9,'Xl3':7,satrec.xl3:15:9,'Xl4':7,satrec.xl4:15:9,'Xlamo':7,
                          satrec.xlamo:15:9,'Zmol':7,
                          satrec.Zmol:15:9,'Zmos':7,satrec.Zmos:15:9 );
       WriteLn( SGP4File,'Atime':7,satrec.Atime:15:9,'Xli':7,satrec.xli:15:9,'Xni':7,satrec.xni:15:9 );
     END;

