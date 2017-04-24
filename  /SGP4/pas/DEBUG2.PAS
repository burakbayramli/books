    IF Help = 'Y' THEN
     BEGIN
      WriteLn( SGP4File, ' ------------------ After DSCOM :----------------- ':84 );
      WriteLn( SGP4File, '    Inputs : ' );
      WriteLn( SGP4File, 'EPOCH':7,EPOCH:15:9,'Ep':7,Ep:15:9,'Argpp':7,Argpp:15:9,'Tc':7,Tc:15:9,'Inclp':7,
                          Inclp:15:9,'nodep':7,nodep:15:9 );
      WriteLn( SGP4File, 'Np  ':7,Np:15:9 );
      WriteLn( SGP4File, '    Outputs : ' );
      WriteLn( SGP4File, 'SNODM':7,SNODM:15:9,'CNODM':7,CNODM:15:9,'SINIM':7,SINIM:15:9,'COSIM':7,
                          COSIM:15:9,'SINOMM':7,SINOMM:15:9,'COSOMM':7,COSOMM:15:9 );
      WriteLn( SGP4File, 'DAY':7,DAY:15:9,'E3':7,E3:15:9,'Ee2':7,Ee2:15:9,'EM':7,EM:15:9,'EMSQ':7,EMSQ:15:9,
                          'GAM':7,GAM:15:9 );
      WriteLn( SGP4File, 'Peo':7,Peo:15:9,'Pgho':7,Pgho:15:9,'Pho':7,Pho:15:9,'PInco':7,PInco:15:9,'Plo':7,
                           Plo:15:9 );
      WriteLn( SGP4File, 'RTemSq':7,RTemSq:15:9,'Se2':7,Se2:15:9,'Se3':7,Se3:15:9,'Sgh2':7,Sgh2:15:9,
                          'Sgh3':7,Sgh3:15:9,'Sgh4':7,Sgh4:15:9 );
      WriteLn( SGP4File, 'Sh2':7,Sh2:15:9,'Sh3':7,Sh3:15:9,'Si2':7,Si2:15:9,'Si3':7,Si3:15:9,'Sl2':7,Sl2:15:9,
                          'Sl3':7,Sl3:15:9 );
      WriteLn( SGP4File, 'Sl4':7,Sl4:15:9,'S1':7,S1:15:9,'S2':7,S2:15:9,'S3':7,S3:15:9,'S4':7,S4:15:9,'S5':7,S5:15:9 );
      WriteLn( SGP4File, 'S6':7,S6:15:9,'S7':7,S7:15:9,'SS1':7,SS1:15:9,'SS2':7,SS2:15:9,'SS3':7,SS3:15:9,'SS4':7,
                          SS4:15:9 );
      WriteLn( SGP4File, 'SS5':7,SS5:15:9,'SS6':7,SS6:15:9,'SS7':7,SS7:15:9,'SZ1':7,SZ1:15:9,'SZ2':7,SZ2:15:9,
                          'SZ3':7,SZ3:15:9 );
      WriteLn( SGP4File, 'SZ11':7,SZ11:15:9,'SZ12':7,SZ12:15:9,'SZ13':7,SZ13:15:9,'SZ21':7,SZ21:15:9,
                          'SZ22':7,SZ22:15:9,'SZ23':7,SZ23:15:9 );
      WriteLn( SGP4File, 'SZ31':7,SZ31:15:9,'SZ32':7,SZ32:15:9,'SZ33':7,SZ33:15:9,'Xgh2':7,Xgh2:15:9,
                          'Xgh3':7,Xgh3:15:9,'Xgh4':7,Xgh4:15:9 );
      WriteLn( SGP4File, 'Xh2':7,Xh2:15:9,'Xh3':7,Xh3:15:9,'Xi2':7,Xi2:15:9,'Xi3':7,Xi3:15:9,'Xl2':7,Xl2:15:9,
                          'Xl3':7,Xl3:15:9 );
      WriteLn( SGP4File, 'Xl4':7,Xl4:15:9,'Nm':7,Nm:15:9,'Z1':7,Z1:15:9,'Z2':7,Z2:15:9,'Z3':7,Z3:15:9,'Z11':7,Z11:15:9 );
      WriteLn( SGP4File, 'Z12':7,Z12:15:9,'Z13':7,Z13:15:9,'Z21':7,Z21:15:9,'Z22':7,Z22:15:9,'Z23':7,Z23:15:9,
                          'Z31':7,Z31:15:9 );
      WriteLn( SGP4File, 'Z32':7,Z32:15:9,'Z33':7,Z33:15:9,'Zmol':7,Zmol:15:9,'Zmos':7,Zmos:15:9 );
     END;

