   IF Help='Y' THEN
     BEGIN
      WriteLn( SGP4File, ' ------------------After DPPER : ---------------- ':84 );
      WriteLn( SGP4File, '    Inputs : ' );
      WriteLn( SGP4File, 'e3':7,e3:15:9,'ee2':7,ee2:15:9,'peo':7,peo:15:9,'pgho':7,pgho:15:9,'pho':7,pho:15:9,
                          'pinco':7,pinco:15:9 );
      WriteLn( SGP4File, 'plo':7,plo:15:9,'se2':7,se2:15:9,'se3':7,se3:15:9,'sgh2':7,sgh2:15:9,'sgh3':7,
                          sgh3:15:9,'sgh4':7,sgh4:15:9 );
      WriteLn( SGP4File, 'sh2':7,sh2:15:9,'sh3':7,sh3:15:9,'si2':7,si2:15:9,'si3':7,si3:15:9,'sl2':7,sl2:15:9,
                          'sl3':7,sl3:15:9 );
      WriteLn( SGP4File, 'sl4':7,sl4:15:9,'T':7,T:15:9,'xgh2':7,xgh2:15:9,'xgh3':7,xgh3:15:9,'xgh4':7,xgh4:15:9,
                          'xh2':7,xh2:15:9 );
      WriteLn( SGP4File, 'xh3':7,xh3:15:9,'xi2':7,xi2:15:9,'xi3':7,xi3:15:9,'xl2':7,xl2:15:9,'xl3':7,xl3:15:9,
                          'xl4':7,xl4:15:9 );
      WriteLn( SGP4File, 'zmol':7,zmol:15:9,'zmos':7,zmos:15:9,'Init':7,Init:13 );
      WriteLn( SGP4File,  '    In/Out : ' );
      WriteLn( SGP4File, 'EP':7,Ep:15:9,'Inclp':7,Inclp:15:9,'nodep':7,nodep:15:9,'Argpp':7,Argpp:15:9,
                          'Mp':7,Mp:15:9 );
     END;

