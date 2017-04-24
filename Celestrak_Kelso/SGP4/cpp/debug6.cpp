      if (help == 'y')
       //    if (dbgfile != NULL)
       {
       printf( "%7s\n",
                        " ------------------after sgp4init :-------------");
       printf( "    inputs  : \n");
       printf(
          "%7s%15d%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
          "satn", satn, "yr", satn, "bstar", satrec.bstar, "ecco", satrec.ecco,
          "epoch", epoch, "argpo", satrec.argpo);
       printf( "%7s%15.9f%7s%15.9f\n", "inclo", satrec.inclo, "mo", satrec.mo);
       printf( " in and out variables \n");
       printf( "%7s%15.9f\n", "no", satrec.no);
       printf( "    outputs  :\n");
       printf( "%7s%15c%7s%15d%7s%15c%7s%15.9f\n",
           "init", satrec.init, "isimp", satrec.isimp,
           "method", satrec.method, "aycof", satrec.aycof);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "con41", satrec.con41, "cc1", satrec.cc1, "cc4", satrec.cc4);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "cc5", satrec.cc5, "d2",    satrec.d2, "d3", satrec.d3,
           "d4",  satrec.d4,  "delmo", satrec.delmo);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "eta", satrec.eta, "argpdot", satrec.argpdot,
           "omgcof", satrec.omgcof);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "sinmao", satrec.sinmao, "t2cof", satrec.t2cof,
           "t3cof", satrec.t3cof);
       printf(
           "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "t4cof",  satrec.t4cof,  "t5cof",  satrec.t5cof,
           "gsto",   satrec.gsto,   "x1mth2", satrec.x1mth2,
           "x7thm1", satrec.x7thm1, "xlcof",  satrec.xlcof);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "xmcof",   satrec.xmcof,   "mdot",     satrec.mdot,
           "nodecf", satrec.nodecf, "nodedt", satrec.nodedot);
       printf( "   in and outputs from deep space satellites :\n");
       printf( "%7s%15.9f%7s%15.9f\n",
           "t", satrec.t, "nodeo", satrec.nodeo);
       printf( "%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "irez",  satrec.irez,  "atime", satrec.atime,
           "d2201", satrec.d2201, "d2211", satrec.d2211,
           "d3210", satrec.d3210);
       printf(
           "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "d3222", satrec.d3222, "d4410", satrec.d4410,
           "d4422", satrec.d4422, "d5220", satrec.d5220,
           "d5232", satrec.d5232, "d5421", satrec.d5421);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "d5433", satrec.d5433, "dedt", satrec.dedt,
           "del1",  satrec.del1,  "del2", satrec.del2,
           "del3",  satrec.del3);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "didt",  satrec.didt,  "dmdt",  satrec.dmdt,
           "dnodt", satrec.dnodt, "domdt", satrec.domdt,
           "e3",    satrec.e3);
       printf(
           "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "ee2", satrec.ee2, "peo",   satrec.peo,   "pgho", satrec.pgho,
           "pho", satrec.pho, "pinco", satrec.pinco, "plo",  satrec.plo);
       printf(
           "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "se2",  satrec.se2,  "se3",  satrec.se3,  "sgh2", satrec.sgh2,
           "sgh3", satrec.sgh3, "sgh4", satrec.sgh4, "sh2",  satrec.sh2);
       printf(
           "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "sh3", satrec.sh3, "si2", satrec.si2, "si3", satrec.si3,
           "sl2", satrec.sl2, "sl3", satrec.sl3, "sl4", satrec.sl4);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "xfact", satrec.xfact, "xgh2", satrec.xgh2, "xgh3", satrec.xgh3,
           "xgh4",  satrec.xgh4,  "xh2",  satrec.xh2);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "xh3",  satrec.xh3, "xi2", satrec.xi2, "xi3", satrec.xi3,
           "xl2",  satrec.xl2, "xl3", satrec.xl3);
       printf(
           "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
           "xl4", satrec.xl4, "xli",  satrec.xli,  "xlamo", satrec.xlamo,
           "xni", satrec.xni, "zmol", satrec.zmol, "zmos",  satrec.zmos);
    }

