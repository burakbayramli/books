     if (help == 'y')
       //    if (dbgfile != NULL)
       {
       printf( "%84s\n",
                        " ------------------after dspace :--------------- ");
       printf( "    inputs : \n");
       printf(
              "%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
              "irez", irez, "d2201", d2201, "d2211", d2211,
              "d3210", d3210, "d3222", d3222, "d4410", d4410);
       printf(
              "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
              "d4422", d4422, "d5220", d5220, "d5232", d5232,
              "d5421", d5421, "d5433", d5433, "dedt", dedt);
       printf(
              "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
              "del1", del1, "del2", del2, "del3", del3,
              "didt", didt, "dmdt", dmdt, "dnodt", dnodt);
       printf(
              "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
              "domdt", domdt, "argpo", argpo, "argpdot", argpdot,
              "t", t, "tc", tc, "gsto", gsto);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f\n",
                        "xfact", xfact, "xlamo", xlamo, "no", no);
       printf( "    in / out : \n");
       printf(
              "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
              "atime", atime, "em", em, "argpm", argpm,
              "inclm", inclm, "xli", xli, "mm", mm);
       printf( "%7s%15.9f%7s%15.9f\n", "xni", xni, "nodem", nodem);
       printf( "    outputs : \n");
       printf( "%7s%15.9f%7s%15.9f\n", "dndt", dndt, "nm", nm);
    }

