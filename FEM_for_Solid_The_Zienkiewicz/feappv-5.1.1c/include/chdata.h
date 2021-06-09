
      character       xxx*256,yyy*256,zzz*80,lzz*80  ,ljs*15  ,lzs*80
      common /chdata/ xxx    ,yyy    ,zzz   ,lzz(999),ljs(999),lzs(999)

      character       matfile*70
      common /chdata/ matfile(100)

      integer         prtyp
      common /fe2var/ prtyp(2,100)
