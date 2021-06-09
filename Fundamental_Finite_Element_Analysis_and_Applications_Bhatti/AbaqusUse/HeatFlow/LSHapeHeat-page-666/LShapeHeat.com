import os, sys, driverUtils
from constants import *
from driverExceptions import *
from driverStandard import StandardAnalysis
if os.name != "nt":
   import signal 
   signal.signal(signal.SIGTERM, signal.SIG_IGN) 
   signal.signal(signal.SIGINT,  signal.SIG_IGN) 
   signal.signal(signal.SIGTSTP, signal.SIG_IGN) 
else:
   import msvcrt
   msvcrt.setmode(sys.stdout.fileno(), os.O_TEXT)
args = {
    'abaquslm_license_file':'1711@server09.icaen.uiowa.edu',
    'admin':[],
    'analysisType':STANDARD,
    'aqua':OFF,
    'ask_delete':OFF,
    'background':None,
    'compile_cpp':'aCC  +DA2.0W +DS2.0a -c -DHP  -DHP11  -DFOR_TRAIL -DCPLUS_IMPLEMENTED -DSWAPPED -ext -DBIT64   +O2 +Olibcalls -I%I',
    'compile_fortran':'f90 +DA2.0w +DS2.0a -c +O2 +extend_source +Olibcalls -I%I',
    'contact':OFF,
    'cpus':1,
    'dataLineCheck':ON,
    'display':'d-coe083:0.0',
    'dsa':OFF,
    'explicit_precision':SINGLE_PRECISION,
    'filPrt':[],
    'fils':[],
    'importer':OFF,
    'includes':[],
    'indir':'/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal',
    'input':'LShapeHeat',
    'job':'LShapeHeat',
    'link_exe':'aCC +DA2.0w +DS2.0a +O2 -Wl,-ashared_archive,+k -Wl,+n,+FPD,+vnocompatwarnings -Wl,+pd,L,+pi,D -o %J %F %M %L %B %O -lpthread -lcps -lcl',
    'link_sl':'/usr/ccs/lbin/ld64 -b -ashared_archive +k +n +FPD +vnocompatwarnings +pd L +pi D +s -c%E +h%U -o %U %F %A %B %L -lpthread -lcps -lcl',
    'lmsvrdownlimit':0,
    'message':ON,
    'onCaeGraphicsStartup':driverUtils.decodeFunction('begin 666 -\012M8P  $0 ;  , <X$&  !_ 0!_! !T  !I 0!I @!]  !_!0!T  !I 0!I P!]\012M 0!_!@!T  !I 0!I! !] @!_!P!T  !I 0!I!0!] P!_$ !T!@!]! !_$0!T\012M" !]!0!_$@!T" !]!@!_$P!T"P!T# !T#0!T#@!F! !]!P!_% !T$ !]" !_\012M%0!T$@!]"0!_%@!D 0!]"@!_%P!D @!]"P!_& !T" !]# !_&0!T  !I%P!I\012M& !]#0!_&@!T  !I%P!I&0!]#@!_&P!T  !I%P!I&@!]#P!_\' !T  !I%P!I\012M&P!]$ !_(@!\\ P!T\' !J @!P%P !? , 9 , :@( < H  7P# &0$ &H" &] \012M  %_)P!D!0!]"@!_* !D!@!]"P!_*@!\\ 0!D!P!J @!O"@ !?   9 @ :@( \012M;PT  7\\K \'02 \'T, &X!  %N3  !?RT ? , 9 D :@( ;PT  7\\N &0* \'T*\012M &XO  %_, !\\ P!D"P!J @!O#@ !? $ 9 P (&0- &H" &\\-  %_,0!D#@!]\012M"@!N 0 !?S@ ?   9 \\ :@( ;\\T  7\\Y \'P! &00 &H" &\\S  %_.@!\\ P!T\012M\' !J @!O#0 !?SP =!( ?0X ;A,  7\\_ &01 \'T* \']  &02 \'T+ &Z\'  %_\012M0@!\\ 0!D$P!J @!O,0 !?T0 9!0 ?0H ?T4 9!4 ?0L ?T< =!( ?0P ?TD \012M= L = T = X 9@, ?0< ;D8  7]+ \'P! &06 "!D%P!J @!O,0 !?TT 9!@ \012M?0H ?TX 9!D ?0L ?U  =!( ?0P ?U( = L = T = X 9@, ?0< ;@$  6YH\012M P%_5 !\\  !D"0!J @!O2P !?U4 ? $ 9 P (&0: &H" &\\6  %_5@!D&P!]\012M"@!_5P!D\' !]"P!N\'@ !?UH ? $ 9!T :@( ;PT  7]; &0> \'T* &X!  %N\012M#0,!?UT ?   9!\\ :@( ;\\(  7]> \'P! &0@ &H" &\\\\  %_7P!D!0!]"@!_\012M8 !D(0!]"P!_8@!\\ @!D(@!D(P!T\' !F P!J @!O#0 !?V8 =!( ?08 ;@$ \012M 6YS  %_: !\\ 0!D) !J @!O%@ !?VL 9 4 ?0H ?VP 9 ( ?0L ;DT  7]N\012M \'P! &0E &H" &\\6  %_;P!D(0!]"@!_< !D)@!]"P!N)P !?W( ? $ 9"< \012M:@( ;Q8  7]U &0% \'T* \']V &0< \'T+ &X!  %N.P(!?W@ ?   9"@ :@( \012M;U   7]Y \'P! &0I &H" &\\6  %_>@!D!0!]"@!_>P!D*@!]"P!N)P !?WT \012M? $ 9"L :@( ;Q8  7]_ &0L \'T* \'^  &0< \'T+ &X!  %NVP$!?X( ?   \012M9"T :@( ;VT  7^$ \'P! &0N &H" &\\6  %_A0!D+P!]"@!_A@!D, !]"P!N\012M1  !?X@ ? $ 9#$ :@( ;Q8  7^* &0% \'T* \'^+ &0" \'T+ &X>  %_C0!\\\012M 0!D,@!J @!O#0 !?Y( = T ?0< ;@$  6Y> 0%_E !\\  !D,P!J @!O-@ !\012M?Y< ? $ 9 P (&0T &H" \' .  %\\ 0!D-0 @9#8 :@( ;PT  7^: \'02 \'T,\012M &X!  %N& $!?YP ?   9#< :@( ;PT  7^B \'02 \'T, &[[  %_K !\\  !D\012M. !J @!O*@ !?ZT ? $ 9#D :@( ;Q8  7^O &0Z \'T* \'^P &0" \'T+ &X!\012M  %NP0 !?[( ?   9#L :@( ;S,  7^S \'P! &0\\ &H" &\\?  %_M0!T$@!]\012M# !_M@!D/0!]"@!_MP!D!0!]"P!N 0 !;GX  7^Y \'P  &0^ &H" &\\S  %_\012MN@!\\ 0!D/P!J @!O\'P !?[P =!( ?0P ?[T 9"$ ?0H ?[X 9"$ ?0L ;@$ \012M 6X[  %_P !\\  !D0 !J @!O*@ !?\\( ? $ 9$$ :@( ;Q8  7_# &02 \'T*\012M \'_$ &1" \'T+ &X!  %N 0 !?\\< ? , =!P :@( ;PH  7P. \'02 &H" &\\-\012M  %_R !T$@!]# !N 0 !?\\H =   :1< :1T 9$, ? 0 ?\\P 9$0 ? 4 ?\\T \012M9$4 ? 8 ?\\X 9$8 ? < ?\\\\ 9$< ? @ ?]  9$@ ? D ?]$ 9$D ? H ?]( \012M9$H ? L ?], 9$L ? P ?]0 9$P ? T ?]4 9$T ? X ?]8 9$X ? \\ ?]< \012M9$\\ ?!  @P - 60  %,H4    $YF!# N-3%F S$N-7,?    2\'5M;6EN9V)I\012M<F0@0V]M;75N:6-A=&EO;G,@3\'1D+G,0    2\'5M;6EN9V)I<F0@3\'1D+F8#\012M,2XP9@4Q+C<Q,G,+    1T1)($=E;F5R:6-S%0   $UI8W)O<V]F="!#;W)P\012M;W)A=&EO;G,#    4T=)9@0P+C T<P,   !)0DUI!@   \',&    1UA4.# P\012M9@0P+C@W<P8    S1&QA8G-S"0   %!%4DU%1$E!,V8$,"XQ-68$,"XW-7,+\012M    1TQ)3E0@4C,@4%1F!# N,#5F S N-VD9    <QD   !\'3$E.5"!2,R!0\012M5" K($=,24Y4($=A;6UA9@,P+C1F S(N,\',&    24U004-49@,S+C5F!# N\012M.#5S"0   %)%4R]3+S$O,F8#,"XX<Q(   !.5DE$24$@0V]R<&]R871I;VYS\012M$P   %%U861R;S(@35A2+U!#22]34T5F S N-6D!    :0(   !S$P   $=E\012M1F]R8V4@,C4V+U!#22]34T5S#@   %))5D$@5$Y4("A00TDI9@0Q+CDW<Q$ \012M  !2259!(%1.5#(O4$-)+U-317,$    14Q307,7    14Q302!%4D%:3U(@\012M6""R+T%\'4"]34T5F!# N-35S#P   $5,4T$@4WEN97)G>2!)268$,"XR-\',7\012M    2&5W;&5T="U086-K87)D($-O;7!A;GES!P   &AP=FES9GAF!# N.#AF\012M S N.7,+    ;&EB9&1V:7-X9VQS%0   %9I<G1U86P@365M;W)Y($1R:79E\012M<G,6    26YT97)G<F%P:"!#;W)P;W)A=&EO;G,&    =V-G9\')V:1P   !S\012M\'    $EN=&5R9W)A<&@@0V]R<&]R871I;VX@,#0N,#%S%@   %-U;B!-:6-R\012M;W-Y<W1E;7,L($EN8RYS%@   %9!($QI;G5X(%-Y<W1E;7,L($EN8RYS$0  \012M $UE<V$@1TQ8($EN9&ER96-T9@0P+C$X<Q0   !-871R;W@@1W)A<&AI8W,@\012M26YC+G,+    36%T<F]X($<T,#!F!# N,#%S$@   $1I86UO;F0@375L=&EM\012M961I87,(    1FER92!\'3#%S%0   $%422!496-H;F]L;V=I97,@26YC+G,4\012M    4D%\'12 Q,C@@4\')O(\'@X-B]34T5F S$N-W,.    9W)A<&AI8W-$<FEV\012M97)S#P   &1O=6)L94)U9F9E<FEN9W,,    9&ES<&QA>4QI<W1S<Q,   !H\012M:6=H;&EG:\'1-971H;V1(:6YT<P@   !D<F%G36]D97,2    875T;T9I=$%F\012M=&5R4F]T871E<Q4   !P;VQY9V]N3V9F<V5T0V]N<W1A;G1S$@   \'!O;\'EG\012M;VY/9F9S9713;&]P97,,    =F5R=&5X07)R87ES<P\\   !D:7)E8W1296YD\012M97)I;F=S%    &AA<F1W87)E06-C96QE<F%T:6]N<P\\   !H87)D=V%R94]V\012M97)L87ES#P   &)A8VMG<F]U;F1#;VQO<B@>    <P<   !S97-S:6]N<PP \012M  !G<F%P:&EC<TEN9F]S"    &=L5F5N9&]R<PH   !G;%)E;F1E<F5R<PD \012M  !G;%9E<G-I;VYS#P   &=L>%-E<G9E<E9E;F1O<G,\'    3U!%3E]\'3\',.\012M    9W)A<&AI8W-$<FEV97)S @   $].<P\\   !D;W5B;&5"=69F97)I;F=S\012M#    &1I<W!L87E,:7-T<W,0    2$%21%=!4D5?3U9%4DQ!67,#    6$]2\012M<Q    !33T945T%215]/5D523$%9<P4   !"3$5.1\',3    :&EG:&QI9VAT\012M365T:&]D2&EN=\',$    1D%35\',(    9\')A9TUO9&5S P   $]&1G,2    \012M875T;T9I=$%F=&5R4F]T871E<Q4   !P;VQY9V]N3V9F<V5T0V]N<W1A;G1S\012M$@   \'!O;\'EG;VY/9F9S9713;&]P97,,    =F5R=&5X07)R87ES<Q8   !D\012M969A=6QT1W)A<&AI8W-/<\'1I;VYS<P\\   !D:7)E8W1296YD97)I;F=S%   \012M &AA<F1W87)E06-C96QE<F%T:6]N<P\\   !H87)D=V%R94]V97)L87ES#P  \012M &)A8VMG<F]U;F1#;VQO<G,$    3F]N97,)    <V5T5F%L=65S*!$   !S\012M"    &=L5F5N9&]R<PH   !G;%)E;F1E<F5R<PD   !G;%9E<G-I;VYS#P  \012M &=L>%-E<G9E<E9E;F1O<G,.    9W)A<&AI8W-$<FEV97)S#P   &1O=6)L\012M94)U9F9E<FEN9W,,    9&ES<&QA>4QI<W1S<Q,   !H:6=H;&EG:\'1-971H\012M;V1(:6YT<P@   !D<F%G36]D97,2    875T;T9I=$%F=&5R4F]T871E<Q4 \012M  !P;VQY9V]N3V9F<V5T0V]N<W1A;G1S$@   \'!O;\'EG;VY/9F9S9713;&]P\012M97,,    =F5R=&5X07)R87ES<P\\   !D:7)E8W1296YD97)I;F=S%    &AA\012M<F1W87)E06-C96QE<F%T:6]N<P\\   !H87)D=V%R94]V97)L87ES#P   &)A\012M8VMG<F]U;F1#;VQO<G-,    +W1M<%]M;G0O;F9S+W-E<G9E<C T+VQO8V%L\012M+W9O;# P+V%P<\',O86)A<75S-B\\V+C,M,2]S:71E+V=R87!H:6-S0V]N9FEG\012M+F5N=G,4    ;VY#865\'<F%P:&EC<U-T87)T=7 ! \'/J     P,/ 0\\!#P$/\012M"0D!"0$) 14!"0$) 0D!"0$) 0\\!#P$/ 0\\&*@4) 0D"\'0$1 A !#0(A 0T\'\012M$ $0 1 "#0,) 0T"$ () 0D""0(6 A0""0$) @D"&@(0 10!"0$- Q !$0(0\012M 1 !"0$) AD$$0(0 PD!#0(0 0D!#0(0 PD!$0(0 1 !"0$- A ""0$1 A "\012M$ $) 0T"$ () 0T"$ 41 A #)0,1 A &#0H0 1 ""0$1 A !$ () 0D!$0(0\012M 1 ""0$) 1$"$ (0 0D!$0,= 0T"$@() 0D!"0$) 0D!"0$) 0D!"0$) 0D!\012 \012end\012'),
    'outdir':'/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal',
    'outputKeywords':ON,
    'paid_up':OFF,
    'parameterized':OFF,
    'partsAndAssemblies':ON,
    'parval':OFF,
    'pre_memory':33554432,
    'restart':OFF,
    'restartWrite':ON,
    'rezone':OFF,
    'runCalculator':OFF,
    'standard_memory':33554432,
    'submodel':OFF,
    'symmetricModelGeneration':OFF,
    'tmpdir':'/var/tmp',
}
env = { 
    'ABADOC':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/Documentation',
    'ABAQUSLM_LICENSE_FILE':'1711@server09.icaen.uiowa.edu',
    'ABAQUS_SEQ':'2002_09_03-10.38.43 36270',
    'ABA_COMMAND':'/usr/local/apps/abaqus/Commands/abq631',
    'ABA_HOME':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1',
    'ABA_LIBRARY_PATHNAME':'SHLIB_PATH',
    'ABA_PATH':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae',
    'ABQLMHANGLIMIT':'0',
    'ABQLMQUEUE':'30',
    'CCOPTS':'+e',
    'COLUMNS':'81',
    'DISPLAY':'d-coe083:0.0',
    'DTAPPSEARCHPATH':'/user/eng/mabhatti/.dt/appmanager:/etc/dt/appconfig/appmanager/%L:/etc/dt/appconfig/appmanager/C:/usr/dt/appconfig/appmanager/%L:/usr/dt/appconfig/appmanager/C',
    'DTDATABASESEARCHPATH':'/user/eng/mabhatti/.dt/types,/etc/dt/appconfig/types/%L,/etc/dt/appconfig/types/C,/usr/dt/appconfig/types/%L,/usr/dt/appconfig/types/C',
    'DTHELPSEARCHPATH':'/user/eng/mabhatti/.dt/help/mabhatti-d-coe083-0/%H:/user/eng/mabhatti/.dt/help/mabhatti-d-coe083-0/%H.sdl:/user/eng/mabhatti/.dt/help/mabhatti-d-coe083-0/%H.hv:/user/eng/mabhatti/.dt/help/%H:/user/eng/mabhatti/.dt/help/%H.sdl:/user/eng/mabhatti/.dt/help/%H.hv:/etc/dt/appconfig/help/%L/%H:/etc/dt/appconfig/help/%L/%H.sdl:/etc/dt/appconfig/help/%L/%H.hv:/etc/dt/appconfig/help/C/%H:/etc/dt/appconfig/help/C/%H.sdl:/etc/dt/appconfig/help/C/%H.hv:/usr/dt/appconfig/help/%L/%H:/usr/dt/appconfig/help/%L/%H.sdl:/usr/dt/appconfig/help/%L/%H.hv:/usr/dt/appconfig/help/C/%H:/usr/dt/appconfig/help/C/%H.sdl:/usr/dt/appconfig/help/C/%H.hv:/etc/vhelp/%T/%L/%H:/etc/vhelp/%T/%H:/etc/vhelp/%T/%L/%H.hv:/etc/vhelp/%T/%H.hv:/etc/vhelp/%T/C/%H:/etc/vhelp/%T/C/%H.hv:/usr/vhelp/%T/%L/%H:/usr/vhelp/%T/%H:/usr/vhelp/%T/%L/%H.hv:/usr/vhelp/%T/%H.hv:/usr/vhelp/%T/C/%H:/usr/vhelp/%T/C/%H.hv',
    'DTINFOLIBDEFAULT':'cde',
    'DTINFOLIBSEARCHPATH':'/usr/dt/infolib/%L/%I.dti:/usr/dt/infolib/C/%I.dti',
    'DTSCREENSAVERLIST':'StartDtscreenSwarm StartDtscreenQix     StartDtscreenFlame StartDtscreenHop StartDtscreenImage StartDtscreenLife     StartDtscreenRotor StartDtscreenPyro StartDtscreenWorm StartDtscreenBlank',
    'DTUSERSESSION':'mabhatti-d-coe083-0',
    'EBTRC':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/Documentation/ebtrc.txt',
    'EDITOR':'/usr/local/bin/dxvi',
    'ENV':'${START[ (_$- = 1) + ( _ = 0 ) - ( _$- != _${-%%*i*} ) ]}',
    'HKS_HLPSVR_EXE':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/exec/hksHelpServer',
    'HKS_LANG':'C',
    'HOME':'/user/eng/mabhatti',
    'KRB5CCNAME':'FILE:/tmp/krb5cc_2159_12603',
    'LANG':'C',
    'LINES':'26',
    'LM_LICENSE_FILE':'1711@server09.icaen.uiowa.edu:1740@server09.icaen.uiowa.edu:7788@server09.icaen.uiowa.edu',
    'LOGNAME':'mabhatti',
    'MAIL':'/var/mail/mabhatti',
    'MANPATH':'/opt/mpi/share/man:/usr/share/man/%L:/usr/share/man:/usr/contrib/man/%L:/usr/contrib/man:/usr/local/man/%L:/usr/local/man:/opt/upgrade/share/man/%L:/opt/upgrade/share/man:/usr/dt/share/man:/opt/samba/man:/opt/pd/share/man/%L:/opt/pd/share/man:/opt/pd/share/man/%L:/opt/pd/share/man:/opt/pd/share/man/%L:/opt/pd/share/man:/opt/resmon/share/man:/opt/graphics/phigs/share/man:/opt/hparray/share/man/%L:/opt/hparray/share/man:/opt/graphics/common/man:/opt/perl/man:/opt/scr/share/man:/opt/OpenSource/man:/opt/videoout/share/man:/opt/ignite/share/man/%L:/opt/ignite/share/man:/opt/gnome/man:/opt/ldapux/share/man:/opt/ipf/man:/opt/ldapux/ypldapd/man:/opt/aCC/share/man/%L:/opt/aCC/share/man:/opt/audio/share/man:/opt/ansic/share/man/%L:/opt/ansic/share/man:/opt/langtools/share/man/%L:/opt/langtools/share/man:/opt/fortran90/share/man/%L:/opt/fortran90/share/man:/opt/fortran90/contrib/man:/opt/perf/man/%L:/opt/perf/man:/opt/image/share/man:/opt/pascal/share/man:/opt/softbench/share/man/%L:/opt/softbench/share/man:/opt/STK/share/man:/opt/wt/share/man:/opt/imake/man',
    'NODENAME':'l-ecn001',
    'NODETYPE':'9000/785',
    'OMP_NUM_THREADS':'1',
    'PAGER':'more',
    'PAID_UP':'no',
    'PATH':'/user/eng/mabhatti/bin:/usr/local/bin:/opt/softbench/bin:/usr/bin:/usr/ccs/bin:/usr/contrib/bin:/opt/mpi/bin:/opt/hparray/bin:/opt/nettladm/bin:/opt/upgrade/bin:/opt/fcms/bin:/usr/local/bin:/usr/bin/X11:/usr/contrib/bin/X11:/opt/pd/bin:/opt/resmon/bin:/opt/graphics/phigs/bin:/opt/graphics/common/bin:/opt/perl/bin:/opt/scr/bin:/usr/sbin/diag/contrib:/opt/OpenSource/bin:/opt/gnome/bin:/opt/ipf/bin:/opt/aCC/bin:/opt/ansic/bin:/opt/langtools/bin:/opt/fortran90/bin:/opt/fortran90/contrib/bin:/opt/perf/bin:/opt/pascal/bin:/opt/STK/bin:/opt/imake/bin:/opt/ignite/bin:/opt/mozilla/bin:/usr/ui/class/com:.',
    'PWD':'/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal',
    'PYTHONPATH':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/Python/Lib:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/Python/Obj:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/exec/lbr:.',
    'PYTHONUNBUFFERED':'nobuffering',
    'SESSIONTYPE':'dt',
    'SESSION_MANAGER':'local/l-ecn001.engr.uiowa.edu:/var/spool/sockets/ICE/12664,tcp/l-ecn001.engr.uiowa.edu:56366',
    'SESSION_SVR':'l-ecn001.engr.uiowa.edu',
    'SHELL':'/bin/csh',
    'SHLIB_PATH':'/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/exec/lbr:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/Python/Obj/lbr:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/External/Acis:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/External:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/cae/External/ebt:/tmp_mnt/nfs/server04/local/vol00/apps/abaqus6/6.3-1/exec',
    'START':'/etc/kshrc',
    'SYSTEMTYPE':'HP-UX',
    'TERM':'dtterm',
    'TERMINAL_EMULATOR':'dtterm',
    'TMPDIR':'/var/tmp',
    'TZ':'CST6CDT',
    'USER':'mabhatti',
    'VISUAL':'/usr/local/bin/dxvi',
    'WINDOWID':'8388670',
    'XFORCE_INTERNET':'True',
    'XMBINDDIR':'/usr/dt/lib/bindings',
    'XMICONBMSEARCHPATH':'/user/eng/mabhatti/.dt/icons/%B%M.bm:/user/eng/mabhatti/.dt/icons/%B%M.pm:/user/eng/mabhatti/.dt/icons/%B:/etc/dt/appconfig/icons/%L/%B%M.bm:/etc/dt/appconfig/icons/%L/%B%M.pm:/etc/dt/appconfig/icons/%L/%B:/etc/dt/appconfig/icons/C/%B%M.bm:/etc/dt/appconfig/icons/C/%B%M.pm:/etc/dt/appconfig/icons/C/%B:/usr/dt/appconfig/icons/%L/%B%M.bm:/usr/dt/appconfig/icons/%L/%B%M.pm:/usr/dt/appconfig/icons/%L/%B:/usr/dt/appconfig/icons/C/%B%M.bm:/usr/dt/appconfig/icons/C/%B%M.pm:/usr/dt/appconfig/icons/C/%B',
    'XMICONSEARCHPATH':'/user/eng/mabhatti/.dt/icons/%B%M.pm:/user/eng/mabhatti/.dt/icons/%B%M.bm:/user/eng/mabhatti/.dt/icons/%B:/etc/dt/appconfig/icons/%L/%B%M.pm:/etc/dt/appconfig/icons/%L/%B%M.bm:/etc/dt/appconfig/icons/%L/%B:/etc/dt/appconfig/icons/C/%B%M.pm:/etc/dt/appconfig/icons/C/%B%M.bm:/etc/dt/appconfig/icons/C/%B:/usr/dt/appconfig/icons/%L/%B%M.pm:/usr/dt/appconfig/icons/%L/%B%M.bm:/usr/dt/appconfig/icons/%L/%B:/usr/dt/appconfig/icons/C/%B%M.pm:/usr/dt/appconfig/icons/C/%B%M.bm:/usr/dt/appconfig/icons/C/%B',
    'XSESSION':'12621',
    '_':'/usr/local/apps/abaqus/Commands/abq631',
}
status = 0
try:
    analysis = StandardAnalysis(args, env)
    analysis.run()
    driverUtils.sendJobCompletedMessage(args, env) 
except AbaqusExecutionError, exc:
    driverUtils.sendJobAbortedMessage(args, env) 
    status = 1
except AbaqusDriverException, exc:
    driverUtils.sendJobAbortedErrorMessage(args, env, exc.message()) 
    status = 1
except KeyboardInterrupt:
    driverUtils.sendJobInterruptedMessage(args, env) 
    status = 1
sys.exit(status)
