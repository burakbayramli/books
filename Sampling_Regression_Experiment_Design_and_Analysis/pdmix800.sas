**** PDMIX800, for SAS Version 8 ******;
**** Modified 03-26-2002, error in by processing;
**** Modified 10-18-2001, printing changed again, turned off log notes;
**** Modified 6-8-2001, bug in slice and printing modified;
/*************************************************************
*    Copyright (C) 2000  Arnold M. Saxton (asaxton@utk.edu)  *
*      University of Tennessee, Knoxville TN 37996-4500      *
*    This program is free software; you can redistribute it  * 
*    and/or modify it under the terms of the GNU General     *
*    Public License as published by the Free Software        *
*    Foundation; either version 2 of the License, or         *
*    (at your option) any later version.  Basically all      *
*    copies, modifications or derivative works must allow    * 
*    the user to freely use the software, to copy, modify    *
*    and distribute, and must carry this same License for    *
*    free use. Source code must be distributed, but          *
*    distribution charges of any magnitude are permitted.    *
*                                                            *
*    This program is distributed in the hope that it will    * 
*    be useful, but WITHOUT ANY WARRANTY; without even the   * 
*    implied warranty of MERCHANTABILITY or FITNESS FOR A    *
*    PARTICULAR PURPOSE.  See the GNU General Public License * 
*    for more details.                                       *
*    A copy of the GNU General Public License can be obtained*
*    from Free Software Foundation, Inc., 59 Temple Place,   *
*    Suite 330, Boston, MA  02111-1307  USA                  *
*    or http://www.gnu.ai.mit.edu/copyleft/gpl.txt.          *
**************************************************************
ORIGINAL REFERENCE:
Saxton, A.M.  1998.  A macro for converting mean separation output to letter 
groupings in Proc Mixed.  In Proc. 23rd SAS Users Group Intl., SAS Institute, 
Cary, NC, pp1243-1246.

PURPOSE:
This macro takes two data sets from Proc MIXED (Version 8), created by the
 DIFFS option on the LSMEANS statement. If an ADJUST= option is used,
the pdiffs from this are used, not the unadjusted defaults.
The pdiffs are converted to groups, labeled by numbers, and this 
is merged onto the lsmeans data set.
The numbers are converted to letters, and for cases where more than 
26 letters are needed, sections of letters are coded.  For example, 
3 means might have the letters A, (2)A, and (3)A.  These 3 means 
are all different, because although all have the letter A, each A 
belongs to a different section, identified by (#).
CAUTIONS!!!!!!!
 Depends on computer using ASCII characters, with 32=blank and capital
 letters following this.
 Requires temporary SAS datasets MSGRPZZ, LSDVALZZ, PDTEMPZZ, PDTEMPZZZ, PDTEMPMZZ,
   so any existing SAS dataset with these names will be destroyed.
 There may be an IML limit of 90 total characters in the group 
  letter labels, but space for 200 are hardcoded.
 Since SAS/IML is used, this must be installed on the computer, along
  with BASE and STAT.

Parameters.
 -First required parameter must name a dataset created by 
  ODS OUTPUT DIFFS in proc mixed;
 -Second required parameter must name a dataset created by 
  ODS OUTPUT LSMEANS in proc mixed;
 -Optional parameters, given in any order, case insensitive.
   SORT=YES  - printing of means is in order of least square mean
               value.  Any value other than YES leaves means in
               the proc mixed sort order.
   ALPHA=.05 - critical probability value for deciding if means
               differ or not.  The default is .05, and values must
               be between 0 and 1.
   WORKSIZE=1 - number of Kb of memory for IML to use.  This should
                only be needed in very extreme circumstances as IML
                dynamically increases memory as needed.
   TEST0=YES  -  this requests that 3 variables (df, t, p) not be
                included in the printing.  Any value other than NO
                prints all variables produced by the lsmeans.
   MIXFMT=NO -  this removes the formatting assigned by proc mixed,
                which helps compress the page width of the output.
                This also will result in the means and std. errors
              being rounded, which usually is desirable.  Any value
                besides NO retains the proc mixed formatting.
   NUMLET=200 - This specifies maximum number of letters that will
                be permitted.  Many means may possibly require many
                letters, but memory requirements get excessive.  The
                default of 200 should fail only in unusual cases. If
                failure occurs (error message in log), rerun with this
                option set higher.
    SLICE=variables  Effects containing all the slice variables will
                be subdivided, and mean separation reporting done within
                slice levels.  Note that all comparisons are made, just
                reporting of comparisons across slice levels is suppressed.
                This is useful to reduce the complexity of letter groupings.

Example of use.
  Assume the file pdmix800.sas, containing the macro code,
  is on the a: drive.  Then the code below will run MIXED, and run
  pdmix800 on the lsmeans.  MIXED is told not to print the means and
  pdiffs, using the ODS exclude statement, as 
  pdmix800 does the printing in the more desirable format.  
  Also shown are two optional parameters.  

proc mixed;
 class block a b;
 model y = a b a*b;
 random block;
 lsmeans a b a*b/pdiff;
 ods output diffs=ppp lsmeans=mmm;
 ods listing exclude diffs lsmeans;
run;
%include 'a:pdmix800.sas';
%pdmix800(ppp,mmm,alpha=.01,sort=yes);

*****************************************************************/
%macro pdmix800(pname,lname,sort=NO,alpha=.05,worksize=1,test0=NO,
                mixfmt=YES,numlet=200,slice=);    %let printdebug=0;
*** check arguments;
%global bylistzz slicezz varlistzz;   **put out for possible use by backtrans;               
%let slicezz=&slice;
%local dsid chk3 error1 error neweffectlength lastslicevar var adjust bylist;
  %let error=0;
  %if %length(&lname)=0 %then %let error=1;
  %if  %sysfunc(exist(&lname)) %then %do;
     %let dsid=%sysfunc(open(&lname,I));
    %let chk3=%sysfunc(varnum(&dsid,ESTIMATE));
    %if &chk3=0 %then %let error=2;
    %let chk3=%sysfunc(varnum(&dsid,EFFECT));
    %if &chk3=0 %then %let error=2;
    %let dsid=%sysfunc(close(&dsid));
  %end;
  %else %let error=1;

  %if &error>0 %then %do;
   %if &error=1 %then %put WARNING: Dataset &lname does not exist.;
   %if &error=2 %then %put WARNING: Dataset &lname was not made by proc mixed.;
  %end;
  %let error1=&error;  

  %let error=0;
  %if %length(&pname)=0 %then %let error=1;
  %if %sysfunc(exist(&pname)) %then %do;
    %let dsid=%sysfunc(open(&pname,I));
    %let chk3=%sysfunc(varnum(&dsid,ESTIMATE));
    %if &chk3=0 %then %let error=3;
    %let chk3=%sysfunc(attrn(&dsid,nobs));
    %if &chk3=0 %then %let error=2;
    %let dsid=%sysfunc(close(&dsid));
  %end;
  %else %let error=1;

  %if &error>0 %then %do;
   %if &error=1 %then %put WARNING: Dataset &pname does not exist.;
   %if &error=2 %then %put WARNING: There are no observations in dataset &pname.;
   %if &error=3 %then %put WARNING: Dataset &pname was not made by proc mixed.;
  %end;
  %if (&error or &error1) %then %do;
   %put NOTE: PDMIX800 terminated due to errors in input values.;
   %goto skip;
  %end;

 %if &error %then %do;
   %put PDMIX800 terminated due to errors in input values.;
   %if &error=3 %then %put Alpha can only have values between 0 and 1.; 
   %if &error=4 %then %put ADJUST=Dunnett output not supported.;
   %goto skip;
 %end;
** save setting of notes option;
%let notesval=notes;
options nonotes;
%put PDMIX800 03.26.2002 processing;

****need list of variable names, either sliced or not;
data _null_;
 *** First get unique list of all names used in BY statements;
 *** these come before the variable EFFECT, but include EFFECT in list;
 dsid=open("&lname",'i');
 length namlist $ 512;
 ii=1;
 value=varname(dsid,ii);
 do while (value ^= 'Effect') ;
   if ii=1 then namlist=value;
   else namlist=trim(namlist)||' '||value;
   ii=ii+1;
   value=varname(dsid,ii);
 end;
 call symput('bylistzz',compbl(namlist)); **list without effect;
 if namlist='' then namlist=value;
 else namlist=trim(namlist)||' '||value;
 namlist=trim(namlist);
 call symput('bylist',namlist);   **list with effect;
****************************************************;
*** Now get list of all class variables (always between effect and estimate);
 length list list1 list2 $ 3200; 
 start=varnum(dsid,"EFFECT") +1;
 ii=1;jj=start;
 slicein=upcase("&slice");
 do while(ii);
  name=varname(dsid,jj);
  name1=upcase(name); **case sensitive names are returned by varname;
  type=vartype(dsid,jj);
  if name1 ^= 'ESTIMATE' then do;
    kk=indexw(slicein,name1);
    if kk=0 then do; list=compress(list||'='||name);
	  if type='N' then 
	   list2= trim(list2)||' left('||trim(name)||left(")= '_' and") ;
	  else list2= trim(list2)||' left('||trim(name)||left(")='' and") ;
    end;
    else do;
      if type='N' then 
         list1= trim(list1)||' left('||trim(name)||left(")='_' or") ;
      else list1= trim(list1)||' left('||trim(name)||left(")='' or") ;
	end;
    jj=jj+1; 
  end;
  else ii=0;
 end;
 list=substr(list,2);
 jj=length(list1); if jj>2 then list1=substr(list1,1,jj-2);
 list2=substr(list2,1,length(list2)-3);
 call symput('lastslicevar',scan("&slice",-1) );
 call symput('slice1',trim(list1));
 call symput('varlist1',trim(list2));
 list=translate(list,' ','=');
call symput ('varlistzz',trim(list));
run;
%if &printdebug=1 %then %do;
  %put bylist      &bylist;
  %put bylistzz    &bylistzz;
  %put varlistzz   &varlistzz;
  %put varlist1    &varlist1;
%end;
********** add variables to datasets ***************;
data pdtempzz; set &pname; by &bylist effect notsorted;
** if adjusted probs are not there, a LSD was used;
 if ADJP=. then do; ADJP=PROBT; ADJUSTMENT='LSD    '; end;
 length _mstech_ $ 30;
 if ADJUSTMENT ='' then _mstech_=compress('LSD(P<'||"&alpha"||')');
  else do;
    _mstech_=compress(ADJUSTMENT||'(P<'||"&alpha"||')' );
   if substr(ADJUSTMENT,1,7)='Dunnett' then call symput('error','4');
  end;
 *** numerical value check only possible in data step;
 if &alpha < 0.0 or &alpha > 1.0 then call symput('error','3');
 *** initalize slice indicator;
  sliceindzz=1;
  retain bygroup 0;
  if first.effect then bygroup=bygroup+1;
run;

%if %length(&slice) ne 0 %then %do;
*******************************************************************;
*******************************************************************;
*** sort, edit, relabel diff and mean data for the slice option ***;
*** this works by redefining effects that are being sliced ***;
*** Example:  In a 2*2 factorial, slicing the A*B interaction by A
***  means only 2 comparisons are needed of the 4*3/2=6 possible.
***  These are A1B1-A1B2  and  A2B1-A2B2;

*** sort and relabel lsmeans;
%if %length(&varlistzz)=0 %then %put ERROR: No variables left after slicing.;
%else %do;
data pdtempmzz; set &lname; by &bylist effect notsorted;
  retain bygroup 0;
  if first.effect then bygroup+1;
  sliceindzz=1;
run;
proc sort data=pdtempmzz; by bygroup &slice;
data pdtempmzz ;   set pdtempmzz; by bygroup &slice;
  retain slicecntzz  dothiseffectzz ;
 if first.bygroup then do;
  dothiseffectzz=0;
  slicecntzz=0; 
  *****test if effect should be sliced;
  sliceynzz=1;
  if not(&slice1) then do; **no slice vars missing;
	if not(&varlist1)  then dothiseffectzz=1;
  end;
 end;
 if first.&lastslicevar then  slicecntzz+1;
 if dothiseffectzz=1 then sliceindzz=slicecntzz;
 drop sliceynzz slicecntzz;
run;


*** now fix up diffs dataset;
data pdtempzzz; set pdtempmzz; by bygroup  &slice notsorted;
***copy slice definitions only ***;
 if first.&lastslicevar;
run;

proc sort data=pdtempzz ; by bygroup &slice;
data pdtempzz; merge pdtempzz (in=have) 
             pdtempzzz(keep= &slice dothiseffectzz bygroup sliceindzz); 
   by bygroup &slice;
   if have;
***compared factor levels must match on all slice variables;
  discardzz=0;
  if dothiseffectzz then do;
   %let ii=1;
   %let var=%scan(&slice,1);
   %do %while(%length(&var) ne 0);
       %let var2=_&var;
       %if %length(&var2)>32 %then %let var2=%substr(&var2,1,32);
       if &var ne &var2 then discardzz=1;
     %let ii=%eval (&ii+1);
     %let var=%scan(&slice,&ii);
   %end;
   if discardzz then delete;
  end;
 drop discardzz dothiseffectzz bygroup sliceindzz;
run;
data pdtempmzz; set pdtempmzz;
 drop  dothiseffectzz bygroup sliceindzz;
run;
%end;
%end;

%else %do;
  **must be created if no slicing;
  data pdtempmzz; set &lname; run;
%end;

**************************************************************;
*** ready to process for differences within each effect ***;
proc iml worksize=&worksize; reset nolog fw=7;  printdebug=0;
 alpha=&alpha;
 use pdtempmzz;  **for reading later;
 **** create mean separation output dataset with length 200;
 temp=j(1,&numlet,'0'); msgroup=rowcatc(temp); 
 ADJUSTMENT='                              ';
 create msgrpzz var{msgroup bygroup lsmrank ADJUSTMENT};

 **** create indexes of effect and by group locations;
 test='a'; ii=1;
 *** the diffs dataset from mixed has all the BY and CLASS
 ***  variable names ordered before the variable ESTIMATE.
 *** Names beginning with underscore are duplicates.
 *** Get all these variable names and read in levels;
 use pdtempzz;
 varlist= "&bylistzz &slice &varlistzz";
 value='a'; ii=1;
 do while (value ^= '') ;
  value=scan(varlist,ii);
  if value ^= '' then do;
    *** the BY variables are not guaranteed to be character,
    *** so convert them if necessary;
     read all var value into hold;
     if type(hold)='N' then level=level||char(hold);
     else level=level||hold;
     free hold;
  end;
  ii=ii+1;
 end;
if printdebug=1 then print  varlist level;
 if ncol(level)=0 then do;
   file log;
   put "NOTE: No variables found for use in &pname.";
   dataerr=1;
 end;
 else dataerr=0;
 if dataerr ^= 1 then do;	
   call change(level,'','-');
   level=rowcatc(level);
   idx=1;
   dim=nrow(level);
if printdebug=1 then print dim level;
 ***search down for number of comparisons in each section;
 ***read number of rows involving first mean to get number of means,
   then calculate number of comparisons; 
  byby=0;
  do jj=1 to dim;
    first=level[jj,1];
    byby=byby+1;
    **go to end of comparisons with mean 1;
    kk=jj; flag=1;
    do while(flag=1);
      kk=kk+1;
      if(kk > dim) then flag=0;
      else if (level[kk,1] ^= first) then flag=0;
    end;
    num=kk-jj+1;
    idx=idx || idx[1,byby] + num;
    jj=jj-1+num*(num-1)/2;  ** skip to next section;
   end;
  free level;
 end;
if printdebug=1 then print idx byby;
 ** BIG BB loop through rows of prob data
 ** subsetting out block dealing with each effect;
 pptr=1;  **points to where probs start for current means;
 do bygroup = 1 to byby;

  dim= idx[1,bygroup+1]-idx[1,bygroup];
  nn= dim*(dim-1)/2;
  
  **********************************************************;
  **for sorting letters need descending order, and antiranks;
  setin pdtempmzz;
  range=idx[1,bygroup] : idx[1,bygroup+1]-1 ;
  read point range var {ESTIMATE} into lsmcur;

  **stupid rank function fails on missing values;
  **so must temporarily make them non missing;
  test=lsmcur[><,]-1.e-30;
  locmiss=loc(lsmcur=.); kk=ncol(locmiss);
  if kk>0 then lsmcur[locmiss,]=test;
  lsmrnk=dim+1-rank(lsmcur);
  if kk>0 then lsmcur[locmiss,]=.;
  lsmarnk=lsmrnk;
  lsmarnk[lsmrnk,]=(1:(dim))`;
if printdebug=1 then print pptr nn;
**********************************************************;
**** get prob file data for these means. 
  _adjp_ contains the probs, no matter what adjust method;
  setin pdtempzz;
  range=pptr:pptr+nn-1;
  read point pptr var {_mstech_} into ADJUSTMENT;
  read point range var {ADJP} into data;
  pptr=pptr+nn;
if printdebug=1 then print data;
  *** put p values into matrix;
  p = j(dim,dim,0);
  kk=1; do ii=1 to dim-1; do jj=ii+1 to dim;
    if data[kk,1]=. then  p[jj,ii]=1;
    else  p[jj,ii] = data[kk,1];
    p[ii,jj]=p[jj,ii];  **fill in upper triangle for next sort;
    kk=kk+1;
 end;end;

  *** sort matrix by lsm value, so high mean gets first letter;
  temp=p;
  p[,lsmrnk]=temp;
  temp[lsmrnk,]=p;
  p=temp; free temp;
  if nn>&numlet then maxlet=&numlet; **memory use limit;
  else maxlet=nn+1;
  group = j(dim, maxlet, 0);
  members=j(dim,1,0);
if printdebug=1 then print p dim data;
  gcode=1; ngroup=1;
  do ii=1 to dim;
     kk=0;
     flag=0;
     do jj=ii+1 to dim;  * go down row, find group members ;
        if p[jj,ii] > alpha then do;   * jj and ii are the same ;
           * check jj against members ;
           do mm=1 to kk ;
              ll=members[mm,1];
              if jj>ll then test1=p[jj,ll];
              else    test1=p[ll,jj];
              if test1<0 then test1=-test1;
              if(test1 < alpha) then goto jmp0; * need new group ;
           end;
           jmp0:
           if mm=kk+1 then do;
              do mm=ii+1 to dim;
                 if mm=jj then mm=mm+1; *skip jj (on diagonal);
                 if mm>dim then go to jmp2;
                 if jj>mm then test1=p[jj,mm];
                 else    test1=p[mm,jj];
                 if test1 > alpha && -p[mm,ii] > alpha then do;
                 * previous grouped mean mm may belong in this group ;
                 * so check if already in and current members;
                 * dont conflict ;
                    do ll=1 to kk;
                       nn=members[ll,1];
                       if nn=mm then goto jmp1;
                       if nn<mm then test1=p[mm,nn];
                       else      test1=p[nn,mm];
                       if(test1<0.0) then test1=-test1;
                       if(test1<alpha) then goto jmp1;
                    end;
                    jmp1: if(ll=kk+1)then do;
                       group[mm,ngroup]=gcode;
                       kk=kk+1; members[ll,1]=mm;
                    end;
                 end;
              end;
       jmp2:  p[jj,ii]=-p[jj,ii];  * set so not put in next group ;
              do mm=1 to kk;
                 ll=members[mm,1];
                 * set so not used again ;
                 if ll<jj then do;
                   if p[jj,ll]>0 then  p[jj,ll]=-p[jj,ll]; end;
                 else do;
                 if p[ll,jj]>0 then p[ll,jj]=-p[ll,jj]; end;
              end;
              group[jj,ngroup]=gcode;
              kk=kk+1;  members[kk,1]=jj;
           end;
           else flag=1;
        end;
     end;
     if(kk=0) then do;  * no members ;
        do jj=1 to ngroup until (group[ii,jj] ^= 0) ; end;
        * not in a group yet, so set flag ;
        if(jj=ngroup+1) then   kk=kk+1;
     end;
     if(kk^=0) then do;   * need to set current mean ;
        group[ii,ngroup]=gcode;
        ngroup=ngroup+1; gcode=gcode+1;
        if ngroup > &numlet then do;
          ** number of letters needed exceeded maximum;
          jj=dim; ii=dim; **stop loops this way to avoid warnings;
          bygroup=byby; dataerr=1;
          call symput('error','1');
        end;
     end;
     if(flag^=0) then ii=ii-1; * need another group for this mean;
  end;
  if dataerr=0 then do; **skip below if error;
  ngroup=ngroup-1;
  group=group[,1:ngroup];

 ***** this section just takes the groups identified by numbers
       above and converts numbers to letters.  This depends on
       the ASCII character definitions, eg. 64 value below is what
       gets capital letters;

     *** write out letters;
     kk=nrow(group);
     do ii=1 to kk;
       gc='';nsect=1;
       do jj=1 to ngroup;
         mm=group[ii,jj];
         if mm > 0 then do; ** blanks are 0, do not do them;
           sect=floor((mm-1)/26);  *** 26 letters in alphabet;
           offset=mm-sect*26;
           sect=sect+1;
           if sect > nsect then do;
              nsect=sect;
              gc=gc||"("||char(sect)||")";
           end;
           gc=gc||byte(64+offset);
         end;
       end;
       lsmrank=lsmarnk[ii,1];
       msgroup=rowcatc(gc);
       ** save letters, by group and sort info;
       append var {msgroup bygroup lsmrank ADJUSTMENT};
     end;
   end; **dataerr;

end;  ** for the big bb loop over effect sections;
quit;

%if &error=1 %then %do;
   %put ERROR: PDMIX800 terminated due to exceeding NUMLET limit.;
   %goto skip;
%end;

**** put group letters back in original lsm order;
**** they were sorted so largest mean gets letter A;
proc sort data=msgrpzz; by bygroup lsmrank;

**** if means data set has single means (eg 0 df)
     then sort these to the bottom so they do not
     merge with the msgrp output;
data pdtempmzz; set pdtempmzz; by EFFECT notsorted;
 if first.EFFECT and last.EFFECT then
  df0=1;
 else df0=0;
run;
proc sort; by df0;
**** merge letters with means and print ****;
data msgrpzz; merge pdtempmzz msgrpzz; 
 drop lsmrank df0;
 label msgroup='Letter Group';
 if ESTIMATE=. then do;
    **do not print for missing means;
    msgroup='';
 end;
 %if %upcase(&mixfmt)=NO %then %do; format _all_; %end;

run;
data pdtempmzz; set pdtempmzz; drop df0; run;


*******************************************************************;
**** before printing, add the lsdvalues;

proc means noprint data=pdtempzz; by &bylist &slice notsorted;
 id df adjustment;
 var STDERR ;
 output out=lsdvalzz n=numcomp mean=meanse max=maxse min=minse;
run;
data lsdvalzz; set lsdvalzz;
 if upcase(substr(adjustment,1,3))='LSD' then critt=tinv( (1-&alpha/2),DF);
 if upcase(substr(adjustment,1,3))='BON' then critt=tinv( 1-&alpha/(2*numcomp), DF);
 if upcase(adjustment)='SIDAK' then do;
        prob=exp( log(1-&alpha/2) /numcomp );
        critt=tinv( prob  , DF);
 end;
 if upcase(adjustment)='SCHEFFE' then do;
       numdf=-1+(sqrt(1+8*numcomp)+1)/2;
       critt=sqrt(numdf*finv(1-&alpha,numdf,DF));
 end;
 if upcase(substr(adjustment,1,5))='TUKEY' then do;
       numdf=(sqrt(1+8*numcomp)+1)/2;  ** number of treatments;
       critt=probmc('RANGE', . , 1-&alpha,DF,numdf);
put critt;
       critt=critt/sqrt(2);  **adjust for tukey needing sd of mean, not diff;      
 end;
 AvgSigDiff=meanse*critt;
 MaxSigDiff=maxse*critt;
 MinSigDiff=minse*critt;
 keep &bylist &slice avgsigdiff maxsigdiff minsigdiff;
 format minsigdiff maxsigdiff avgsigdiff best7. ;
 put adjustment ' values for ' &bylist &slice ' are ' avgsigdiff ' (avg) ' minsigdiff ' (min) '  maxsigdiff  ' (max).' ;
run;
proc sort; by &bylist &slice;
proc sort data=msgrpzz; by &bylist  &slice;
proc sort; by ADJUSTMENT bygroup EFFECT;

******** print mean separation ************;
%if %upcase(&sort)=YES %then %do;
 proc sort data=msgrpzz; by ADJUSTMENT bygroup EFFECT descending ESTIMATE;
%end;
 %if %upcase(&test0)=NO  %then %do;
  data msgrpzz; set msgrpzz;
     drop tvalue probt df;
  run;
%end;
proc print data=msgrpzz label ; 
 by  effect adjustment bygroup notsorted;
 label bygroup='  Set'
       adjustment='  Method';
run;
*** restore notes option;
options &notesval;
%skip:
%mend;
