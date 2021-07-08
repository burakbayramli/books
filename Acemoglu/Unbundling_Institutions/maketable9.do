***********************************************************
*Creates Table 9: Determinants of Contracting and Property Rights Institutions: Sample of Ex-Colonies
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable9, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
	
use unbundle_firm, clear


keep ex2col wb_code code_country logem4 lpd1500s sjlouk vsal xcon1990sj sdformalism apay gcorr law_pred qcrt averagecourt cmpe

label var apay "frequency of additional payments"
label var gcorr "corruption of gov officials"
label var law_pred "predict of legis and regulation"
label var qcrt "quality of courts"
label var averagecourt "judic function bus disputes"
label var cmpe "violation copyrights etc"
label var vsal "firm-level sales"
label var code_country "country number code"
label var wb_code "World Bank 3-letter country code"
label var  xcon1990sj "constraint on exec 1990s av."

*using the wbes firm-level data
***********************************************
*controlling for value of sales: vsal drops 500 firms and 1 country
*must use ex2col==1, otherwise get China and Thailand
*
**************
*PANEL C

*replace apay=(-1)*apay
*sign change just for easier display/make consistent with other scores, so higher is "worse"
*col 1
reg apay logem4 sjlouk vsal  if ex2col==1 & logem4~=. & vsal~=.&xcon1990sj~=.&sdformalism~=., cluster(wb_code)
*reduced form with log pop density
*col 2
reg apay lpd1500s sjlouk if ex2col==1 & vsal~=.&xcon1990sj~=.&sdformalism~=., cluster(wb_code)
*******
*corruption:
*col 3
reg gcorr logem4 sjlouk  vsal if ex2col==1 & logem4~=. & vsal~=.&xcon1990sj~=.&sdformalism~=., cluster(wb_code)
*reduced form with log pop density
*col 4
reg gcorr lpd1500s sjlouk   vsal if ex2col==1 & vsal~=.&xcon1990sj~=.&sdformalism~=., cluster(wb_code)
************************
*col 5
reg law_pred logem4 sjlouk vsal  if ex2col==1 & logem4~=. & vsal~=.&xcon1990sj~=.&sdformalism~=., cluster(wb_code)
*reduced form with log pop density
*col 6
reg law_pred lpd1500s sjlouk if ex2col==1 & vsal~=.&xcon1990sj~=.&sdformalism~=., cluster(wb_code)
********************


*Panel B
*col 1
ivreg apay (xcon1990sj sdformalism = logem4 sjlouk) vsal  if ex2col==1, cluster(wb_code) first
*first stage separately
reg xcon1990sj logem4 sjlouk vsal if ex2col==1&sdformalism~=.&apay~=., cluster(wb_code)
reg sdformalism logem4 sjlouk vsal if ex2col==1&xcon1990sj~=.&apay~=., cluster(wb_code)

*col 2
ivreg apay (xcon1990sj sdformalism = lpd1500s sjlouk)  vsal if ex2col==1 & vsal~=., cluster(wb_code) first
*first stage
reg xcon1990sj lpd1500s sjlouk vsal if ex2col==1&sdformalism~=.&apay~=., cluster(wb_code)
reg sdformalism lpd1500s sjlouk vsal if ex2col==1&xcon1990sj~=.&apay~=., cluster(wb_code)
*********
*col 3
ivreg gcorr (xcon1990sj sdformalism = logem4 sjlouk) vsal  if ex2col==1, cluster(wb_code) first
*
reg xcon1990sj logem4 sjlouk vsal if ex2col==1&sdformalism~=.&gcorr~=., cluster(wb_code)
reg sdformalism logem4 sjlouk vsal if ex2col==1&xcon1990sj~=.&gcorr~=., cluster(wb_code)
*col 4
ivreg gcorr (xcon1990sj sdformalism = lpd1500s sjlouk)  vsal if ex2col==1 & vsal~=., cluster(wb_code) first
*
reg xcon1990sj lpd1500s sjlouk vsal if ex2col==1&sdformalism~=.&gcorr~=., cluster(wb_code)
reg sdformalism lpd1500s sjlouk vsal if ex2col==1&xcon1990sj~=.&gcorr~=., cluster(wb_code)
*********************
*col 5
ivreg law_pred (xcon1990sj sdformalism = logem4 sjlouk) vsal  if ex2col==1, cluster(wb_code) first
*first stage separately
reg xcon1990sj logem4 sjlouk vsal if ex2col==1&sdformalism~=.&law_pred~=., cluster(wb_code)
reg sdformalism logem4 sjlouk vsal if ex2col==1&xcon1990sj~=.&law_pred~=., cluster(wb_code)

*col 6
ivreg law_pred (xcon1990sj sdformalism = lpd1500s sjlouk)  vsal if ex2col==1 & vsal~=., cluster(wb_code) first
*first stage
reg xcon1990sj lpd1500s sjlouk vsal if ex2col==1&sdformalism~=.&law_pred~=., cluster(wb_code)
reg sdformalism lpd1500s sjlouk vsal if ex2col==1&xcon1990sj~=.&law_pred~=., cluster(wb_code)
*********************
