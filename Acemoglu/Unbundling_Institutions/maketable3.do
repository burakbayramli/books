***********************************************************
*Creates Table 3: First-Stage Regressions for Contracting and Property Rights Institutions (OLS, Sample of Ex-Colonies)
***********************************************************
clear
capture log close
cd G:\daron\unbundling
log using maketable3, replace

/*Data Files Used
	unbundle
	
*Data Files Created as Final Product
	none
	
*Data Files Created as Intermediate Product
	none*/
	
	
use unbundle, clear
keep if ex2col==1

***-----Panel A. Measure of Contracting Institutions

*Column 1: Legal Formalism

reg sdformalism sjlouk logem4  if ex2col==1&loggdppc1995~=.&conssj7000~=.

*Column 2: Legal Formalism

reg sdformalism  sjlouk lpd1500s  if ex2col==1&loggdppc1995~=.&conssj7000~=.

*Column 3: Procedural Complexity

reg ecproccompindex  sjlouk logem4  if ex2col==1&loggdppc1995~=.&conssj7000~=.

*Column 4: Procedural Complexity

reg ecproccompindex sjlouk lpd1500s  if ex2col==1&loggdppc1995~=.&conssj7000~=.

*Column 5: Number of Procedures

reg ecnumprocedures sjlouk logem4  if ex2col==1&loggdppc1995~=.&conssj7000~=.

*Column 6: Number of Procedures

reg ecnumprocedures sjlouk lpd1500s  if ex2col==1&loggdppc1995~=.&conssj7000~=.


***---Panel B. Measure of Property Rights Institutions

*Column 1: Constraint on Executive

reg conssj7000 sjlouk logem4 if ex2col==1&loggdppc1995~=.&sdformalism~=.

*Column 2: Constraint on Executive

reg conssj7000  sjlouk lpd1500s if ex2col==1&loggdppc1995~=.&sdformalism~=.

*Column 3: Protection Against Expropriation

reg avexpr  sjlouk logem4 if ex2col==1&loggdppc1995~=.&sdformalism~=.

*Column 4: Protection Against Expropriation

reg avexpr sjlouk lpd1500s if ex2col==1&loggdppc1995~=.&sdformalism~=.

*Column 5: Private Property

reg efhrpr7 sjlouk logem4 if ex2col==1&loggdppc1995~=.&sdformalism~=.

*Column 6: Private Property

reg efhrpr7 sjlouk lpd1500s if ex2col==1&loggdppc1995~=.&sdformalism~=.


