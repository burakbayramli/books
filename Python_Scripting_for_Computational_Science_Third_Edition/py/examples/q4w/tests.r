./tests.verify: test performed on 2000-08-04(basunus i686 running Linux)


#### Test: ./tests.verify running python q4w-generate.py example1.q4w language.en
The file language.en is not found

example1.q4w was successfully translated to a web form: example1.html
in the new subdirectory example1

CPU time of python: 0.1 seconds on basunus i686, Linux



----- appending file example1.html ------

<!-- file automatically generate by q4w-generate.py -->
<!-- from input file example1.q4w -->
<HTML>
<HEAD>
<TITLE>Questionnaire: example1</TITLE>
</HEAD>
<BODY BGCOLOR="white">
<FORM ACTION="q4w-feedback.cgi" METHOD="POST">
<!--  headline: -->
<CENTER>
<IMG SRC="colorline.gif">
<H1>A basic example on using the q4w tool</H1>
<IMG SRC="colorline.gif">
</CENTER>

<!--  Lines starting with # are comment lines, these will not appear -->
<!--  on the web page. Text without comment signs are inserted "as is". -->
<!--  If you want some kind of formatting, e.g., making the text appear -->
<!--  as a separate paragraph, suitable HTML statements must be inserted. -->

<P>
This is a self-explanatory introduction to the q4w tool for
specifying questionnaires on the web.
The questions and answers can be translated to valid HTML code,
yielding a Web page with input fields, buttons, etc.
By clicking on "submit" on this Web page, a program 
(CGI script) interprets the answers and stores them in a database.
Another program (CGI script) displays statistics about the answers
so far. 
<P>
Here are a few examples demonstrating the various types of
answers that can be used with the q4w tool:
<P>

<!--  notice that questions (Q) and answers (A) must be a single line! -->
<!--  single choice pull-down menu: -->


<OL>
<P><LI><H4>What is your total full-time experience with programming?</H4>

<SELECT NAME="1" SIZE=1>
<OPTION VALUE="1 week"> 1 week </OPTION>
<OPTION VALUE="2 months"> 2 months </OPTION>
<OPTION VALUE="more than 1 year"> more than 1 year </OPTION>
</SELECT><BR>

<!--  free text: -->
<P><LI><H4>Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)</H4>

<TEXTAREA COLS=40 ROWS=5 WRAP="hard" NAME="2"></TEXTAREA>

<!--  single choice (radio box) -->
<P><LI><H4>Specialization:</H4>

<INPUT TYPE="radio" NAME="3" VALUE="computer science">&nbsp; computer science <BR>
<INPUT TYPE="radio" NAME="3" VALUE="scientific computing">&nbsp; scientific computing <BR>
<INPUT TYPE="radio" NAME="3" VALUE="mathematics/physics">&nbsp; mathematics/physics <BR>
<INPUT TYPE="radio" NAME="3" VALUE="engineering">&nbsp; engineering <BR>
<INPUT TYPE="radio" NAME="3" VALUE="economics">&nbsp; economics <BR>
<INPUT TYPE="radio" NAME="3" VALUE="management">&nbsp; management <BR>
<INPUT TYPE="radio" NAME="3" VALUE="another subject">&nbsp; another subject <BR>

<!--  multiple choice (check box) -->
<P><LI><H4>Which of the following languages do you master ?</H4>

<INPUT TYPE="checkbox" NAME="4" VALUE="Perl">&nbsp; Perl <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="Tcl">&nbsp; Tcl <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="Python">&nbsp; Python <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="Java">&nbsp; Java <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="C">&nbsp; C <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="C++">&nbsp; C++ <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="F77">&nbsp; F77 <BR>
<INPUT TYPE="checkbox" NAME="4" VALUE="F90">&nbsp; F90 <BR>

<P><LI><H4>What's your favorite language?</H4>

<INPUT TYPE="radio" NAME="5" VALUE="Perl">&nbsp; Perl <BR>
<INPUT TYPE="radio" NAME="5" VALUE="Python">&nbsp; Python <BR>
<INPUT TYPE="radio" NAME="5" VALUE="Java">&nbsp; Java <BR>
<INPUT TYPE="radio" NAME="5" VALUE="C++">&nbsp; C++ <BR>
<INPUT TYPE="radio" NAME="5" VALUE="F90">&nbsp; F90 <BR>

<!--  one-line text field: -->
<P><LI><H4>In case your favorite language was not mentioned, please provide it:</H4>

<INPUT TYPE="text" NAME="6" SIZE="40">

<!--  single choice (grading with numbers) -->
<P><LI><H4>Evaluate your quality as a computer programmer:</H4>

<INPUT TYPE="radio" NAME="7" VALUE="1 (poor)">&nbsp; 1 (poor) <BR>
<INPUT TYPE="radio" NAME="7" VALUE="2">&nbsp; 2 <BR>
<INPUT TYPE="radio" NAME="7" VALUE="3">&nbsp; 3 <BR>
<INPUT TYPE="radio" NAME="7" VALUE="4">&nbsp; 4 <BR>
<INPUT TYPE="radio" NAME="7" VALUE="5">&nbsp; 5 <BR>
<INPUT TYPE="radio" NAME="7" VALUE="6 (excellent)">&nbsp; 6 (excellent) <BR>
<!--  answers starting with integers enable computation of -->
<!--  the average score on the output statistics -->

<!--  In case you want the form in another language, edit the -->
<!--  translations in the "language" file accordingly -->

</OL>  <!-- end of list of questions -->

<!-- transfer the basename of filenames through a "hidden" form -->
<INPUT TYPE="hidden" NAME="filenamebase" VALUE="example1">
<P>
<!-- the standard submit and reset buttons at the end -->
<INPUT TYPE="submit" VALUE="Submit" NAME="submitbutton">
<INPUT TYPE="reset"  VALUE="Reset"  NAME="resetbutton">
</FORM>

</BODY>
</HTML>

#### Test: ./tests.verify running python q4w-record.cgi
Content-type: text/html

<!-- written by q4w-statistics.cgi -->
<HTML>
<BODY BGCOLOR="white">
<HEAD><TITLE>Feedback</TITLE></HEAD>
<H1> The following answers were recorded :</H1>
<OL>
<P><LI><H4> What is your total full-time experience with programming? </H4>
1 week <BR>
<P><LI><H4> Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.) </H4>
just playing around <BR>
<P><LI><H4> Specialization: </H4>
computer science <BR>
<P><LI><H4> Which of the following languages do you master ? </H4>
Tcl <BR>
<P><LI><H4> What's your favorite language? </H4>
Perl <BR>
<P><LI><H4> In case your favorite language was not mentioned, please provide it: </H4>
No answer was given <BR>
<P><LI><H4> Evaluate your quality as a computer programmer: </H4>
2 <BR>
</OL>

View <A HREF="q4w-statistics.cgi?filenamebase=statistics">example1</A>!
</BODY>
</HTML>

CPU time of python: 0.1 seconds on basunus i686, Linux


#### Test: ./tests.verify running python q4w-record.cgi
Content-type: text/html

<!-- written by q4w-statistics.cgi -->
<HTML>
<BODY BGCOLOR="white">
<HEAD><TITLE>Feedback</TITLE></HEAD>
<H1> The following answers were recorded :</H1>
<OL>
<P><LI><H4> What is your total full-time experience with programming? </H4>
1 week <BR>
<P><LI><H4> Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.) </H4>
trying Java <BR>
<P><LI><H4> Specialization: </H4>
computer science <BR>
<P><LI><H4> Which of the following languages do you master ? </H4>
Java <BR>
<P><LI><H4> What's your favorite language? </H4>
Java <BR>
<P><LI><H4> In case your favorite language was not mentioned, please provide it: </H4>
No answer was given <BR>
<P><LI><H4> Evaluate your quality as a computer programmer: </H4>
2 <BR>
</OL>

View <A HREF="q4w-statistics.cgi?filenamebase=statistics">example1</A>!
</BODY>
</HTML>

CPU time of python: 0.1 seconds on basunus i686, Linux


#### Test: ./tests.verify running python q4w-statistics.cgi filenamebase=example1
Content-type: text/html

<!-- written by q4w-statistics.cgi -->
<HTML>
<BODY BGCOLOR="white">
<HEAD><TITLE> Statistics of a webform </TITLE></HEAD>
<H1>statistics: 2 answers</H1>
<TABLE BORDER="0" CELLPADDING="2" CELLSPACING="0" WIDTH="700">


<!-- question 1 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>What is your total full-time experience with programming?</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">1 week</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">100%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="400" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">2 months</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">more than 1 year</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>

no match for opening integers: more than 1 year


<!-- question 2 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="700" COLSPAN="5">just playing around</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="700" COLSPAN="5"><A HREF="ans.html#1">trying Java</A></TD></TR>


<!-- question 3 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>Specialization:</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">computer science</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">100%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="400" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">scientific computing</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">mathematics/physics</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">engineering</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">economics</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">management</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">another subject</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>

no match for opening integers: computer science
no match for opening integers: scientific computing
no match for opening integers: mathematics/physics
no match for opening integers: engineering
no match for opening integers: economics
no match for opening integers: management
no match for opening integers: another subject


<!-- question 4 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>Which of the following languages do you master ?</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Perl</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Tcl</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">50%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="200" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Python</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Java</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">50%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="200" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">C</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">C++</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">F77</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">F90</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>

no match for opening integers: Perl
no match for opening integers: Tcl
no match for opening integers: Python
no match for opening integers: Java
no match for opening integers: C
no match for opening integers: C++
no match for opening integers: F77
no match for opening integers: F90


<!-- question 5 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>What's your favorite language?</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Perl</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">50%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="200" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Python</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">Java</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">50%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="200" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">C++</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">F90</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>

no match for opening integers: Perl
no match for opening integers: Python
no match for opening integers: Java
no match for opening integers: C++
no match for opening integers: F90


<!-- question 6 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>In case your favorite language was not mentioned, please provide it:</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>


<!-- question 7 -->

<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>Evaluate your quality as a computer programmer:</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>

<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">1 (poor)</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">2</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">100%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="400" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">3</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">4</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">5</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">6 (excellent)</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">0%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="0" HEIGHT="10" BORDER="0"</TD>


<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">average = 2.0</TD>
</TR>


</TABLE>
</BODY>
</HTML>

CPU time of python: 0.2 seconds on basunus i686, Linux



----- appending file ans.html ------

<!-- written by q4w-statistics.cgi -->
<HTML><BODY BGCOLOR="white">
<HEAD><TITLE>Individual answers of a webform</TITLE></HEAD>
<H3><A NAME="0">Answer no. 1</A></H3>
<OL>
<P><LI><H4>What is your total full-time experience with programming?</H4>
1 week<BR>
<P><LI><H4>Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)</H4>
just playing around<BR>
<P><LI><H4>Specialization:</H4>
computer science<BR>
<P><LI><H4>Which of the following languages do you master ?</H4>
Tcl<BR>
<P><LI><H4>What's your favorite language?</H4>
Perl<BR>
<P><LI><H4>In case your favorite language was not mentioned, please provide it:</H4>
No answer was given
<BR>
<P><LI><H4>Evaluate your quality as a computer programmer:</H4>
2<BR>
</OL>
<P>
<H3><A NAME="1">Answer no. 2</A></H3>
<OL>
<P><LI><H4>What is your total full-time experience with programming?</H4>
1 week<BR>
<P><LI><H4>Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)</H4>
trying Java<BR>
<P><LI><H4>Specialization:</H4>
computer science<BR>
<P><LI><H4>Which of the following languages do you master ?</H4>
Java<BR>
<P><LI><H4>What's your favorite language?</H4>
Java<BR>
<P><LI><H4>In case your favorite language was not mentioned, please provide it:</H4>
No answer was given
<BR>
<P><LI><H4>Evaluate your quality as a computer programmer:</H4>
2<BR>
</OL>
<P>
</BODY>
</HTML>


----- appending file example1.dball ------
#------------- new user input -----------
#------- question 1 -------
1
What is your total full-time experience with programming?
pull-down-menu
['1 week', '2 months', 'more than 1 year']
['1 week']
#------- question 2 -------
2
Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)
free-text
['']
['just playing around']
#------- question 3 -------
3
Specialization:
single-choice
['computer science', 'scientific computing', 'mathematics/physics', 'engineering', 'economics', 'management', 'another subject']
['computer science']
#------- question 4 -------
4
Which of the following languages do you master ?
multiple-choice
['Perl', 'Tcl', 'Python', 'Java', 'C', 'C++', 'F77', 'F90']
['Tcl']
#------- question 5 -------
5
What's your favorite language?
single-choice
['Perl', 'Python', 'Java', 'C++', 'F90']
['Perl']
#------- question 6 -------
6
In case your favorite language was not mentioned, please provide it:
one-line-text
[]
[]
#------- question 7 -------
7
Evaluate your quality as a computer programmer:
single-choice
['1 (poor)', '2', '3', '4', '5', '6 (excellent)']
['2']
#------------- new user input -----------
#------- question 1 -------
1
What is your total full-time experience with programming?
pull-down-menu
['1 week', '2 months', 'more than 1 year']
['1 week']
#------- question 2 -------
2
Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)
free-text
['']
['trying Java']
#------- question 3 -------
3
Specialization:
single-choice
['computer science', 'scientific computing', 'mathematics/physics', 'engineering', 'economics', 'management', 'another subject']
['computer science']
#------- question 4 -------
4
Which of the following languages do you master ?
multiple-choice
['Perl', 'Tcl', 'Python', 'Java', 'C', 'C++', 'F77', 'F90']
['Java']
#------- question 5 -------
5
What's your favorite language?
single-choice
['Perl', 'Python', 'Java', 'C++', 'F90']
['Java']
#------- question 6 -------
6
In case your favorite language was not mentioned, please provide it:
one-line-text
[]
[]
#------- question 7 -------
7
Evaluate your quality as a computer programmer:
single-choice
['1 (poor)', '2', '3', '4', '5', '6 (excellent)']
['2']


----- appending file example1.dbsum ------
#------- question 1 -------
1
What is your total full-time experience with programming?
pull-down-menu
['1 week', '2 months', 'more than 1 year']
['1 week', '1 week']
#------- question 2 -------
2
Give a short description of the type of programming you have experience with (university courses, industrial projects, games etc.)
free-text
['']
['just playing around', 'trying Java']
#------- question 3 -------
3
Specialization:
single-choice
['computer science', 'scientific computing', 'mathematics/physics', 'engineering', 'economics', 'management', 'another subject']
['computer science', 'computer science']
#------- question 4 -------
4
Which of the following languages do you master ?
multiple-choice
['Perl', 'Tcl', 'Python', 'Java', 'C', 'C++', 'F77', 'F90']
['Tcl', 'Java']
#------- question 5 -------
5
What's your favorite language?
single-choice
['Perl', 'Python', 'Java', 'C++', 'F90']
['Perl', 'Java']
#------- question 6 -------
6
In case your favorite language was not mentioned, please provide it:
one-line-text
[]
[]
#------- question 7 -------
7
Evaluate your quality as a computer programmer:
single-choice
['1 (poor)', '2', '3', '4', '5', '6 (excellent)']
['2', '2']
