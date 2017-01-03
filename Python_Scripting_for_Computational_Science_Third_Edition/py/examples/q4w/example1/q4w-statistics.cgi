#!/usr/bin/python
import sys, cgi
print "Content-type: text/html"
print
try:
    from QAdata import *
except:
    print "Could not import QAdata.py"
    print "This CGI script is located in a wrong directory"
    sys.exit(1)

sys.stderr = sys.stdout

form = cgi.FieldStorage()
if form.has_key("filenamebase"):
    filenamebase = form['filenamebase'].value
else:
    #print "Could not extract filenamebase from form"
    #sys.exit(1)
    filenamebase = sys.argv[1]

# we need language-specific text for the feedback:
import glob
languagefile = glob.glob("language.*")[0]
try:
    execfile(languagefile)  # initialize the translate dictionary
except:
    print "The %s file contains an error or is missing!" % languagefile
    sys.exit(1)

print "<!-- written by q4w-statistics.cgi -->"
print "<HTML>"
print """<BODY BGCOLOR="white">"""
print "<HEAD><TITLE>",translate['Statistics of a webform'],"</TITLE></HEAD>"

# load data structure from .dbsum file:
qa = []
dbfile = open(filenamebase+".dbsum", 'r')
while 1:
    line = dbfile.readline()  # try to read record separator
    if not line: break
    item = QAdata()
    item.read(dbfile)
    qa.append(item)
dbfile.close()

# load independent users' answers from the .dball file:
all_qa = []
dbfile = open(filenamebase+".dball", 'r')
while 1:
    # try to read record separator between individual users:
    line = dbfile.readline()
    if not line: break
    one_user = []
    for i in range(len(qa)):
        line = dbfile.readline()  # read record separator
        # should not happen: if not line: break
        item = QAdata()
        item.read(dbfile)
        one_user.append(item)
    all_qa.append(one_user)
dbfile.close()

# write an HTML files with each user's input:
ansfile = open("ans.html", 'w')
ansfile.write("""
<!-- written by q4w-statistics.cgi -->
<HTML><BODY BGCOLOR="white">
<HEAD><TITLE>%s</TITLE></HEAD>
""" % translate['Individual answers of a webform'])
for i in range(len(all_qa)):
    # write out the user's answers as an HTML list:
    ansfile.write("""<H3><A NAME="%d">%s %d</A></H3>\n<OL>\n""" \
                 % (i, translate['Answer no.'], i+1))
    for item in all_qa[i]:
        ansfile.write("<P><LI><H4>" + item.question + "</H4>\n")
        if len(item.all_users_input) == 0:
            ansfile.write(translate["No answer was given"] + "\n<BR>\n")
        else:
            for choice in item.all_users_input:
                ansfile.write(choice + "<BR>\n")
    ansfile.write("</OL>\n<P>\n")
ansfile.write("</BODY>\n</HTML>\n")
ansfile.close()

def findlink(question, users_answer):
    # find link to individual answer file (ans3.html f.ex.) by
    # searching for the given question and user's answer
    for i in range(len(all_qa)):
        for item in all_qa[i]:
            try:  # might have empty item.all_users_input ...
                if item.question == question and \
                   item.all_users_input[0] == users_answer:
                       # link = "ans" + str(i) + ".html"
                       return i
            except: pass
    return None
    

print "<H1>%s: %d %s</H1>" % \
  (translate['statistics'], len(all_qa), translate['answers'])

# print answers using a table environment:
print """<TABLE BORDER="0" CELLPADDING="2" CELLSPACING="0" WIDTH="700">"""

for item in qa:
    print "\n\n<!-- question %d -->" % item.question_no
    print """
<TR><TD>&nbsp;&nbsp;</TD></TR>
<TR><TD WIDTH="700" COLSPAN="5"><B>%s</B></TD></TR>
<TR><TD>&nbsp;&nbsp;</TD></TR>""" % item.question
    if item.answer_type == "free-text" or \
        item.answer_type == "one-line-text":
        # just list all the answer we got:
        for input in item.all_users_input:
            if input != "":
                # if possible, insert a link from this text answer to
                # a page with all answers from this user:
                link = findlink(item.question, input)
                if link:
                    print """
<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="700" COLSPAN="5"><A HREF="ans.html#%d">%s</A></TD></TR>""" % (link,input)
                else:
                    print """
<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="700" COLSPAN="5">%s</TD></TR>""" % input
    else:
        # make a table and some graphic illustrations:
        nusers = len(item.all_users_input)
        histogram = {}
        for answer in item.answers: histogram[answer] = 0.0
        for input in item.all_users_input: 
            histogram[input] = histogram[input] + 1.0
        for answer in item.answers:
            if nusers > 0: 
                histogram[answer] = histogram[answer]/float(nusers)
            percent = histogram[answer]*100
            print """
<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">%s</TD>
    <TD WIDTH="30" ALIGN="right"><FONT COLOR="red">%d%%</FONT></TD>
    <TD>&nbsp;&nbsp;</TD>
    <TD WIDTH="400"><IMG SRC="percent.gif" WIDTH="%d" HEIGHT="10" BORDER="0"</TD>
""" % (answer,int(percent+0.5),int(4*percent))
        avg = item.averageAnswer()
	if avg:
	    print """
<TR><TD WIDTH="90">&nbsp;&nbsp;</TD>
    <TD WIDTH="350">%s = %3.1f</TD>
</TR>
""" % (translate['average'], avg)

print """
</TABLE>
</BODY>
</HTML>
"""









