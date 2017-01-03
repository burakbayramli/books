#!/usr/bin/python
import sys, cgi, re
print "Content-type: text/html"
print
try:
    from QAdata import *
except:
    print "Could not import QAdata.py"
    print "This CGI script is located in a wrong directory"
    sys.exit(1)

# indispensable for debugging(!):
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
print "<HEAD><TITLE>Feedback</TITLE></HEAD>"
print "<H1>", translate['The following answers were recorded'], ":</H1>"


# load data structure from file:
qa = []
dbfile = open(filenamebase+".dbsum", 'r')
while 1:
    line = dbfile.readline()  # try to read record separator
    if not line: break
    #print "got line",line,"<P>"
    item = QAdata()
    item.read(dbfile)
    qa.append(item)
dbfile.close()

#print "Let's see what we found in the file:"
#for item in qa:
#    print re.sub(r"\n","<BR>\n",item.write())

# read the form
#print "form.keys=",form.keys()
for name in form.keys():
    #print "form: name='%s'" % name
    input = form.getvalue(name)

    # equivalent code:
    # formname is a list of multiple answers or a single str
    #formname = form[name]
    #if type(formname) is type([]):  # list?
    #    # make a list of all the strings the user provided:
    #    input = []
    #    for item in formname: input.append(item.value)
    #    #print "choice=",input
    #else:
    #    # single answer, formname has a value attribute (a string)
    #    # containing the user's answer:
    #    input = formname.value
    #    #print "value=", input
    # if name is a number, it's a question => update qa
    if re.search(r"^[0-9]+$", name):
        qno = int(name)  # question number
        #qa[qno-1].usersAnswer(input)
	try:
            qa[qno-1].usersAnswer(input)
        except:
            print "Question", qno, " out of range...:\n", input

# write out the user's answers as an HTML list:
print "<OL>"
for item in qa:
    print "<P><LI><H4>",item.question,"</H4>"
    if len(item.user_input) == 0:
        print translate["No answer was given"],"<BR>"
    else:
        for choice in item.user_input:
            print choice,"<BR>"
print "</OL>"

# store qa on file, with this and other users' input (write):
# the rest:
dbfile = open(filenamebase+".dbsum", 'w')
for item in qa:
    dbfile.write("#------- question %d -------\n" % item.question_no)
    dbfile.write(item.write())
dbfile.close()

# append qa on file, with only this users' input (writeSingle):
# the rest:
dbfile = open(filenamebase+".dball", 'a')
dbfile.write("#------------- new user input -----------\n")
for item in qa:
    dbfile.write("#------- question %d -------\n" % item.question_no)
    dbfile.write(item.writeSingle())
dbfile.close()

# write statistics?
if translate['View'] != '':
    print """
%s <A HREF="q4w-statistics.cgi?filenamebase=%s">%s</A>!
</BODY>
</HTML>
""" % (translate['View'], filenamebase, translate['statistics'])

