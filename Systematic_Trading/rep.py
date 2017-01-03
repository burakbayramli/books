# change all logging statements to print() statements
import os, re, sys
filename = sys.argv[1]
content = open(filename).read()
fout = open(filename,"w")

# you can insert stuff
fout.write("import inspect\n")

# or replace stuff 

#ps = "print(__file__ + \" \" + "
ps = "print(__file__ + \":\" + str(inspect.getframeinfo(inspect.currentframe())[:3][1]) + \":\" +"

content = content.replace("log.terse(",ps)
content = content.replace("log.msg(",ps)
content = content.replace("this_stage.print","print")
content = content.replace("self.print","print")
content = content.replace("rules_stage.print","print")
content = re.sub("print(.*?),\n(.*?)\n","print\\1)\n",content,re.DOTALL)
#content = content.replace(", instrument_code=instrument_code","")

fout.write(content)
fout.close()
