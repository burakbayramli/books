def write_XML(data, outfile, student_name_is_element=True):
    outfile.write('<root>\n')
    for name in data:
        if student_name_is_element:
            outfile.write("""\
<student>
  <name>%s</name>
""" % name)
        else:
            outfile.write("""\
<student name="%s">
""" % name)
        for course in data[name]:
            outfile.write("""\
  <course>
    <name>%s</name>
    <semester>%s</semester>
    <credit>%s</credit>
    <grade>%s</grade>
  </course>
""" % (course['title'], course['semester'],
       course['credit'], course['grade']))
        outfile.write('</student>\n')
    outfile.write('</root>\n')
                    
from xml.etree import ElementTree as ET
from students import load
data = load('students.dat')


outfile = open('students.xml', 'w')
write_XML(data, outfile)
outfile.close()

tree = ET.parse('students.xml')
from scitools.misc import dump
dump(tree)
r = tree.getroot()
print r.tag, len(r)  # root
students = r.getchildren()
for student in students:
    for course in student.getchildren():
        for tag in course.getchildren():
            print tag.tag, tag.text
    dump(student)


