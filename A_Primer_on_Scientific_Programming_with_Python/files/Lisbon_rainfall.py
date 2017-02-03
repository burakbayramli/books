"""
import urllib
url = 'http://www.worldclimate.com/cgi-bin/data.pl?ref=N38W009+2100+08535W'
local_file = 'Lisabon_rainfall.html'
urllib.urlretrieve(url, filename=local_file)
"""
infile = open('Lisabon_rainfall.html', 'r')
rainfall = []
for line in infile:
    if 'Weather station' in line:
        station = line.split('</strong>')[0].split('<strong>')[1]
        print station
    if '<td> mm <td' in line:
        data = line.split('<td align=right>')
        data[-1] = data[-1].split('<br>')[0]
        data = [float(x) for x in data[1:]]

print data

