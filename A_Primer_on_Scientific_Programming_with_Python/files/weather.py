import urllib

cities = {
    'Sandefjord':
    'http://weather.yahoo.com/forecast/NOXX0032_c.html',
    'Oslo':
    'http://weather.yahoo.com/forecast/NOXX0029_c.html',
    'Gothenburg':
    'http://weather.yahoo.com/forecast/SWXX0007_c.html',
    'Copenhagen':
    'http://weather.yahoo.com/forecast/DAXX0009_c.html',
    }

def get_data(url):
    urllib.urlretrieve(url=url, filename='tmp_weather.html')

    infile = open('tmp_weather.html')
    lines = infile.readlines()
    for i in range(len(lines)):
        line = lines[i]  # short form
        if 'Current conditions' in line:
            weather = lines[i+1][4:-6]
        if 'forecast-temperature' in line:
            temperature = float(lines[i+1][4:].split('&')[0])
            break  # everything is found, jump out of loop
    infile.close()
    return weather, temperature

for city in cities:
    weather, temperature = get_data(cities[city])
    print city, weather, temperature
