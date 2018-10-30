# BEGIN IMPORT
from kivy.network.urlrequest import UrlRequest
# END IMPORT

from kivy.app import App
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import ObjectProperty


class AddLocationForm(BoxLayout):
    search_input = ObjectProperty()

    # BEGIN SEARCHLOCATION
    def search_location(self):
        search_template = "http://api.openweathermap.org/data/2.5/" +
            "find?q={}&type=like"  # <1>
        search_url = search_template.format(self.search_input.text)
        request = UrlRequest(search_url, self.found_location) # <2>

    def found_location(self, request, data):  # <3>
        cities = ["{} ({})".format(d['name'], d['sys']['country']) 
            for d in data['list']]  # <4>
        print("\n".join(cities))
    # END SEARCHLOCATION


class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
