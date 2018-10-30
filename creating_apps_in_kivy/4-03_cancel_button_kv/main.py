from kivy.network.urlrequest import UrlRequest
import json

from kivy.app import App
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import ObjectProperty
from kivy.uix.listview import ListItemButton
from kivy.factory import Factory


class LocationButton(ListItemButton):
    pass


class AddLocationForm(BoxLayout):
    search_input = ObjectProperty()
    search_results = ObjectProperty()

    def search_location(self):
        search_template = "http://api.openweathermap.org/data/2.5/find?q={}&type=like"
        search_url = search_template.format(self.search_input.text)
        request = UrlRequest(search_url, self.found_location)

    def found_location(self, request, data):
        data = json.loads(data.decode()) if not isinstance(data, dict) else data
        cities = ["{} ({})".format(d['name'], d['sys']['country']) for d in data['list']]
        self.search_results.item_strings = cities
        del self.search_results.adapter.data[:]
        self.search_results.adapter.data.extend(cities)
        self.search_results._trigger_reset_populate()


class WeatherRoot(BoxLayout):
    # BEGIN CURRENT_WEATHER
    current_weather = ObjectProperty()  # <1>

    def show_current_weather(self, location=None):  # <2>
        self.clear_widgets()

        if location is None and self.current_weather is None:  # <3>
            location = "New York (US)"
        if location is not None:  # <4>
            self.current_weather = Factory.CurrentWeather()
            self.current_weather.location = location
        self.add_widget(self.current_weather)  # <5>
    # END CURRENT_WEATHER

    def show_add_location_form(self):
        self.clear_widgets()
        self.add_widget(AddLocationForm())


class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
