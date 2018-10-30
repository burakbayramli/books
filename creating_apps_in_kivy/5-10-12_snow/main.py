from kivy.network.urlrequest import UrlRequest
import json
# BEGIN IMPORTS
import random
from kivy.graphics import Color, Ellipse
from kivy.clock import Clock
# END IMPORTS
from kivy.app import App
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import (ObjectProperty, ListProperty, StringProperty,
    NumericProperty)
from kivy.uix.listview import ListItemButton
from kivy.factory import Factory


class LocationButton(ListItemButton):
    location = ListProperty()


class AddLocationForm(BoxLayout):
    search_input = ObjectProperty()
    search_results = ObjectProperty()

    def search_location(self):
        search_template = "http://api.openweathermap.org/data/2.5/find?q={}&type=like"
        search_url = search_template.format(self.search_input.text)
        request = UrlRequest(search_url, self.found_location)

    def found_location(self, request, data):
        data = json.loads(data.decode()) if not isinstance(data, dict) else data
        cities = [(d['name'], d['sys']['country']) for d in data['list']]
        self.search_results.item_strings = cities
        del self.search_results.adapter.data[:]
        self.search_results.adapter.data.extend(cities)
        self.search_results._trigger_reset_populate()

    def args_converter(self, index, data_item):
        city, country = data_item
        return {'location': (city, country)}


class CurrentWeather(BoxLayout):
    location = ListProperty(['New York', 'US'])
    conditions = ObjectProperty()
    temp = NumericProperty()
    temp_min = NumericProperty()
    temp_max = NumericProperty()

    def update_weather(self):
        weather_template = "http://api.openweathermap.org/data/2.5/weather?q={},{}&units=metric"
        weather_url = weather_template.format(*self.location)
        request = UrlRequest(weather_url, self.weather_retrieved)
        
    def weather_retrieved(self, request, data):
        data = json.loads(data.decode()) if not isinstance(data, dict) else data
        self.render_conditions(data['weather'][0]['description'])
        self.temp = data['main']['temp']
        self.temp_min = data['main']['temp_min']
        self.temp_max = data['main']['temp_max']

    def render_conditions(self, conditions_description):
        if "clear" in conditions_description.lower():
            conditions_widget = Factory.ClearConditions()
        # BEGIN RENDER_SNOW
        elif "snow" in conditions_description.lower():
            conditions_widget = SnowConditions()
        # END RENDER_SNOW
        else:
            conditions_widget = Factory.UnknownConditions()
        conditions_widget.conditions = conditions_description
        self.conditions.clear_widgets()
        self.conditions.add_widget(conditions_widget)


class Conditions(BoxLayout):
    conditions = StringProperty()


# BEGIN SNOW_CONDITIONS
class SnowConditions(Conditions):
    FLAKE_SIZE = 5  # <1>
    NUM_FLAKES = 60
    FLAKE_AREA = FLAKE_SIZE * NUM_FLAKES
    FLAKE_INTERVAL = 1.0 / 30.0

    def __init__(self, **kwargs):
        super(SnowConditions, self).__init__(**kwargs)
        self.flakes = [[x * self.FLAKE_SIZE, 0]
            for x in range(self.NUM_FLAKES)]  # <2>

        Clock.schedule_interval(self.update_flakes, self.FLAKE_INTERVAL)  # <3>

    def update_flakes(self, time):
        for f in self.flakes:  # <4>
            f[0] += random.choice([-1, 1])
            f[1] -= random.randint(0, self.FLAKE_SIZE)
            if f[1] <= 0:
                f[1] = random.randint(0, int(self.height))

        self.canvas.before.clear()
        with self.canvas.before:  # <5>
            widget_x = self.center_x - self. FLAKE_AREA / 2  # <6>
            widget_y = self.pos[1]
            for x_flake, y_flake in self.flakes:
                x = widget_x + x_flake  # <7>
                y = widget_y + y_flake
                Color(0.9, 0.9, 1.0)  # <8>
                Ellipse(pos=(x, y), size=(self.FLAKE_SIZE, self.FLAKE_SIZE))
# END SNOW_CONDITIONS


class WeatherRoot(BoxLayout):
    current_weather = ObjectProperty()

    def show_current_weather(self, location=None):
        self.clear_widgets()

        if self.current_weather is None:
            self.current_weather = CurrentWeather()

        if location is not None:
            self.current_weather.location = location

        self.current_weather.update_weather()
        self.add_widget(self.current_weather)

    def show_add_location_form(self):
        self.clear_widgets()
        self.add_widget(AddLocationForm())


class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
