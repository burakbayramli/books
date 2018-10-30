from kivy.app import App
from kivy.uix.boxlayout import BoxLayout
# BEGIN ADDLOCATIONFORM
from kivy.properties import ObjectProperty  # <1>


class AddLocationForm(BoxLayout):
    search_input = ObjectProperty()  # <2>

    def search_location(self):
        print("Explicit is better than Implicit")
# END ADDLOCATIONFORM


class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
