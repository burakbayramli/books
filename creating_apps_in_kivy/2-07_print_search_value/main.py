from kivy.app import App
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import ObjectProperty


class AddLocationForm(BoxLayout):
    search_input = ObjectProperty()

    # BEGIN SEARCHLOCATION
    def search_location(self):
        print("The user searched for '{}'".format(self.search_input.text))  # <1>
    # END SEARCHLOCATION


class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
