from kivy.app import App
from kivy.uix.boxlayout import BoxLayout

# BEGIN ADDLOCATIONFORM
class AddLocationForm(BoxLayout):
    def search_location(self):  # <1>
        print("Explicit is better than implicit.")
# END ADDLOCATIONFORM

class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
