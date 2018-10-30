from kivy.app import App
from kivy.uix.boxlayout import BoxLayout  # <1>


class AddLocationForm(BoxLayout):  # <2>
    pass


class WeatherApp(App):
    pass

if __name__ == '__main__':
	WeatherApp().run()
