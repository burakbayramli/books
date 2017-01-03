import unittest

from agm1 import encode, decode, transmit

class Testagm1(unittest.TestCase):

    def setUp(self):
        self.correct_encoding = {
            (0,1,0,1,0,1,0,1): (0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1),
            (0,0,0,1,0,1,0,1): (0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0),
            (0,1,0,1,1,1,0,0): (0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0),
            (0,0,0,0,0,0,0,0): (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)}
        self.correct_decoding = {
            (0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1) :
            (0, 1, 0, 0, 0, 1, 1, 1),
            (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) :
            (1, 1, 1, 1, 1, 1, 1, 1),
            (0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0) :
            (0, 1, 0, 1, 1, 1, 0, 1),
            (0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1) :
            (0, 0, 0, 0, 1, 1, 0, 1)}
            
    def test_encode(self):
        for message, encoded_message in self.correct_encoding.items():
            self.assertEqual(tuple(encode(message)),self.correct_encoding[message])

    def test_transmit(self):
        # not much of a test!
        for encoded_message in self.correct_encoding.values():
            transmitted = transmit(encoded_message)
            self.assertEqual(len(transmitted),16)
            for bit in transmitted:
                self.assert_(bit==0 or bit==1)

    def test_decode(self):
        for message, decoded_message in self.correct_decoding.items():
            self.assertEqual(tuple(decode(message)),self.correct_decoding[message])

    
suite = unittest.makeSuite(Testagm1)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
