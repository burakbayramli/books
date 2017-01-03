#!/usr/bin/env python

import re

class QAdata:
    def __init__(self, question='', question_number=0,
                 answers=[], answer_type=''):
        self.question = question
        self.answers = answers
        self.answer_type = answer_type
        self.question_no = question_number
        self.user_input = []
        self.all_users_input = []

    def specifyAnswers(self, answer_list, answer_type):
        self.answers = answer_list
        self.answer_type = answer_type

    def usersAnswer(self, input):
        if type(input) is type(''): # is input a string?
            input = [input]  # convert to list
        self.user_input = input  # list of strings

    # print all data and add all user input:
    def write(self):
        return '%s\n%s\n%s\n%s\n%s\n' % \
               (str(self.question_no),self.question,self.answer_type,
                str(self.answers), 
                str(self.all_users_input + self.user_input))

    # print all data, but only current user's input:
    def writeSingle(self):
        return '%s\n%s\n%s\n%s\n%s\n' % \
               (str(self.question_no),self.question,self.answer_type,
                str(self.answers), 
                str(self.user_input))

    def read(self, file):
        self.question_no     =  int(file.readline())
        self.question        =      file.readline().strip()
        self.answer_type     =      file.readline().strip()
        self.answers         = eval(file.readline().strip())
        self.all_users_input = eval(file.readline().strip())

    def averageAnswer(self):
        # if all answers start with an integer, we compute the
        # average, otherwise we return None
        h = 1  # h is true if the answers have the right form
        for ans in self.answers:
            # regex: either number and something or just the number
            if not re.search(r'^(\d+)(\s|$)', ans):
                h = 0
                #print 'no match for opening integers:',ans
        if h:
            sum = 0
            for ans in self.all_users_input:
                sum = sum + int(re.search(r'^(\d+)', ans).group(1))
            n = len(self.all_users_input)
            if n == 0:
                return None   # no meaningful average anyway
            else:
                return float(sum/float(n))
        else:
            return None








