#!/usr/bin/env python3

import logging
import unittest
from collections import namedtuple

from katzplotkinpy.cli import parse_args

# FIXME Rewrite this test with new CLI

InputArg = namedtuple("InputArg", ["name", "arg", "expect"])


logger = logging.getLogger(__name__)


class TestCLI(unittest.TestCase):
    def test_parse_args(self):
        args_test = [
            InputArg("program", "afgen", "afgen"),
            InputArg("verbose_count", "-vv", 2),
            InputArg("debug", "-d", True),
            InputArg("silent", "-s", True),
        ]
        args_parsed = parse_args(
            argv=["kppy"] + [t.arg for t in args_test], get_logger=logger
        )

        for arg in args_test:
            self.assertEqual(getattr(args_parsed, arg.name), arg.expect)

        # self.assertEqual(logger.getEffectiveLevel(), logging.ERROR)
