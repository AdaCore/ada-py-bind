#!/usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Libadalang testsuite.
"""

from __future__ import absolute_import, division, print_function


import os

from distutils.spawn import find_executable
from e3.testsuite import Testsuite

from drivers import DefaultDriver


class AdaPyBindTestsuite(Testsuite):
    DRIVERS = {'default': DefaultDriver}
    TEST_SUBDIR = 'tests'

    def tear_up(self):
        pass

    @property
    def default_driver(self):
        return 'default'

if __name__ == '__main__':
    AdaPyBindTestsuite(os.path.dirname(__file__)).testsuite_main()
