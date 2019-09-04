"""
Script meant to be copied in test directories, to automate the part of building
the library and copying the resulting .so file to a name that python can find.
"""

import os.path as P
from subprocess import Popen, PIPE
import sys
import re

import e3.fs as fs

# Build the library
p = Popen([
    'gprbuild', '-XLIBRARY_TYPE=relocatable', '-XBUILD_MODE=dev', '-Pgen',
], stdout=PIPE, stderr=PIPE)
output, error = p.communicate()

if p.returncode != 0:
    # Build failed

    # Remove some patterns from the error to make it resilient to changes in
    # the library.
    error = re.sub("^.*instantiation error at .*", "", error)

    # Print the errors
    print error

    # Exit
    sys.exit(1)
else:
    # Rename libgen so that it's named according to python native modules
    # conventions.
    fs.mv(P.join('test', 'libgen.so'), P.join('test', 'gen.so'))
