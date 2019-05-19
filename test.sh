#!/bin/bash
echo "Building libraries"
gprclean -r -P test/test.gpr -XLIBRARY_TYPE=relocatable
gprbuild -p -P test/test.gpr -XLIBRARY_TYPE=relocatable

echo "Basename of the library should be the module name"
cp test/lib/libtest.so test/lib/test.so

echo
echo
echo "Run python example"
echo

export PYTHONPATH=$PWD/test/lib
python -c "import test; print test; p = test.Pet(); print p"
