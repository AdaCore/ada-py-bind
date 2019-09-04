from __future__ import absolute_import, division, print_function

from e3.testsuite.driver import BasicTestDriver
from e3.testsuite.result import TestStatus
from e3.os.process import Run, PIPE, STDOUT
from e3 import diff
import os.path as P
import e3.fs as fs
from os import environ
import logging
from funcy import keep
import sys


class SetupError(Exception):
    """
    Exception to raise when the testcase is invalid.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


class TestError(Exception):
    """
    Exception to raise when the testcase fails.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


def catch_test_errors(func):
    """
    Helper decorator for driver entry points.

    This returns a wrapper around func that catches SetupError and TestError
    exceptions and that turns them into the appropriate test status. Using
    exceptions is convenient to stop any method from any point: this simplifies
    the control flow.
    """

    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except SetupError as exc:
            self.set_setup_error(exc.message)
        except TestError as exc:
            logging.error(exc.message)
            self.result.set_status(TestStatus.FAIL)
    return wrapper


class BaseDriver(BasicTestDriver):

    DEFAULT_TIMEOUT = 300

    def tear_up(self, prev):
        try:
            timeout = self.test_env['timeout']
        except KeyError:
            timeout = self.DEFAULT_TIMEOUT
        else:
            if (not isinstance(timeout, int)
                    or timeout < 0):
                raise SetupError('Invalid "timeout" entry: expected a positive'
                                 ' number of seconds, got {} instead'.format(
                                     timeout))
        self.timeout = timeout

    def run_and_check(self, argv, for_debug=False, append_output=False,
                      log_errors=True):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        In case of failure, the test output is appended to the actual output
        and a TestError is raised.
        """
        program = argv[0]

        p = Run(argv, cwd=self.working_dir(),
                timeout=self.timeout,
                output=PIPE,
                error=STDOUT)

        if append_output:
            self.result.out += p.out

        if p.status != 0:
            self.result.out += (
                '{} returned status code {}\n'.format(program, p.status)
            )
            self.result.out += p.out
            if log_errors:
                logging.error(p.out)
            raise TestError(
                '{} returned status code {}'.format(program, p.status)
            )

        return p.out

    def working_dir(self, *args):
        """
        Return the working dir, plus any path elements joined to it if passed
        in *args.
        """
        return P.join(self.test_env['working_dir'], *args)

    def test_dir(self, *args):
        """
        Return the test dir, plus any path elements joined to it if passed
        in *args.
        """
        return P.join(self.test_env['test_dir'], *args)


class DefaultDriver(BaseDriver):

    def tear_up(self, prev):
        super(DefaultDriver, self).tear_up(prev)

        # REMARK: Why do I have to mkdir the working dir ??
        fs.mkdir(self.working_dir())
        with open(self.working_dir('gen.gpr'), 'w') as f:
            f.write('''
            with "ada_py_bind";

            library project Gen is
               for Source_Dirs use ("{}");
               for Library_Dir use "test";
               for Create_Missing_Dirs use "True";

               for Library_Name use "gen";
               for Library_Kind use "relocatable";
               for Library_Standalone use "standard";
               for Library_Auto_Init use "true";
               for Library_Interface use ("demo");
               for Object_Dir use "obj";

               package Compiler renames Ada_Py_Bind.Compiler;
               for Leading_Library_Options use Ada_Py_Bind.Py_Bind_Lib_Options;
            end Gen;
            '''.format(P.abspath(self.test_dir())))

        # Copy build_lib.py to working directory
        fs.cp(P.join(P.dirname(P.abspath(__file__)), "build_lib.py"),
              self.working_dir('build_lib.py'))

    @catch_test_errors
    def run(self, prev):
        fs.mkdir(self.working_dir('test'))
        fs.cp(self.test_dir('test.py'), self.working_dir('test', 'test.py'))

        # Try to build, but don't log errors in the build, and recover from
        # TestError: We want to be able to test compilation errors too.
        try:
            self.run_and_check([sys.executable, 'build_lib.py'],
                               log_errors=False)
        except TestError:
            return

        environ['PYTHONPATH'] = P.pathsep.join(
            keep([environ.get('PYTHONPATH'), self.working_dir('test')])
        )

        self.run_and_check(['python', self.working_dir('test', 'test.py')],
                           append_output=True)

    def analyze(self, prev):
        test_diff = diff.diff(self.test_dir('test.out'),
                              self.result.out.log.splitlines())
        if test_diff != '':

            # Rewrite mode: If there is a diff and rewrite mode is on, rewrite
            # the "test.out" file.
            if self.env.options.rewrite:
                with open(self.test_dir('test.out'), 'w') as f:
                    f.write(self.result.out.log)

            # Log the error diff
            logging.error("Diff in test")
            logging.error(test_diff)
            if self.env.options.rewrite:
                logging.info(
                    "Rewritten test '{}'".format(self.test_env['test_name'])
                )
            self.result.set_status(TestStatus.FAIL)
        else:
            self.result.set_status(TestStatus.PASS)

        self.push_result()
