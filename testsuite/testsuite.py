#!/usr/bin/env python3

import sys

from e3.testsuite import Testsuite
from e3.testsuite.driver.diff import DiffTestDriver


class AdmpfrDriver(DiffTestDriver):
    """
    Driver to run admpfr stuff and check its output.
    """

    def set_up(self):
        super().set_up()

        with open(self.working_dir('test.gpr'), 'w') as f:
            f.write('''
            project Test is

            for Languages use ("ada", "c");

            for Source_Dirs use ("{0}", "{1}");
            for Main use ("test.adb");
            for Object_Dir use "obj";

            package Linker is
                for Default_Switches ("ada") use ("-lmpfr");
            end Linker;

            end Test;
            '''.format(
                self.test_dir('..', '..', '..', 'src'),
                self.test_dir())
            )

        self.shell(["gprbuild", "-p"], analyze_output=False)

    def run(self):
        self.shell(["./obj/test"])


class AdmpfrTestsuite(Testsuite):
    """Testsuite for the mpfr Ada bindings."""
    test_driver_map = {"admpfr": AdmpfrDriver}
    default_driver = "admpfr"


if __name__ == "__main__":
    sys.exit(AdmpfrTestsuite().testsuite_main())