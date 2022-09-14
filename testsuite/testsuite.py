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
            with "{0}/admpfr.gpr";

            project Test is

            for Source_Dirs use ("{1}");
            for Main use ("test.adb");
            for Object_Dir use "obj";

            package Compiler is
               for Switches ("Ada") use ("-gnat2022");
            end Compiler;

            end Test;
            '''.format(
                self.test_dir('..', '..', '..'),
                self.test_dir())
            )

        self.shell(["gprbuild", "-p"], analyze_output=False)

    def run(self):
        self.shell(["./obj/test"])


class AdmpfrTestsuite(Testsuite):
    """Testsuite for the mpfr Ada bindings."""
    test_driver_map = {"admpfr": AdmpfrDriver}
    default_driver = "admpfr"

    def add_options(self, parser):
        parser.add_argument(
            "--rewrite", action="store_true",
            help="Rewrite test baselines according to current outputs"
        )

    def set_up(self):
        super(AdmpfrTestsuite, self).set_up()
        self.env.rewrite_baselines = self.main.args.rewrite


if __name__ == "__main__":
    sys.exit(AdmpfrTestsuite().testsuite_main())
