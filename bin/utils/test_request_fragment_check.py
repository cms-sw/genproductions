"""
This test checks that McM SDK works
properly when it is being used with CMS GEN's
`request_fragment_check.py`
"""
import unittest
import sys
import os
import logging
import subprocess
import re


EMPTY_SPACE = " "
NUMBER_OF_ERRORS_PATTERN = r'Number of errors = ([0-9]{1,3})'
NUMBER_OF_ERRORS = re.compile(NUMBER_OF_ERRORS_PATTERN)


class McMFragmentCheckTest(unittest.TestCase):
    """
    Check that McM SDK integrates/works properly with
    `request_fragment_check.py` module
    """

    def __shell_execution(self, command):
        """
        Executes the desired command as a subprocess
        and retrieves its output and exit code.

        Args:
            command (str): Command to execute in a subprocess
        Returns:
            tuple[int, str]: Status code and standard output
        """
        # INFO: Do not use `stdout=subprocess.PIPE` if you
        # run the script setting `McM(id=McM.OIDC, ...)` for the 
        # McM REST client. That authentication schema requires
        # human intervention to complete the auth flow. Therefore,
        # the subprocess will be blocked and you can not interact 
        # with it. If you require to run it with this schema for debugging
        # something, just comment that line.
        command_args = command.strip().split(EMPTY_SPACE)
        completed_process = subprocess.Popen(
            args=command_args,
            # Comment the following line if you require it.
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            universal_newlines=True,
        )
        stdout, _ = completed_process.communicate()
        exit_code = completed_process.returncode
        return (exit_code, stdout)

    def build(self):
        """
        Prepare the test case.
        Retrieve the script path, set the McM request to validate
        and set up the logger.
        """
        # Set up logging
        logging.basicConfig(
            format="[%(module)s][%(asctime)s][%(levelname)s] %(message)s",
            level=logging.INFO,
        )
        self.logger = logging.getLogger()
        self.mcm_prepid = "PPD-TestForGENScriptDoNotOperateThis-00001"

        # Retrieve script path
        script_name = "request_fragment_check.py"
        path_from_utils = os.path.join(os.getcwd(), script_name)
        path_from_repo_root = os.path.join(os.getcwd(), "bin", "utils", script_name)
        if os.path.isfile(path_from_utils):
            self.script_path = path_from_utils
        elif os.path.isfile(path_from_repo_root):
            self.script_path = path_from_repo_root
        else:
            raise FileNotFoundError(
                (
                    "CMS GEN fragment check script is not available at: %s nor %s"
                    % (path_from_utils, path_from_repo_root)
                )
            )

        # Log the test configuration
        self.logger.info("Using Python version: %s", sys.version)
        self.logger.info("Using McM request %s for perfoming the test", self.mcm_prepid)
        self.logger.info(
            "CMS GEN fragment check script is available at: %s", self.script_path
        )

    def setUp(self):
        """
        Set up the test case
        """
        self.build()

    def test_integration(self):
        """
        Checks that `request_fragment_check.py` script
        works properly with the McM SDK and the integration
        for McM validation process
        """
        test_command = "%s --bypass_status --prepid %s --dev --develop" % (self.script_path, self.mcm_prepid)
        exit_code, stdout = self.__shell_execution(test_command)
        self.logger.info('Script output:\n%s', stdout)
        number_of_errors = NUMBER_OF_ERRORS.findall(stdout)
        self.assertEqual(
            0, 
            exit_code, 
            'The related McM request should not have validation errors'
        )
        self.assertIsNotNone(
            stdout,
            msg=(
                'Please make sure you capture the standard output \n'
                '(have enabled `stdout=subprocess.PIPE` inside self.__shell_execution(...) function) \n'
                'Script output is None.'
            )
        )
        self.assertTrue(number_of_errors, msg='There should be a line in the output stating the number of errors')
        number_of_errors = int(number_of_errors[0])
        self.assertEqual(
            0,
            number_of_errors,
            msg='Number of errors should be zero.'
        )


if __name__ == "__main__":
    unittest.main()
