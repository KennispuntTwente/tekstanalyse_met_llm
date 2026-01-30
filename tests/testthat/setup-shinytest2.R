# Load application support files into testing environment
shinytest2::load_app_env()

# Note: Environment variables set here with Sys.setenv() are NOT inherited by
# the Shiny app subprocess that shinytest2 spawns (because callr, which
# shinytest2 uses internally, doesn't inherit env vars from the parent R
# process). To pass configuration to the app subprocess, use the `options`
# parameter in AppDriver$new(), which IS passed to the subprocess.
#
# The env vars below are kept for non-shinytest2 contexts (e.g., manual testing
# or CI jobs that set these at the shell level before running tests).
Sys.setenv(KWALLM_TEST_ASYNC = "true")
Sys.setenv(KWALLM_LOG_PROMPTS_TO_FILE = "true")
