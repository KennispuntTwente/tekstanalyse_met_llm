# Load application support files into testing environment
shinytest2::load_app_env()

# Enable async mode for e2e tests - this is the primary way to run the app
# and ensures mirai worker issues are caught by tests
Sys.setenv(KWALLM_TEST_ASYNC = "true")

# Enable prompt tracing during e2e tests - this ensures log helper functions
# are exercised in async contexts (catches issues like helpers not being
# resolvable in mirai workers)
Sys.setenv(KWALLM_LOG_PROMPTS_TO_FILE = "true")
