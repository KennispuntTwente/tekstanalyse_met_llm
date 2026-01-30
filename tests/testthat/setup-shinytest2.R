# Load application support files into testing environment
shinytest2::load_app_env()

# Enable async mode for e2e tests - this is the primary way to run the app
# and ensures mirai worker issues are caught by tests
Sys.setenv(KWALLM_TEST_ASYNC = "true")
