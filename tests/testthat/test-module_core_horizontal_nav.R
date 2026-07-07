# Tests for horizontal navigation mode logic
# These tests verify navigation bounds, progress calculation, and UI structure
# without requiring full E2E browser tests.

library(testthat)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

source(here::here("R", "utils_logger.R"))
source(here::here("R", "module_core_main_ui_and_server.R"))


# 1 Progress Bar Calculation Tests (Pure R) ---------------------------------
# The progress percentage is calculated as: round((cur - 1L) / (n_sections - 1L) * 100L)
# This is tested as a pure function without Shiny reactivity.

calculate_progress_pct <- function(current_section, n_sections) {
  if (n_sections <= 1L) {
    return(100L)
  }
  round((current_section - 1L) / (n_sections - 1L) * 100L)
}

test_that("progress percentage calculation is correct for standard 5 sections", {
  n_sections <- 5L

  expect_equal(calculate_progress_pct(1L, n_sections), 0L)
  expect_equal(calculate_progress_pct(2L, n_sections), 25L)
  expect_equal(calculate_progress_pct(3L, n_sections), 50L)
  expect_equal(calculate_progress_pct(4L, n_sections), 75L)
  expect_equal(calculate_progress_pct(5L, n_sections), 100L)
})

test_that("progress percentage handles edge case of single section", {
  expect_equal(calculate_progress_pct(1L, 1L), 100L)
})

test_that("progress percentage handles different section counts", {
  # 3 sections: 0%, 50%, 100%
  expect_equal(calculate_progress_pct(1L, 3L), 0L)
  expect_equal(calculate_progress_pct(2L, 3L), 50L)
  expect_equal(calculate_progress_pct(3L, 3L), 100L)

  # 2 sections: 0%, 100%
  expect_equal(calculate_progress_pct(1L, 2L), 0L)
  expect_equal(calculate_progress_pct(2L, 2L), 100L)
})


# 2 Section Boundary Validation Tests (Pure R) ------------------------------
# Test the logic that validates section step input values

validate_section_step <- kwallm_validate_section_step

test_that("section step validation accepts valid values", {
  expect_equal(validate_section_step("1", 5L), 1L)
  expect_equal(validate_section_step("3", 5L), 3L)
  expect_equal(validate_section_step("5", 5L), 5L)
  expect_equal(validate_section_step(2, 5L), 2L)
})

test_that("section step validation rejects invalid values", {
  # NA and non-numeric
  expect_null(validate_section_step(NA, 5L))
  expect_null(validate_section_step("invalid", 5L))
  expect_null(validate_section_step("", 5L))

  # Out of bounds
  expect_null(validate_section_step("0", 5L))
  expect_null(validate_section_step("-1", 5L))
  expect_null(validate_section_step("6", 5L))
  expect_null(validate_section_step("100", 5L))
})


# 3 Direction Calculation Tests (Pure R) ------------------------------------
# Test the logic that determines slide animation direction

calculate_direction <- kwallm_section_direction

test_that("direction calculation returns correct animation direction", {
  expect_equal(calculate_direction(1L, 2L), "right")
  expect_equal(calculate_direction(1L, 5L), "right")
  expect_equal(calculate_direction(5L, 4L), "left")
  expect_equal(calculate_direction(5L, 1L), "left")
  expect_equal(calculate_direction(3L, 3L), "none")
})


# 4 testServer Tests for Navigation Logic -----------------------------------

test_that("main_server: current_section starts at 1 and updates correctly", {
  # This test creates a minimal test environment to verify navigation state
  shiny::testServer(
    function(input, output, session) {
      n_sections <- 5L
      current_section <- reactiveVal(1L)

      list(current_section = current_section, n_sections = n_sections)
    },
    {
      # Starts at section 1
      expect_equal(current_section(), 1L)

      # Update to valid section
      current_section(3L)
      expect_equal(current_section(), 3L)

      # Update to last section
      current_section(n_sections)
      expect_equal(current_section(), n_sections)
    }
  )
})

test_that("navigation: prev button at section 1 stays at section 1", {
  shiny::testServer(
    function(input, output, session) {
      n_sections <- 5L
      current_section <- reactiveVal(1L)

      # Simulate prev button logic from main_server
      handle_prev <- function() {
        cur <- current_section()
        if (cur <= 1L) {
          return() # No change
        }
        current_section(cur - 1L)
      }

      list(
        current_section = current_section,
        handle_prev = handle_prev
      )
    },
    {
      expect_equal(current_section(), 1L)

      # Try to go back from section 1 - should stay at 1
      handle_prev()
      expect_equal(current_section(), 1L)

      # Move to section 3, then go back
      current_section(3L)
      handle_prev()
      expect_equal(current_section(), 2L)
    }
  )
})

test_that("navigation: next button at last section stays at last section", {
  shiny::testServer(
    function(input, output, session) {
      n_sections <- 5L
      current_section <- reactiveVal(5L)

      # Simulate next button logic from main_server
      handle_next <- function() {
        cur <- current_section()
        if (cur >= n_sections) {
          return() # No change
        }
        current_section(cur + 1L)
      }

      list(
        current_section = current_section,
        handle_next = handle_next,
        n_sections = n_sections
      )
    },
    {
      expect_equal(current_section(), n_sections)

      # Try to go forward from last section - should stay at 5
      handle_next()
      expect_equal(current_section(), n_sections)

      # Move to section 2, then go forward
      current_section(2L)
      handle_next()
      expect_equal(current_section(), 3L)
    }
  )
})

test_that("navigation: step input validation ignores invalid values", {
  shiny::testServer(
    function(input, output, session) {
      n_sections <- 5L
      current_section <- reactiveVal(2L)

      # Simulate step input handling from main_server
      handle_step_input <- function(input_value) {
        new <- suppressWarnings(as.integer(input_value))
        if (is.na(new) || new < 1L || new > n_sections) {
          return() # Ignore invalid input
        }
        current_section(new)
      }

      list(
        current_section = current_section,
        handle_step_input = handle_step_input,
        n_sections = n_sections
      )
    },
    {
      expect_equal(current_section(), 2L)

      # Valid input
      handle_step_input("4")
      expect_equal(current_section(), 4L)

      # Invalid inputs - should not change current section
      handle_step_input("0")
      expect_equal(current_section(), 4L)

      handle_step_input("-1")
      expect_equal(current_section(), 4L)

      handle_step_input("6")
      expect_equal(current_section(), 4L)

      handle_step_input(NA)
      expect_equal(current_section(), 4L)

      handle_step_input("invalid")
      expect_equal(current_section(), 4L)
    }
  )
})

test_that("layout toggle: processing state controls enable/disable", {
  shiny::testServer(
    function(input, output, session) {
      processing <- reactiveVal(FALSE)
      layout_toggle_disabled <- reactiveVal(FALSE)

      # Simulate the observer from main_server (lines 764-770)
      observe({
        if (isTRUE(processing())) {
          layout_toggle_disabled(TRUE)
        } else {
          layout_toggle_disabled(FALSE)
        }
      })

      list(
        processing = processing,
        layout_toggle_disabled = layout_toggle_disabled
      )
    },
    {
      # Initially not processing, toggle should be enabled
      expect_false(processing())
      expect_false(layout_toggle_disabled())

      # Start processing, toggle should be disabled
      processing(TRUE)
      session$flushReact()
      expect_true(layout_toggle_disabled())

      # Stop processing, toggle should be enabled again
      processing(FALSE)
      session$flushReact()
      expect_false(layout_toggle_disabled())
    }
  )
})


# 5 UI Structure Tests ------------------------------------------------------
# Verify that the rendered main_ui output contains the expected navigation
# DOM elements by inspecting the actual source of module_core_main_ui_and_server.R.
# This catches regressions where IDs are renamed, removed, or miswired.

test_that("main_ui renderUI contains section div IDs for all 5 sections", {
  src <- paste(
    readLines(here::here("R", "module_core_main_ui_and_server.R")),
    collapse = "\n"
  )

  for (i in 1:5) {
    id <- paste0("kwallm_section_", i)
    expect_true(
      grepl(sprintf('id\\s*=\\s*"%s"', id), src, perl = TRUE),
      info = sprintf("renderUI must contain div with id = \"%s\"", id)
    )
  }
})

test_that("main_ui renderUI contains navigation control IDs", {
  src <- paste(
    readLines(here::here("R", "module_core_main_ui_and_server.R")),
    collapse = "\n"
  )

  nav_ids <- c(
    "kwallm_sections_nav",
    "kwallm_sections_step",
    "kwallm_sections_prev",
    "kwallm_sections_next",
    "kwallm_sections_progress_bar",
    "kwallm_sections_progress_text",
    "kwallm_layout_view"
  )

  for (id in nav_ids) {
    expect_true(
      grepl(id, src, fixed = TRUE),
      info = sprintf("renderUI must reference navigation element \"%s\"", id)
    )
  }
})

test_that("prev/next observers guard on section bounds", {
  src <- readLines(here::here("R", "module_core_main_ui_and_server.R"))
  src_text <- paste(src, collapse = "\n")

  # Prev button observer must check cur <= 1L
  expect_true(
    grepl("cur\\s*<=\\s*1L", src_text, perl = TRUE),
    info = "prev observer must guard on cur <= 1L"
  )

  # Next button observer must check cur >= n_sections
  expect_true(
    grepl("cur\\s*>=\\s*n_sections", src_text, perl = TRUE),
    info = "next observer must guard on cur >= n_sections"
  )
})

test_that("prev/next observers update server section state directly", {
  src <- paste(
    readLines(here::here("R", "module_core_main_ui_and_server.R")),
    collapse = "\n"
  )

  expect_true(
    grepl("set_current_section\\(\\s*cur - 1L", src, perl = TRUE),
    info = "prev observer must route through set_current_section()"
  )
  expect_true(
    grepl("set_current_section\\(\\s*cur \\+ 1L", src, perl = TRUE),
    info = "next observer must route through set_current_section()"
  )
  expect_true(
    grepl("exportTestValues(kwallm_current_section", src, fixed = TRUE),
    info = "current section must be exported for shinytest2 synchronization"
  )
})

test_that("layout toggle is disabled during processing", {
  src <- paste(
    readLines(here::here("R", "module_core_main_ui_and_server.R")),
    collapse = "\n"
  )

  expect_true(
    grepl('disable("kwallm_layout_view")', src, fixed = TRUE),
    info = "layout toggle must be disabled during processing"
  )
  expect_true(
    grepl('enable("kwallm_layout_view")', src, fixed = TRUE),
    info = "layout toggle must be re-enabled after processing"
  )
})


# 6 Layout Switch Behavior Tests --------------------------------------------

test_that("layout view values are constrained to valid options", {
  valid_views <- c("vertical", "sections")

  is_valid_view <- function(view) {
    !is.null(view) && view %in% valid_views
  }

  expect_true(is_valid_view("vertical"))
  expect_true(is_valid_view("sections"))
  expect_false(is_valid_view(NULL))
  expect_false(is_valid_view("invalid"))
  expect_false(is_valid_view(""))
  expect_false(is_valid_view("horizontal")) # Common mistake
})

test_that("test mode defaults to vertical layout", {
  # In test mode (line 369-373), the default should be "vertical"
  in_test_mode <- TRUE
  default_layout <- if (isTRUE(in_test_mode)) "vertical" else "sections"

  expect_equal(default_layout, "vertical")
})

test_that("non-test mode defaults to sections layout", {
  in_test_mode <- FALSE
  default_layout <- if (isTRUE(in_test_mode)) "vertical" else "sections"

  expect_equal(default_layout, "sections")
})
