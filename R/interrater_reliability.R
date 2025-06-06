# Module for obtaining inter-rater reliability
# User gets presented a modal dialog to rate a sample of texts
# Module calculates Cohen's Kappa (categories data) or paired t-test (score data)

#### 1 Server ####

# (Note, server is fully responsible for UI rendering, there is no separate UI function)

interrater_server <- function(
  id,
  rating_data = tibble::tibble(
    text = paste("Default Text", 1:10),
    # For single categories:
    # result = sample(c("A", "B"), 10, replace = TRUE)
    # For multiple categories:
    A = sample(c(TRUE, FALSE), 10, replace = TRUE),
    B = sample(c(TRUE, FALSE), 10, replace = TRUE),
    C = sample(c(TRUE, FALSE), 10, replace = TRUE)
  ),
  text_col = "text",
  mode = c("Categorisatie", "Scoren", "Onderwerpextractie"),
  all_categories = c("A", "B", "C"),
  assign_multiple_categories = TRUE,
  rater1_col = "result",
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  mode <- match.arg(mode)

  if (
    mode %in%
      c("Categorisatie", "Onderwerpextractie") &&
      assign_multiple_categories
  ) {
    # If multiple categories are assigned, we need to convert the data
    #   to a long format for rating
    # Each text must have 1 row for each category, containing TRUE/FALSE

    # Check that all categories are present in the data
    missing_categories <- setdiff(all_categories, names(rating_data))
    if (length(missing_categories) > 0) {
      stop(
        "Missing categories in the data: ",
        paste(missing_categories, collapse = ", ")
      )
    }

    rating_data <- rating_data |>
      tidyr::pivot_longer(
        cols = all_categories,
        names_to = "category",
        values_to = rater1_col
      )
  }

  stopifnot(
    is.data.frame(rating_data),
    text_col %in% names(rating_data),
    rater1_col %in% names(rating_data),
    ((mode %in%
      c("Categorisatie", "Onderwerpextractie") &&
      length(unique(all_categories)) > 1) |
      mode == "Scoren")
  )

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # To start/initialize the module from the main server:
      start <- function() {
        showModal(modalDialog(
          title = lang()$t("Inter-rater reliability"),
          size = "l",
          easyClose = FALSE,
          footer = NULL,
          uiOutput(ns("dynamic_content_area"))
        ))
      }
      # Make available to the main server:
      #   start function; done status; result
      return <- reactiveValues(
        start = start,
        done = FALSE,
        result = NULL
      )

      # State can be: "configure_sample", "rating", "finished"
      module_state <- reactiveVal("configure_sample")

      # Stores the subset of data after sampling
      sampled_data_rv <- reactiveVal(NULL)

      # Rating process state
      current_item_index <- reactiveVal(1)
      user_ratings_store <- reactiveVal(list())

      # Render the UI for the absolute count input
      output$abs_count_input_ui_module <- renderUI({
        max_items <- nrow(rating_data)
        default_val <- min(10, max(1, max_items))
        numericInput(
          ns("sample_abs"),
          lang()$t("Aantal:"),
          value = default_val,
          min = 1,
          max = max_items,
          step = 1
        )
      })

      # Calculate number of items to sample (based on input)
      n_to_sample <- reactive({
        req(module_state() == "configure_sample") # Only calculate in config state
        full_data <- rating_data
        req(full_data, nrow(full_data) > 0) # Need data
        total_n <- nrow(full_data)

        min_recommend <- 10
        n <- 0 # Initialize
        sample_type <- input$sample_type
        req(sample_type) # Make sure selection is made

        if (sample_type == "perc") {
          req(input$sample_perc)
          perc <- input$sample_perc / 100
          n <- max(1, ceiling(total_n * perc))
        } else if (sample_type == "abs") {
          req(input$sample_abs) # Ensure absolute input exists
          n <- input$sample_abs
        }

        # Cannot exceed total:
        n <- min(n, total_n)

        return(as.integer(n))
      })

      # Display the number of items to sample
      output$n_to_sample_display_module <- renderText({
        req(module_state() == "configure_sample")
        n <- n_to_sample()
        total_n <- nrow(rating_data)
        req(n, total_n)
        paste(
          n,
          lang()$t("van de"),
          total_n,
          lang()$t("totale items worden willekeurig geselecteerd.")
        )
      })

      # Dynamic UI rendering
      output$dynamic_content_area <- renderUI({
        current_state <- module_state()
        full_data <- rating_data

        # UI for sample configuration
        if (current_state == "configure_sample") {
          total_items_full <- nrow(full_data)

          tagList(
            tags$h4(lang()$t("Steekproef")),
            tags$p(lang()$t("Selecteer hoeveel items je wil beoordelen.")),
            radioButtons(
              ns("sample_type"),
              lang()$t("Trek als:"),
              choices = setNames(
                c("perc", "abs"),
                c("Percentage", lang()$t("Aantal"))
              ),
              selected = isolate(input$sample_type) %||% "perc", # Preserve selection if returning
              inline = TRUE
            ),
            # Conditional Panel for Percentage
            conditionalPanel(
              condition = sprintf("input['%s'] == 'perc'", ns("sample_type")), # Use ns() correctly in condition
              numericInput(
                ns("sample_perc"),
                "Percentage:",
                value = isolate(input$sample_perc) %||% 10,
                min = 1,
                max = 100,
                step = 1
              )
            ),
            # Conditional Panel for Absolute Count
            conditionalPanel(
              condition = sprintf("input['%s'] == 'abs'", ns("sample_type")),
              # Use the dynamic UI output here
              uiOutput(ns("abs_count_input_ui_module")) # Will update based on data
            ),
            tags$small(lang()$t(
              "Aanbevolen: minimaal 10 items of 10% (welke groter is)"
            )),
            tags$hr(),
            tags$p(strong(lang()$t("Aantal items om te beoordelen:"))),
            wellPanel(
              style = "background-color: #f8f9fa;", # Light background
              # Use the dynamic text output here
              textOutput(ns("n_to_sample_display_module"))
            ),
            tags$hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  ns("confirm_sample_start"),
                  "Start",
                  icon = icon("play"),
                  class = "btn-success"
                )
              )
            )
          )
        } else if (current_state == "rating") {
          # UI for rating items
          df_sample <- sampled_data_rv()
          validate(
            need(
              is.data.frame(df_sample) && nrow(df_sample) > 0,
              "Sampled data is missing or empty."
            ),
            need(
              text_col %in% names(df_sample),
              "Text column not found in sampled data."
            ),
            need(
              rater1_col %in% names(df_sample),
              "Rater 1 column not found in sampled data."
            )
          )

          total_items <- nrow(df_sample)
          current_index <- current_item_index()
          stored_ratings <- user_ratings_store()
          item_text <- df_sample[[text_col]][current_index]

          # Get previously selected choice for this item if going back
          selected_choice <- stored_ratings[[as.character(current_index)]] %||%
            character(0)
          submit_label <- ifelse(
            current_index == total_items,
            lang()$t("Afronden"),
            lang()$t("Volgende")
          )
          show_back_button <- current_index > 1

          tagList(
            tags$h4(tags$strong(paste0(
              lang()$t("Beoordeel item "),
              current_index,
              lang()$t(" van "),
              total_items,
              ":"
            ))),
            if (mode == "Scoren") {
              numericInput(
                inputId = ns("current_rating"),
                label = HTML(paste0(
                  "<i>",
                  htmltools::htmlEscape(item_text),
                  lang()$t("</i><br><br><b>Geef een score (1–100):</b>")
                )),
                value = selected_choice %||% NA,
                min = 1,
                max = 100,
                step = 1
              )
            } else {
              if (assign_multiple_categories && mode != "Scoren") {
                # Show binary choice: Is this category applicable?
                current_category <- df_sample[["category"]][current_index]

                radioButtons(
                  inputId = ns("current_rating"),
                  label = HTML(paste0(
                    "<b>Item:</b><br><i>",
                    htmltools::htmlEscape(item_text),
                    lang()$t("</i><br><br><b>Categorie:</b><br><i>"),
                    htmltools::htmlEscape(current_category),
                    lang()$t("</i><br><br>Is deze categorie van toepassing?")
                  )),
                  choices = setNames(
                    c(TRUE, FALSE),
                    c(lang()$t("Ja"), lang()$t("Nee"))
                  ),
                  selected = selected_choice
                )
              } else {
                # Original category selection
                radioButtons(
                  inputId = ns("current_rating"),
                  label = HTML(paste0(
                    "<i>",
                    htmltools::htmlEscape(item_text),
                    "</i>"
                  )),
                  choices = all_categories,
                  selected = selected_choice
                )
              }
            },
            tags$hr(),
            fluidRow(
              column(
                width = 4,
                div(
                  class = "d-flex justify-content-center justify-content-md-start",
                  actionButton(
                    ns("reset_module"),
                    "Reset",
                    icon = icon("undo"),
                    class = "btn-warning"
                  )
                )
              ),
              column(
                width = 4,
                div(
                  class = "d-flex justify-content-center",
                  if (show_back_button) {
                    actionButton(
                      ns("go_back"),
                      lang()$t("Terug"),
                      icon = icon("arrow-left"),
                      class = "btn-default"
                    )
                  }
                )
              ),
              column(
                width = 4,
                div(
                  class = "d-flex justify-content-center justify-content-md-end",
                  actionButton(
                    ns("submit_next"),
                    submit_label,
                    icon = icon("arrow-right"),
                    class = "btn-primary"
                  )
                )
              )
            )
          )
        } else {
          # UI for finished/error state
          tags$p("...")
        }
      })

      # Confirm sample & start rating
      observeEvent(input$confirm_sample_start, {
        req(module_state() == "configure_sample")

        full_data <- rating_data
        n_sample <- n_to_sample()
        total_n <- nrow(full_data)

        if (is.null(n_sample) || n_sample <= 0 || n_sample > total_n) {
          showNotification(
            lang()$t("Ongeldige steekproefgrootte"),
            type = "error"
          )
          return()
        }

        # Perform random sampling
        sampled_indices <- sample(
          seq_len(total_n),
          size = n_sample,
          replace = FALSE
        )
        data_subset <- full_data[sampled_indices, , drop = FALSE]

        # Store sampled data and switch to rating mode
        sampled_data_rv(data_subset)
        user_ratings_store(list())
        current_item_index(1)
        return$result <- NULL
        module_state("rating")
      })

      # Reset button; returns to configuration state
      observeEvent(input$reset_module, {
        req(module_state() == "rating")

        # Reset relevant states
        module_state("configure_sample")
        sampled_data_rv(NULL)
        user_ratings_store(list())
        current_item_index(1)
        return$result <- NULL
        return$done <- FALSE
      })

      # Observer enable/disable submit button
      observe({
        req(module_state() == "rating")
        rating_value <- input$current_rating
        button_id <- "submit_next"

        # Different handling for numeric and character input
        if (mode == "Scoren") {
          if (
            !is.null(rating_value) &&
              !is.na(rating_value) &&
              is.numeric(rating_value)
          ) {
            shinyjs::enable(id = button_id)
          } else {
            shinyjs::disable(id = button_id)
          }
        } else {
          if (!is.null(rating_value) && rating_value != "") {
            shinyjs::enable(id = button_id)
          } else {
            shinyjs::disable(id = button_id)
          }
        }
      })

      # Observe back item button
      observeEvent(input$go_back, {
        req(module_state() == "rating")
        req(current_item_index() > 1)
        current_item_index(current_item_index() - 1)
      })

      # Observe next/submit button; handles also final submission + calculation
      observeEvent(input$submit_next, {
        req(module_state() == "rating")

        df_sample <- sampled_data_rv()
        validate(
          need(
            is.data.frame(df_sample) && nrow(df_sample) > 0,
            "Error: Sampled data is missing or invalid at submission."
          ),
          need(
            text_col %in% names(df_sample),
            paste(
              "Error: Required text column '",
              htmlEscape(text_col),
              "' not found in sampled data."
            )
          ),
          need(
            rater1_col %in% names(df_sample),
            paste(
              "Error: Required rater 1 column '",
              htmlEscape(rater1_col),
              "' not found in sampled data."
            )
          )
        )

        current_index <- current_item_index()
        total_items <- nrow(df_sample)
        user_input <- input$current_rating

        if (mode == "Scoren") {
          if (
            is.null(user_input) ||
              !is.numeric(user_input) ||
              user_input < 1 ||
              user_input > 100
          ) {
            showNotification(
              lang()$t("Voer een geldige score in tussen 1 en 100."),
              type = "warning"
            )
            return()
          }
        } else if (assign_multiple_categories) {
          if (!user_input %in% c("TRUE", "FALSE", TRUE, FALSE)) {
            showNotification(
              lang()$t("Beantwoord met 'Ja' of 'Nee' voor deze categorie."),
              type = "warning"
            )
            return()
          }
          # Convert string input to logical
          user_input <- as.logical(user_input)
        } else {
          if (is.null(user_input) || user_input == "") {
            showNotification(
              lang()$t("Selecteer een categorie."),
              type = "warning"
            )
            return()
          }
        }

        # Store the rating
        current_ratings <- user_ratings_store()
        current_ratings[[as.character(current_index)]] <- user_input
        user_ratings_store(current_ratings)

        # Move to next item OR calculate final result
        if (current_index < total_items) {
          current_item_index(current_index + 1)
        } else {
          # Last item submitted - perform calculation
          module_state("finished")

          final_ratings_list <- user_ratings_store()
          original_ratings <- df_sample[[rater1_col]]

          calculation_result <- tryCatch(
            {
              if (mode == "Scoren") {
                # T.test for score data
                user_scores <- as.numeric(unlist(final_ratings_list[as.character(
                  1:total_items
                )]))
                original_scores <- as.numeric(df_sample[[rater1_col]])

                t_test_result <- t.test(
                  user_scores,
                  original_scores,
                  paired = TRUE
                )

                # Sensitivity analysis for paired t-test
                sensitivity <- pwr::pwr.t.test(
                  n = length(user_scores),
                  power = 0.80,
                  sig.level = 0.05,
                  type = "paired",
                  alternative = "two.sided"
                )

                # Interpret the effect size
                effect_size_d <- sensitivity$d
                effect_size_label <- if (effect_size_d < 0.3) {
                  lang()$t("een klein effect")
                } else if (effect_size_d < 0.7) {
                  lang()$t("een gemiddeld effect")
                } else {
                  lang()$t("een groot effect")
                }

                # Create Dutch summary sentence
                sensitivity_sentence <- sprintf(
                  lang()$t(
                    "Met een steekproefgrootte van %d hadden we bij deze test 80%% power om een effectgrootte van %.2f (Cohen's d) te detecteren, wat overeenkomt met %s volgens de conventies van [Cohen (1988)](https://doi.org/10.4324/9780203771587). Kleinere verschillen tussen het taalmodel en de menselijke beoordelaar zijn moeilijker te detecteren met deze steekproefgrootte."
                  ),
                  length(user_scores),
                  effect_size_d,
                  effect_size_label
                )

                # Combine tidy t-test result with summary stats
                t_test_summary <- t_test_result |> broom::tidy()
                summary_stats <- list(
                  user_mean = mean(user_scores, na.rm = TRUE),
                  user_sd = sd(user_scores, na.rm = TRUE),
                  llm_mean = mean(original_scores, na.rm = TRUE),
                  llm_sd = sd(original_scores, na.rm = TRUE),
                  sensitivity_sentence = sensitivity_sentence
                )

                showNotification(
                  lang()$t("Inter-rater reliability is berekend! (t-test)"),
                  type = "message",
                  duration = 3
                )

                # Combine everything into one list
                result_list <- c(as.list(t_test_summary), summary_stats)
                result_list
              } else {
                # Kappa for categorical data

                # 1. Basic Validation: check rating count
                if (length(final_ratings_list) != total_items) {
                  stop(paste(
                    "Rating count mismatch. Expected",
                    total_items,
                    "got",
                    length(final_ratings_list)
                  ))
                }

                # 2. Assemble user ratings in order
                user_ratings_vector <- unlist(final_ratings_list[as.character(
                  1:total_items
                )])

                # 3. Determine combined levels
                all_levels <- sort(unique(c(
                  as.character(na.omit(original_ratings)),
                  as.character(na.omit(user_ratings_vector))
                )))

                # 4. Calculate Kappa
                # (irr::kappa2 handles NAs internally by default - pair-wise deletion)
                if (assign_multiple_categories) {
                  # Ensure logical vectors
                  original_logical <- as.logical(original_ratings)
                  user_logical <- as.logical(user_ratings_vector)

                  # Create dataframe with both raters
                  ratings_combined <- data.frame(
                    original = original_logical,
                    user = user_logical
                  )

                  # Apply Cohen's Kappa to TRUE/FALSE labels
                  kappa_output <- irr::kappa2(ratings_combined)
                } else {
                  # Original categorical mode (single-label)
                  ratings_combined <- data.frame(
                    original = factor(original_ratings, levels = all_levels),
                    user = factor(user_ratings_vector, levels = all_levels)
                  )

                  kappa_output <- irr::kappa2(ratings_combined)
                }

                # 5. Show message
                showNotification(
                  lang()$t("Inter-rater reliability is berekend! (Kappa)"),
                  type = "message",
                  duration = 3
                )

                # Make suitable object type
                class(kappa_output) <- "list"

                # Return
                kappa_output
              }
            },
            error = function(e) {
              # If any step above failed, catch the error here
              # Return a standardized error structure
              app_error(
                e,
                when = "during Kappa calculation",
                fatal = FALSE,
                lang = lang()
              )
              NULL
            }
          )

          return$result <- calculation_result
          return$done <- TRUE
          removeModal()
        }
      })

      return(return)
    }
  )
}


#### 2 Helper functions ####

# Helper to format the interrater result
#   (note: only used in the testing app for debugging)
format_kappa_result <- function(result) {
  # ... (keep the existing function as is) ...
  if (is.null(result)) {
    return("No result available.")
  }
  if (inherits(result, "error")) {
    # Check if it's an error from the sampling stage (custom structure)
    if (!is.null(result$message) && grepl("^Sampling Error:", result$message)) {
      return(result$message)
    }
    # Check if it's a specific Kappa error we added
    if (
      !is.null(result$message) &&
        grepl("^Cannot calculate Kappa:", result$message)
    ) {
      return(result$message)
    }
    # Default error formatting
    return(paste("Error during calculation:", result$message))
  }

  # Basic check if it looks like a kappa2 result
  if (
    !all(
      c("method", "subjects", "raters", "value", "statistic", "p.value") %in%
        names(result)
    )
  ) {
    # Handle potential sampling error passed through
    if (!is.null(result$message)) return(paste("Error:", result$message))
    return("Result object does not contain expected Kappa information.")
  }

  output_string <- paste0(
    "Cohen's Kappa Calculation\n",
    "-------------------------\n",
    "Subjects (Sampled) = ",
    result$subjects,
    "\n", # Clarify it's sampled
    "Raters = ",
    result$raters,
    "\n",
    "Kappa = ",
    round(result$value, 3),
    "\n",
    "Z = ",
    round(result$statistic, 3),
    "\n",
    "p-value = ",
    format.pval(result$p.value, digits = 3),
    "\n\n",
    "Interpretation (example based on Landis & Koch, 1977):\n"
  )

  kappa_val <- result$value
  interpretation <- dplyr::case_when(
    kappa_val < 0.00 ~ "Poor",
    kappa_val < 0.20 ~ "Slight",
    kappa_val < 0.40 ~ "Fair",
    kappa_val < 0.60 ~ "Moderate",
    kappa_val < 0.80 ~ "Substantial",
    kappa_val <= 1.00 ~ "Almost Perfect",
    TRUE ~ "N/A"
  )
  output_string <- paste0(output_string, "Agreement: ", interpretation, "\n")
  return(output_string)
}


#### 3 Example/development usage ####

if (FALSE) {
  ui <- fluidPage(
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        actionButton(
          "start_rating_btn",
          "Start Rating Task",
          class = "btn-success",
          icon = icon("play")
        )
      ),
      mainPanel(
        tags$h4("Rating Result:"),
        verbatimTextOutput("final_result_display")
      )
    )
  )

  server <- function(input, output, session) {
    full_sample_data <- tibble::tibble(
      text = paste("Full Dataset Item", 1:75),
      # For testing categorical data:
      # result = sample(
      #   c("Good", "Bad", "Neutral", "Very Good"),
      #   75,
      #   replace = TRUE,
      #   prob = c(0.4, 0.3, 0.2, 0.1)
      # )
      # For testing score data:
      result = sample(
        c(1:100),
        75,
        replace = TRUE
      )
    )

    # Reactive value to store the final Kappa result returned by the module
    kappa_result_from_module <- reactiveVal(NULL)

    # Launch modal dialog for inter-rater reliability
    observeEvent(input$start_rating_btn, {
      kappa_result_from_module(NULL)
      output$final_result_display <- renderText({
        "Waiting for rating task completion..."
      })

      # Pass the FULL dataset reactive
      irr <- interrater_server(
        id = "rater_modal",
        # rating_data = full_sample_data,
        # text_col = "text",
        # all_categories = c("Good", "Bad", "Neutral", "Very Good", "idk"),
        # mode = "Categorisatie",
        mode = "Categorisatie",
        assign_multiple_categories = TRUE
      )
      irr$start()

      # Watches the reactive value RETURNED by the module server
      completion_observer <- observe(
        {
          result <- irr$result # Get the current value of the returned reactive

          # Check if the result is no longer NULL (meaning the module finished or errored)
          if (irr$done) {
            kappa_result_from_module(result)
            output$final_result_display <- renderPrint({
              format_kappa_result(kappa_result_from_module())
            })
          }
        },
        suspended = FALSE,
        autoDestroy = TRUE
      )
    })

    # Initial state
    output$final_result_display <- renderText({
      "Click 'Start Rating Task' to begin."
    })
  }

  shinyApp(ui, server)
}
