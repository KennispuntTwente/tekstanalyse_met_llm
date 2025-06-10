# Define CSS & JS to include in the head of the Shiny app;
#   applies some styling to make the app look better

#### 1 Function ####

css_js_head <- function() {
  tags$head(
    tags$style(HTML(
      "
      .well {
        background-color: #f8f9fa; /* Light gray background */
        border: 1px solid #dee2e6; /* Light gray border */
        padding: 20px;
        border-radius: 0.25rem;
        margin-bottom: 20px;
      }
      .well h4 {
        color: #495057; /* Dark gray heading text */
        font-weight: bold;
        margin-top: 0;
        margin-bottom: 10px;
      }
      .well p {
        color: #6c757d; /* Medium gray text */
        font-size: 0.9rem;
        margin-bottom: 1rem;
      }
      .progress {
        background-color: #e9ecef; /* Light background for progress bar */
        border-radius: 0.25rem;
        overflow: hidden;
        margin-bottom: 1rem;
      }
      .progress-bar {
        background-color: #007bff; /* Bootstrap primary color */
        color: white;
        text-align: center;
        white-space: nowrap;
        overflow: hidden;
        border-radius: 0.25rem;
      }
      #progress_text {
        color: #6c757d; /* Medium gray progress text */
        font-size: 0.9rem;
        text-align: center;
        margin-bottom: 1rem;
      }
      .btn-info {
        color: #fff;
        background-color: #17a2b8;
        border-color: #17a2b8;
      }
      .btn-info:hover {
        background-color: #138496;
        border-color: #117a8b;
      }
      .btn-success {
        color: #fff;
        background-color: #28a745;
        border-color: #28a745;
      }
      .btn-success:hover {
        background-color: #218838;
        border-color: #1e7e34;
      }
      .btn-danger {
        color: #fff;
        background-color: #dc3545;
        border-color: #dc3545;
      }
      .btn-danger:hover {
        background-color: #c82333;
        border-color: #bd2130;
      }
      .btn-primary {
        color: #fff;
        background-color: #007bff;
        border-color: #007bff;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
      .text-center {
        text-align: center;
      }
      hr {
        margin-top: 1rem;
        margin-bottom: 1rem;
        border: 0;
        border-top: 1px solid rgba(0, 0, 0, 0.1);
      }
      .card-container {
        display: flex;
        flex-direction: column;
        align-items: stretch;
        width: 100%;
        max-width: 1000px;
        margin: 0 auto;
      }
      .card {
        width: 100%;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
      }
      .category-button-container {
        display: flex;
        justify-content: center;
        gap: 10px; /* Space between buttons */
        margin-bottom: 10px;
      }
      .category-button {
        width: auto;
        padding: 5px 10px;
        font-size: 0.9rem;
      }
      .progress-bar {
        transition: width .6s ease;
      }
      .card {
        position: relative;
        overflow: visible !important;
        z-index: 0;
      }
      .card:hover {
        box-shadow: 0 8px 20px rgba(0,0,0,0.1);
        z-index: 9;
      }
      .selectize-dropdown,
      .dropdown-menu {
        position: absolute;
        z-index: 9999 !important;
      }
      .snake-btn {
        position: relative;
        overflow: hidden;
      }
      .snake-btn.loading::before {
        content: '';
        position: absolute;
        top: 0; left: 0;
        width: 100%; height: 100%;
        box-sizing: border-box; /* Include border in size */
        border: 2px solid #fff; /* The border that gets revealed */

        /* Define the animation */
        animation: snake-border 3s linear infinite;

        /* Set the starting clip-path state explicitly */
        clip-path: inset(0 100% 0 0);
      }
      /* Keyframes for continuous clockwise border animation */
      @keyframes snake-border {
        /* Start State: Left edge visible */
        0%   { clip-path: inset(0 100% 0 0); }

        /* State after Top edge is drawn (L->R) */
        25%  { clip-path: inset(0 0 100% 0); }

        /* State after Right edge is drawn (T->B) */
        50%  { clip-path: inset(0 0 0 100%); }

        /* State after Bottom edge is drawn (R->L) */
        75%  { clip-path: inset(100% 0 0 0); }

        /* State after Left edge is drawn (B->T) - back to start */
        100% { clip-path: inset(0 100% 0 0); }
      }
      .btn {
        border: 1px solid rgba(0, 0, 0, 0.2);
        transition: all 0.2s ease-in-out; /* smooth effect */
      }
      .btn:hover {
        border: 1px solid rgba(0, 0, 0, 0.4); /* slightly darker border on hover */
        box-shadow: 0 0 5px rgba(0, 0, 0, 0.1); /* very light shadow on hover */
      }
      .fade-in {
        opacity: 0;
        animation: fadeIn 0.6s forwards;
      }
      @keyframes fadeIn {
        to { opacity: 1; }
      }
      .pulse {
        animation: subtlePulse 2s ease-in-out infinite;
        transform-origin: 50% 50%;
      }
      @keyframes subtlePulse {
        0%   { transform: scale(1);   }
        50%  { transform: scale(1.06);}
        100% { transform: scale(1);   }
      }
      .title-break {
        display: inline;
      }

      @media (max-width: 600px) {
        .title-break {
          display: block;
        }
      }
      .card {
        position: relative !important;
        z-index: auto !important;
        transform: none !important;
        filter: none !important;
        opacity: 1 !important;
        will-change: auto !important;
      }
      "
    )),
    tags$style(HTML(
      "
      /* Allow container to display dropdown */
      .selector-container {
        position: relative;
        overflow: visible !important;
      }

      /* Let Selectize control and dropdown overflow as needed */
      .selectize-control {
        overflow: visible !important;
      }
      .selectize-dropdown {
        z-index: 9999 !important;
        overflow: visible !important;
      }
      .selectize-container {
        position: relative;
        z-index: 9999;
      }
      /* Ensure card and its body also allow overflow */
      .card {
        overflow: visible !important;
      }
      .card-body, .row, .col-md-6, .form-group.shiny-input-container {
        overflow: visible !important;
      }
    "
    )),
    tags$script(HTML(
      "
      $(document).on('click', 'a.action-button', function(e) {
        e.preventDefault();
      });
    "
    ))
  )
}

#### 2 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)

  ui <- fluidPage(
    useShinyjs(),
    css_js_head(),
    h1("Test CSS and JS Head"),
    p("This is a test page to check the CSS and JS included in the head."),
    actionButton("test_button", "Test Button"),
    tags$div(
      class = "well",
      h4("Well Styled Section"),
      p("This section should have a light gray background and a border.")
    ),
    # Show card with progress bar
    tags$div(
      class = "card-container",
      tags$div(
        class = "card",
        tags$div(
          class = "progress mb-2",
          tags$div(
            id = "test_bar",
            class = "progress-bar",
            role = "progressbar",
            style = "width: 50%;",
            `aria-valuenow` = "50",
            `aria-valuemin` = "0",
            `aria-valuemax` = "100"
          )
        ),
        tags$div(
          id = "test_text",
          "Progress Text",
          class = "text-center mb-3"
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Server logic can go here
  }

  shinyApp(ui, server)
}
