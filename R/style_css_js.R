# Define CSS & JS to include in the head of the Shiny app;
#   applies some styling to make the app look better

# 1 Function ---------------------------------------------------------
css_js_head <- function() {
  tags$head(
    # Bootstrap Icons CDN
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
    ),
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
        transition: width 0.3s ease-out;
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

      /* --- KWALLM: sectioned (horizontal) layout ------------------------- */
      .kwallm-sections-nav {
        position: sticky;
        top: 0.5rem;
        z-index: 20;
        padding: 0.75rem;
        margin-bottom: 1rem;
        border: 1px solid #dee2e6;
        background-color: rgba(248, 249, 250, 0.96);
        backdrop-filter: blur(6px);
        border-radius: 0.5rem;
        box-shadow: 0 1px 8px rgba(0, 0, 0, 0.06);
        opacity: 1;
        transform: translateY(0);
        transition: opacity 0.2s ease-out, transform 0.2s ease-out;
      }

      .kwallm-sections-nav.kwallm-nav-hidden {
        opacity: 0;
        transform: translateY(-10px);
        pointer-events: none;
      }

      .kwallm-sections-nav .btn-group {
        display: flex;
        flex-wrap: wrap;
        width: 100%;
        justify-content: center;
        gap: 0.35rem;
      }

      .kwallm-sections-nav .btn-group > .btn {
        border-radius: 999px !important;
        transition: all 0.15s ease-out;
      }

      .kwallm-sections-nav .btn-group > .btn.active {
        box-shadow: 0 2px 8px rgba(0, 123, 255, 0.35);
        transform: scale(1.02);
      }

      .kwallm-sections-progress .progress {
        height: 0.35rem;
        margin-bottom: 0;
      }

      .kwallm-sections-progress .progress-bar {
        transition: width 0.35s ease-out;
      }

      @keyframes kwallm-slide-in-right {
        from { opacity: 0; transform: translateX(18px); }
        to   { opacity: 1; transform: translateX(0); }
      }

      @keyframes kwallm-slide-in-left {
        from { opacity: 0; transform: translateX(-18px); }
        to   { opacity: 1; transform: translateX(0); }
      }

      .kwallm-slide-in-right {
        animation: kwallm-slide-in-right 220ms ease-out both;
      }

      .kwallm-slide-in-left {
        animation: kwallm-slide-in-left 220ms ease-out both;
      }

      /* Sticky footer: ensure footer stays at bottom of viewport */
      .kwallm-page-wrapper {
        display: flex;
        flex-direction: column;
        min-height: 100vh;
      }

      .kwallm-main-content {
        flex: 1 0 auto;
      }

      .kwallm-footer {
        flex-shrink: 0;
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

      // KWALLM: Blur any button in a btn-group after click so keyboard nav works for sections
      $(document).on('click', '.btn-group .btn, .btn-group-container .btn', function() {
        document.activeElement.blur();
      });

      // KWALLM: Keyboard navigation for sections (← / →)
      $(document).on('keydown', function(e) {
        // Only handle arrow keys
        if (e.key !== 'ArrowLeft' && e.key !== 'ArrowRight') return;

        // Only handle if in sections mode
        var layoutView = $('input[name=kwallm_layout_view]:checked').val();
        if (layoutView !== 'sections') return;

        // Don't trigger if user is typing in an input/textarea/select
        var tag = e.target.tagName.toLowerCase();
        if (tag === 'input' || tag === 'textarea' || tag === 'select') return;

        // If focus is on a button inside a btn-group, blur it first
        if ($(e.target).is('.btn-group .btn, .btn-group-container .btn, button')) {
          e.target.blur();
        }

        e.preventDefault();
        var btnId = e.key === 'ArrowLeft' ? 'kwallm_sections_prev' : 'kwallm_sections_next';
        var btn = $('#' + btnId);
        if (btn.length && !btn.prop('disabled')) {
          btn.click();
          btn.blur(); // Don't leave focus on the nav button either
        }
      });

      // KWALLM: Update prev/next button state immediately on section change (client-side)
      var kwallmNumSections = 5;
      function kwallmUpdateNavButtons(sectionNum) {
        var prevBtn = $('#kwallm_sections_prev');
        var nextBtn = $('#kwallm_sections_next');
        if (sectionNum <= 1) {
          prevBtn.prop('disabled', true);
        } else {
          prevBtn.prop('disabled', false);
        }
        if (sectionNum >= kwallmNumSections) {
          nextBtn.prop('disabled', true);
        } else {
          nextBtn.prop('disabled', false);
        }
      }

      // Listen for section step button changes
      $(document).on('change', 'input[name=kwallm_sections_step]', function() {
        var val = parseInt($(this).val(), 10);
        if (!isNaN(val)) {
          kwallmUpdateNavButtons(val);
          // Persist section step in localStorage (skip in test mode)
          if (!(document.body && document.body.hasAttribute('data-shiny-testmode'))) {
            localStorage.setItem('kwallm_sections_step', val.toString());
          }
        }
      });

      // Also run on initial load if in sections mode
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          var checkedStep = $('input[name=kwallm_sections_step]:checked').val();
          if (checkedStep) {
            kwallmUpdateNavButtons(parseInt(checkedStep, 10));
          }
        }, 100);
      });

      // Server-driven updates (reliable when selection is changed programmatically)
      Shiny.addCustomMessageHandler('kwallm_nav_buttons', function(message) {
        try {
          if (!message) return;

          if (message.view !== 'sections') {
            // In vertical view, enable both buttons (they are not used)
            $('#kwallm_sections_prev').prop('disabled', false);
            $('#kwallm_sections_next').prop('disabled', false);
            return;
          }

          if (message.n_sections) {
            kwallmNumSections = message.n_sections;
          }
          kwallmUpdateNavButtons(parseInt(message.section, 10));
        } catch (e) {
          // no-op
        }
      });

      // KWALLM: Persist layout preference in localStorage
      // Skip localStorage restoration in test mode to preserve programmatic defaults
      
      // Helper to apply layout view (show/hide sections nav and sections)
      function kwallmApplyLayoutView(view) {
        var sectionsNav = document.getElementById('kwallm_sections_nav');
        if (!sectionsNav) return;
        
        if (view === 'vertical') {
          // Hide sections nav, show all sections
          sectionsNav.style.display = 'none';
          for (var i = 1; i <= 5; i++) {
            var section = document.getElementById('kwallm_section_' + i);
            if (section) section.style.display = '';
          }
        } else if (view === 'sections') {
          // Show sections nav, show only current section
          sectionsNav.style.display = '';
          // Try to get saved section from localStorage, fall back to checked button, then default to 1
          var savedSection = localStorage.getItem('kwallm_sections_step');
          var currentStep = savedSection || $('input[name=kwallm_sections_step]:checked').val() || '1';
          var currentSection = parseInt(currentStep, 10) || 1;
          
          // Also update the section step button if we're using saved value
          if (savedSection) {
            $('input[name=kwallm_sections_step][value=' + savedSection + ']').prop('checked', true);
            Shiny.setInputValue('kwallm_sections_step', savedSection, {priority: 'event'});
          }
          
          for (var i = 1; i <= 5; i++) {
            var section = document.getElementById('kwallm_section_' + i);
            if (section) {
              section.style.display = (i === currentSection) ? '' : 'none';
            }
          }
        }
      }
      
      // Helper to restore layout from localStorage
      function kwallmRestoreLayoutView() {
        // In test mode, Shiny sets body[data-shiny-testmode], skip localStorage restore
        if (document.body && document.body.hasAttribute('data-shiny-testmode')) {
          return;
        }
        var saved = localStorage.getItem('kwallm_layout_view');
        if (saved && (saved === 'vertical' || saved === 'sections')) {
          // Update the radio button to saved preference
          $('input[name=kwallm_layout_view][value=' + saved + ']').prop('checked', true).trigger('change');
          // Use priority: 'event' to ensure the change is always processed even if value is the same
          Shiny.setInputValue('kwallm_layout_view', saved, {priority: 'event'});
          // Also directly apply the layout view for immediate effect
          kwallmApplyLayoutView(saved);
        }
      }
      
      $(document).on('shiny:connected', function() {
        kwallmRestoreLayoutView();
      });
      
      // Restore layout after UI re-renders (e.g., after language change)
      // MutationObserver watches for when the layout controls are recreated
      var kwallmLayoutObserver = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
            // Check if the layout controls were added
            for (var i = 0; i < mutation.addedNodes.length; i++) {
              var node = mutation.addedNodes[i];
              if (node.nodeType === 1) { // Element node
                // Check if this node or its children contain the layout controls
                var layoutControls = node.id === 'kwallm_layout_controls' ? node : 
                                     (node.querySelector ? node.querySelector('#kwallm_layout_controls') : null);
                if (layoutControls) {
                  // Delay slightly to ensure Shiny has bound the inputs
                  setTimeout(kwallmRestoreLayoutView, 50);
                  break;
                }
              }
            }
          }
        });
      });
      
      // Start observing once DOM is ready
      $(document).ready(function() {
        var mainUi = document.getElementById('main_ui');
        if (mainUi) {
          kwallmLayoutObserver.observe(mainUi, { childList: true, subtree: true });
        } else {
          // If main_ui not yet available, wait for it
          var bodyObserver = new MutationObserver(function(mutations, obs) {
            var mainUi = document.getElementById('main_ui');
            if (mainUi) {
              kwallmLayoutObserver.observe(mainUi, { childList: true, subtree: true });
              obs.disconnect();
            }
          });
          bodyObserver.observe(document.body, { childList: true, subtree: true });
        }
      });

      $(document).on('change', 'input[name=kwallm_layout_view]', function() {
        var val = $(this).val();
        if (val) {
          localStorage.setItem('kwallm_layout_view', val);
        }
      });

      // KWALLM: Update progress bar smoothly (without DOM replacement)
      Shiny.addCustomMessageHandler('kwallm_sections_progress_update', function(message) {
        try {
          var bar = document.getElementById('kwallm_sections_progress_bar');
          var text = document.getElementById('kwallm_sections_progress_text');
          if (bar) {
            bar.style.width = message.pct + '%';
            bar.setAttribute('aria-valuenow', message.pct);
          }
          if (text) {
            text.textContent = message.text;
          }
        } catch (e) {
          // no-op
        }
      });

      // ---- KWALLM: cookie helpers (for persisting UI state) ------------
      // We use cookies (not localStorage) so settings persist per-browser
      // and can be read without depending on storage APIs in locked-down envs.
      // In Shiny test mode (shinytest2), skip cookie IO to keep tests deterministic.
      function kwallmIsTestMode() {
        try {
          return (document.body && document.body.hasAttribute('data-shiny-testmode'));
        } catch (e) {
          return false;
        }
      }

      function kwallmSetCookie(name, value, days) {
        try {
          if (!name) return;
          var expires = '';
          if (typeof days === 'number' && !isNaN(days)) {
            var date = new Date();
            date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
            expires = '; expires=' + date.toUTCString();
          }
          var v = (value === null || value === undefined) ? '' : String(value);
          document.cookie = encodeURIComponent(name) + '=' + encodeURIComponent(v) + expires + '; path=/';
        } catch (e) {
          // no-op
        }
      }

      function kwallmGetCookie(name) {
        try {
          if (!name) return null;
          var nameEQ = encodeURIComponent(name) + '=';
          var ca = document.cookie.split(';');
          for (var i = 0; i < ca.length; i++) {
            var c = ca[i];
            while (c.charAt(0) === ' ') c = c.substring(1, c.length);
            if (c.indexOf(nameEQ) === 0) {
              var raw = c.substring(nameEQ.length, c.length);
              var decoded = decodeURIComponent(raw || '');
              return decoded || null;
            }
          }
          return null;
        } catch (e) {
          return null;
        }
      }

      // Server -> browser: set cookie
      Shiny.addCustomMessageHandler('kwallm_cookie_set', function(message) {
        try {
          if (kwallmIsTestMode() && !(message && message.allow_testmode === true)) return;
          if (!message || !message.name) return;
          kwallmSetCookie(message.name, message.value, message.days || 365);
        } catch (e) {
          // no-op
        }
      });

      // Server -> browser: read cookie and send back as Shiny input
      // message: { name: 'cookie_name', input_id: 'ns(cookie_value_input)' }
      Shiny.addCustomMessageHandler('kwallm_cookie_get', function(message) {
        try {
          if (kwallmIsTestMode() && !(message && message.allow_testmode === true)) return;
          if (!message || !message.name || !message.input_id) return;
          var val = kwallmGetCookie(message.name);
          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue(message.input_id, val, { priority: 'event' });
          }
        } catch (e) {
          // no-op
        }
      });

      // ---- KWALLM: modal open/close auto logging -------------------------
      // Shiny renders modals using Bootstrap's .modal. We attach a marker element
      // inside each modal with attributes:
      //   data-kwallm-modal-id=\"filter_modal\" (required)
      //   data-kwallm-modal-details=\"...\" (optional)
      // This handler forwards modal show/hide events to the server.
      (function() {
        function getKwallmModalMeta(modalEl) {
          try {
            var $marker = $(modalEl).find('[data-kwallm-modal-id]').first();
            if (!$marker || $marker.length === 0) return null;
            return {
              id: $marker.attr('data-kwallm-modal-id'),
              details: $marker.attr('data-kwallm-modal-details') || null
            };
          } catch (e) {
            return null;
          }
        }

        function emitModalEvent(type, modalEl) {
          if (!window.Shiny || !window.Shiny.setInputValue) return;
          var meta = getKwallmModalMeta(modalEl);
          if (!meta || !meta.id) return;
          window.Shiny.setInputValue(
            'kwallm_modal_event',
            { type: type, id: meta.id, details: meta.details, ts: Date.now() },
            { priority: 'event' }
          );
        }

        $(document).on('shown.bs.modal', '.modal', function() {
          emitModalEvent('opened', this);
        });
        $(document).on('hidden.bs.modal', '.modal', function() {
          emitModalEvent('closed', this);
        });
      })();
    "
    ))
  )
}

# 2 Example/development usage ----------------------------------------
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
