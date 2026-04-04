# Typed result model for one analysis run.
# This file defines the S7 classes behind the export contract.
# Shared validators and empty-table helpers stay first because S7 needs them
# available when the classes are created during sourcing.

# 1 Validation helpers ---------------------------------------------------------

# Small validators used by S7 properties and class-level checks.
# These keep bad data out of the result model before serialization starts.

# Returns NULL when no validation errors were collected.
# We use this in S7 validators, which expect NULL for success.
.kwallm_problems_or_null <- function(problems) {
  if (length(problems)) problems else NULL
}

# Validates one required character value.
# We use this for scalar string properties like run ids and score labels.
.kwallm_validate_scalar_string <- function(allow_empty = FALSE) {
  function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a single non-missing string")
    }
    if (!allow_empty && !nzchar(trimws(value))) {
      return("must be a non-empty string")
    }
    NULL
  }
}

# Validates an optional character value.
# We use this for metadata fields that may be absent in some runs.
.kwallm_validate_optional_scalar_string <- function(allow_empty = FALSE) {
  validator <- .kwallm_validate_scalar_string(allow_empty = allow_empty)
  function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    validator(value)
  }
}

# Validates one required logical value.
# We use this for flags such as split_enabled and human_in_the_loop.
.kwallm_validate_scalar_logical <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    return("must be a single non-missing logical value")
  }
  NULL
}

# Validates an optional logical value.
# We use this for input flags that are only known in some workflows.
.kwallm_validate_optional_scalar_logical <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  .kwallm_validate_scalar_logical(value)
}

# Builds a validator for one required numeric value.
# We use this for bounded numbers like overlap size and score limits.
.kwallm_validate_scalar_numeric <- function(min = NULL, max = NULL) {
  function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a single non-missing number")
    }
    if (!is.null(min) && value < min) {
      return(sprintf("must be >= %s", min))
    }
    if (!is.null(max) && value > max) {
      return(sprintf("must be <= %s", max))
    }
    NULL
  }
}

# Builds a validator for one optional numeric value.
# We use this for optional context-window and split settings.
.kwallm_validate_optional_scalar_numeric <- function(min = NULL, max = NULL) {
  validator <- .kwallm_validate_scalar_numeric(min = min, max = max)
  function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    validator(value)
  }
}

# Builds a validator for one required integer value.
# We use this for schema_version and other true integer fields.
.kwallm_validate_scalar_integer <- function(min = NULL) {
  function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a single non-missing integer value")
    }
    if (!is.integer(value)) {
      return("must be an integer value")
    }
    if (!is.null(min) && value < min) {
      return(sprintf("must be >= %s", min))
    }
    NULL
  }
}

# Builds a validator for one optional integer value.
# We use this for optional iteration counts and other nullable integer fields.
.kwallm_validate_optional_scalar_integer <- function(min = NULL) {
  validator <- .kwallm_validate_scalar_integer(min = min)
  function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    validator(value)
  }
}

# Validates the canonical mode id.
# We use this to keep mode-dependent dispatch stable across builders and serializers.
.kwallm_validate_mode_id <- function(value) {
  allowed <- c("categorization", "scoring", "topic_extraction", "marking")
  if (length(value) != 1L || is.na(value) || !(value %in% allowed)) {
    return(sprintf(
      "must be one of: %s",
      paste(allowed, collapse = ", ")
    ))
  }
  NULL
}

# Validates the app language code.
# We use this so exports only record supported UI languages.
.kwallm_validate_language <- function(value) {
  allowed <- c("en", "nl")
  if (length(value) != 1L || is.na(value) || !(value %in% allowed)) {
    return("must be 'en' or 'nl'")
  }
  NULL
}

# Validates one POSIXct timestamp.
# We use this for the run timestamp stored in metadata.
.kwallm_validate_posixct_scalar <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    return("must be a single non-missing datetime")
  }
  NULL
}

# Validates that a list has names for every entry.
# We use this for reliability summary objects that are serialized as key-value data.
.kwallm_validate_named_list <- function(value) {
  if (is.null(names(value)) || !all(nzchar(names(value)))) {
    return("must be a named list")
  }
  NULL
}

# Builds a validator that checks required data-frame columns.
# We use this on most table-like properties in the result model.
.kwallm_validate_df_columns <- function(required_cols) {
  function(value) {
    missing_cols <- setdiff(required_cols, names(value))
    if (length(missing_cols)) {
      return(sprintf(
        "must contain columns: %s",
        paste(missing_cols, collapse = ", ")
      ))
    }
    NULL
  }
}


# 2 Empty table helpers --------------------------------------------------------

# Provides empty data frames for defaults on S7 properties.
# These keep the model typed even when a section has no rows.

# Builds an empty source-document table.
# We use this as the default for lineage before any source texts are attached.
.kwallm_empty_source_documents <- function() {
  data.frame(
    source_document_id = integer(),
    source_text = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty document table.
# We use this as the default for per-row document lineage.
.kwallm_empty_documents <- function() {
  data.frame(
    document_id = integer(),
    source_document_id = integer(),
    document_text = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty analysis-unit table.
# We use this when no preprocessed analysis units are available yet.
.kwallm_empty_analysis_units <- function() {
  data.frame(
    analysis_unit_id = integer(),
    preprocessed_text = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty document-to-analysis-unit map.
# We use this as the default for lineage joins.
.kwallm_empty_document_units <- function() {
  data.frame(
    document_id = integer(),
    analysis_unit_id = integer(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty document-group table.
# We use this when the upload has no grouping column.
.kwallm_empty_document_groups <- function() {
  data.frame(
    source_document_id = integer(),
    group_value = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty stage-model table.
# We use this before model provenance has been filled in.
.kwallm_empty_stage_models <- function() {
  data.frame(
    stage_id = character(),
    provider_kind = character(),
    api_url = character(),
    model_id = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty stage-prompt table.
# We use this when no prompt previews are available.
.kwallm_empty_stage_prompts <- function() {
  data.frame(
    stage_id = character(),
    prompt_preview = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty stage-execution table.
# We use this when no execution provenance was captured for a run.
.kwallm_empty_stage_executions <- function() {
  data.frame(
    prompt_id = character(),
    stage_id = character(),
    model_id = character(),
    started_at = character(),
    completed_at = character(),
    duration_ms = numeric(),
    attempt_count = integer(),
    retry_count = integer(),
    max_tries = integer(),
    retry_delay_seconds = numeric(),
    max_interactions = integer(),
    completion_status = character(),
    error_messages = character(),
    final_error_message = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty issues table.
# We use this when no warnings were produced during a run.
.kwallm_empty_issues <- function() {
  data.frame(
    stage_id = character(),
    level = character(),
    issue_code = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty paragraphs table.
# We use this as the default when paragraph generation is disabled or absent.
.kwallm_empty_paragraphs <- function() {
  data.frame(
    paragraph_id = integer(),
    subject_kind = character(),
    subject_id = integer(),
    paragraph_text = character(),
    prompt_fits = logical(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty paragraph-source table.
# We use this when no paragraph-to-document links were captured.
.kwallm_empty_paragraph_sources <- function() {
  data.frame(
    paragraph_id = integer(),
    document_id = integer(),
    excerpt_text = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty labels table.
# We use this as the default for categorization and topic label sets.
.kwallm_empty_labels <- function() {
  data.frame(
    label_id = integer(),
    label_text = character(),
    is_exclusive = logical(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty assignments table.
# We use this when no label assignments have been created yet.
.kwallm_empty_assignments <- function() {
  data.frame(
    analysis_unit_id = integer(),
    label_id = integer(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty scores table.
# We use this as the default for scoring payloads.
.kwallm_empty_scores <- function() {
  data.frame(
    analysis_unit_id = integer(),
    score = numeric(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty codes table.
# We use this as the default for marking-code metadata.
.kwallm_empty_codes <- function() {
  data.frame(
    code_id = integer(),
    code_text = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty chunks table.
# We use this when marking did not produce any text chunks.
.kwallm_empty_chunks <- function() {
  data.frame(
    chunk_id = integer(),
    analysis_unit_id = integer(),
    chunk_index = integer(),
    chunk_text = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty markings table.
# We use this when there are no marked spans to store.
.kwallm_empty_markings <- function() {
  data.frame(
    mark_id = integer(),
    chunk_id = integer(),
    code_id = integer(),
    source_marked_text = character(),
    marked_text = character(),
    match_start = integer(),
    match_end = integer(),
    match_distance = integer(),
    match_method = character(),
    response_status = character(),
    stringsAsFactors = FALSE
  )
}

# Builds an empty topic-generation settings table.
# We use this as the default for topic mode configuration.
.kwallm_empty_topic_generation_settings <- function() {
  data.frame(
    setting = character(),
    value = character(),
    stringsAsFactors = FALSE
  )
}


# 3 Shared classes -------------------------------------------------------------

# Defines the common classes used by all modes.
# These classes hold run metadata, provenance, and shared auxiliary tables.

# Stores run-level metadata shared by every analysis mode.
AnalysisMetadata <- S7::new_class(
  "AnalysisMetadata",
  properties = list(
    schema_version = S7::new_property(
      S7::class_integer,
      default = 1L,
      validator = .kwallm_validate_scalar_integer(min = 1L)
    ),
    run_id = S7::new_property(
      S7::class_character,
      validator = .kwallm_validate_scalar_string()
    ),
    mode_id = S7::new_property(
      S7::class_character,
      validator = .kwallm_validate_mode_id
    ),
    language = S7::new_property(
      S7::class_character,
      validator = .kwallm_validate_language
    ),
    timestamp = S7::new_property(
      S7::class_POSIXct,
      default = quote(Sys.time()),
      validator = .kwallm_validate_posixct_scalar
    ),
    research_background = S7::new_property(
      S7::class_character,
      default = "",
      validator = .kwallm_validate_scalar_string(allow_empty = TRUE)
    ),
    app_version = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    )
  )
)

# Stores upload and preprocessing provenance for one run.
AnalysisInput <- S7::new_class(
  "AnalysisInput",
  properties = list(
    file_type = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    ),
    selected_sheet = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    ),
    text_column = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    ),
    grouping_column = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    ),
    filter_spec = S7::new_property(
      NULL | S7::class_data.frame,
      default = NULL
    ),
    txt_split_lines = S7::new_property(
      NULL | S7::class_logical,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_logical
    ),
    anonymization_requested_mode = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    ),
    anonymization_applied_mode = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string()
    ),
    anonymization_completed = S7::new_property(
      NULL | S7::class_logical,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_logical
    ),
    split_enabled = S7::new_property(
      NULL | S7::class_logical,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_logical
    ),
    split_chunk_size = S7::new_property(
      NULL | S7::class_numeric,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_numeric(min = 0)
    ),
    split_overlap = S7::new_property(
      NULL | S7::class_numeric,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_numeric(min = 0)
    )
  )
)

# Stores the lineage from uploaded source texts to documents and analysis units.
TextLineage <- S7::new_class(
  "TextLineage",
  properties = list(
    source_documents = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_source_documents()),
      validator = .kwallm_validate_df_columns(c(
        "source_document_id",
        "source_text"
      ))
    ),
    documents = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_documents()),
      validator = .kwallm_validate_df_columns(c(
        "document_id",
        "source_document_id",
        "document_text"
      ))
    ),
    analysis_units = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_analysis_units()),
      validator = .kwallm_validate_df_columns(c(
        "analysis_unit_id",
        "preprocessed_text"
      ))
    ),
    document_units = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_document_units()),
      validator = .kwallm_validate_df_columns(c(
        "document_id",
        "analysis_unit_id"
      ))
    ),
    document_groups = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_document_groups()),
      validator = .kwallm_validate_df_columns(c(
        "source_document_id",
        "group_value"
      ))
    )
  ),
  validator = function(self) {
    problems <- character()

    if (anyDuplicated(self@source_documents$source_document_id)) {
      problems <- c(
        problems,
        "source_documents$source_document_id must be unique"
      )
    }
    if (anyDuplicated(self@documents$document_id)) {
      problems <- c(problems, "documents$document_id must be unique")
    }
    if (anyDuplicated(self@analysis_units$analysis_unit_id)) {
      problems <- c(problems, "analysis_units$analysis_unit_id must be unique")
    }
    if (anyDuplicated(self@document_units$document_id)) {
      problems <- c(
        problems,
        "document_units must contain at most one row per document_id"
      )
    }
    if (
      !all(
        self@documents$source_document_id %in%
          self@source_documents$source_document_id
      )
    ) {
      problems <- c(
        problems,
        "documents$source_document_id must reference source_documents"
      )
    }
    if (!all(self@document_units$document_id %in% self@documents$document_id)) {
      problems <- c(
        problems,
        "document_units$document_id must reference documents"
      )
    }
    if (
      !all(
        self@document_units$analysis_unit_id %in%
          self@analysis_units$analysis_unit_id
      )
    ) {
      problems <- c(
        problems,
        "document_units$analysis_unit_id must reference analysis_units"
      )
    }
    if (
      nrow(self@document_groups) > 0 &&
        !all(
          self@document_groups$source_document_id %in%
            self@source_documents$source_document_id
        )
    ) {
      problems <- c(
        problems,
        "document_groups$source_document_id must reference source_documents"
      )
    }

    .kwallm_problems_or_null(problems)
  }
)

# Stores the model used for each analysis stage.
StageModelTable <- S7::new_class(
  "StageModelTable",
  properties = list(
    rows = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_stage_models()),
      validator = .kwallm_validate_df_columns(c(
        "stage_id",
        "provider_kind",
        "api_url",
        "model_id"
      ))
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@rows$stage_id)) {
      problems <- c(problems, "rows$stage_id must be unique")
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores the prompt preview used for each analysis stage.
StagePromptTable <- S7::new_class(
  "StagePromptTable",
  properties = list(
    rows = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_stage_prompts()),
      validator = .kwallm_validate_df_columns(c("stage_id", "prompt_preview"))
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@rows$stage_id)) {
      problems <- c(problems, "rows$stage_id must be unique")
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores one execution record per LLM call made during a run.
StageExecutionTable <- S7::new_class(
  "StageExecutionTable",
  properties = list(
    rows = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_stage_executions()),
      validator = .kwallm_validate_df_columns(c(
        "prompt_id",
        "stage_id",
        "model_id",
        "started_at",
        "completed_at",
        "duration_ms",
        "attempt_count",
        "retry_count",
        "max_tries",
        "retry_delay_seconds",
        "max_interactions",
        "completion_status",
        "error_messages",
        "final_error_message"
      ))
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@rows$prompt_id)) {
      problems <- c(problems, "rows$prompt_id must be unique")
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores non-fatal issues found while building the final result object.
IssueTable <- S7::new_class(
  "IssueTable",
  properties = list(
    issues = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_issues()),
      validator = .kwallm_validate_df_columns(c(
        "stage_id",
        "level",
        "issue_code",
        "message"
      ))
    )
  )
)

# Stores generated paragraphs and the document rows that support them.
ParagraphSet <- S7::new_class(
  "ParagraphSet",
  properties = list(
    paragraphs = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_paragraphs()),
      validator = .kwallm_validate_df_columns(c(
        "paragraph_id",
        "subject_kind",
        "subject_id",
        "paragraph_text",
        "prompt_fits"
      ))
    ),
    paragraph_sources = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_paragraph_sources()),
      validator = .kwallm_validate_df_columns(c(
        "paragraph_id",
        "document_id",
        "excerpt_text"
      ))
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@paragraphs$paragraph_id)) {
      problems <- c(problems, "paragraphs$paragraph_id must be unique")
    }
    if (
      nrow(self@paragraph_sources) > 0 &&
        !all(
          self@paragraph_sources$paragraph_id %in% self@paragraphs$paragraph_id
        )
    ) {
      problems <- c(
        problems,
        "paragraph_sources$paragraph_id must reference paragraphs"
      )
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores interrater reliability output and the sampled rows used to compute it.
ReliabilityResult <- S7::new_class(
  "ReliabilityResult",
  properties = list(
    summary = S7::new_property(
      S7::class_list,
      validator = .kwallm_validate_named_list
    ),
    sample = S7::new_property(
      NULL | S7::class_data.frame,
      default = NULL
    )
  )
)

# Stores the topic-generation history for topic extraction runs.
TopicProvenance <- S7::new_class(
  "TopicProvenance",
  properties = list(
    candidate_topics = S7::new_property(
      S7::class_character,
      default = character()
    ),
    reduced_topics = S7::new_property(
      S7::class_character,
      default = character()
    ),
    final_topics = S7::new_property(
      S7::class_character,
      default = character()
    ),
    human_edited = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    not_applicable_requested = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    auto_added_not_applicable = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    not_applicable_check_performed = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    reduction_iterations = S7::new_property(
      NULL | S7::class_integer,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_integer(min = 0L)
    ),
    chunk_size = S7::new_property(
      NULL | S7::class_numeric,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_numeric(min = 0)
    ),
    draws = S7::new_property(
      NULL | S7::class_numeric,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_numeric(min = 0)
    ),
    n_chunks = S7::new_property(
      NULL | S7::class_numeric,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_numeric(min = 0)
    ),
    context_window_tokens = S7::new_property(
      NULL | S7::class_numeric,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_numeric(min = 0)
    )
  ),
  validator = function(self) {
    if (!length(self@final_topics)) {
      return("final_topics must contain at least one topic")
    }
    NULL
  }
)


# 4 Result payload classes -----------------------------------------------------

# Defines the mode-specific output payloads.
# Each mode gets its own typed result shape, but all inherit from ResultPayload.

# Base class for mode-specific result payloads.
ResultPayload <- S7::new_class("ResultPayload", abstract = TRUE)

# Stores categorization labels and assignments.
CategorizationResult <- S7::new_class(
  "CategorizationResult",
  parent = ResultPayload,
  properties = list(
    labels = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_labels()),
      validator = .kwallm_validate_df_columns(c(
        "label_id",
        "label_text",
        "is_exclusive"
      ))
    ),
    assignments = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_assignments()),
      validator = .kwallm_validate_df_columns(c("analysis_unit_id", "label_id"))
    ),
    multi_label = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@labels$label_id)) {
      problems <- c(problems, "labels$label_id must be unique")
    }
    if (anyDuplicated(self@labels$label_text)) {
      problems <- c(problems, "labels$label_text must be unique")
    }
    if (
      nrow(self@assignments) > 0 &&
        !all(self@assignments$label_id %in% self@labels$label_id)
    ) {
      problems <- c(problems, "assignments$label_id must reference labels")
    }
    if (
      !isTRUE(self@multi_label) &&
        anyDuplicated(self@assignments$analysis_unit_id)
    ) {
      problems <- c(
        problems,
        "single-label assignments must contain at most one row per analysis_unit_id"
      )
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores numeric scoring results.
ScoringResult <- S7::new_class(
  "ScoringResult",
  parent = ResultPayload,
  properties = list(
    scores = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_scores()),
      validator = .kwallm_validate_df_columns(c("analysis_unit_id", "score"))
    ),
    characteristic = S7::new_property(
      S7::class_character,
      validator = .kwallm_validate_scalar_string()
    ),
    scale_min = S7::new_property(
      S7::class_numeric,
      default = 0,
      validator = .kwallm_validate_scalar_numeric()
    ),
    scale_max = S7::new_property(
      S7::class_numeric,
      default = 100,
      validator = .kwallm_validate_scalar_numeric()
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@scores$analysis_unit_id)) {
      problems <- c(problems, "scores$analysis_unit_id must be unique")
    }
    if (
      nrow(self@scores) > 0 &&
        any(
          is.na(self@scores$score) |
            self@scores$score < self@scale_min |
            self@scores$score > self@scale_max
        )
    ) {
      problems <- c(
        problems,
        sprintf(
          "scores$score must be within [%s, %s]",
          self@scale_min,
          self@scale_max
        )
      )
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores topic labels, assignments, and topic-generation provenance.
TopicResult <- S7::new_class(
  "TopicResult",
  parent = ResultPayload,
  properties = list(
    labels = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_labels()),
      validator = .kwallm_validate_df_columns(c(
        "label_id",
        "label_text",
        "is_exclusive"
      ))
    ),
    assignments = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_assignments()),
      validator = .kwallm_validate_df_columns(c("analysis_unit_id", "label_id"))
    ),
    multi_label = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    topic_provenance = S7::new_property(TopicProvenance)
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@labels$label_id)) {
      problems <- c(problems, "labels$label_id must be unique")
    }
    if (anyDuplicated(self@labels$label_text)) {
      problems <- c(problems, "labels$label_text must be unique")
    }
    if (
      nrow(self@assignments) > 0 &&
        !all(self@assignments$label_id %in% self@labels$label_id)
    ) {
      problems <- c(problems, "assignments$label_id must reference labels")
    }
    if (
      !isTRUE(self@multi_label) &&
        anyDuplicated(self@assignments$analysis_unit_id)
    ) {
      problems <- c(
        problems,
        "single-label assignments must contain at most one row per analysis_unit_id"
      )
    }
    if (!all(self@labels$label_text %in% self@topic_provenance@final_topics)) {
      problems <- c(
        problems,
        "labels$label_text must be represented in topic_provenance@final_topics"
      )
    }
    .kwallm_problems_or_null(problems)
  }
)

# Stores marking codes, chunks, and individual marked spans.
MarkingResult <- S7::new_class(
  "MarkingResult",
  parent = ResultPayload,
  properties = list(
    codes = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_codes()),
      validator = .kwallm_validate_df_columns(c("code_id", "code_text"))
    ),
    chunks = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_chunks()),
      validator = .kwallm_validate_df_columns(c(
        "chunk_id",
        "analysis_unit_id",
        "chunk_index",
        "chunk_text"
      ))
    ),
    markings = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_markings()),
      validator = .kwallm_validate_df_columns(c(
        "mark_id",
        "chunk_id",
        "code_id",
        "source_marked_text",
        "marked_text",
        "match_start",
        "match_end",
        "match_distance",
        "match_method",
        "response_status"
      ))
    )
  ),
  validator = function(self) {
    problems <- character()
    if (anyDuplicated(self@codes$code_id)) {
      problems <- c(problems, "codes$code_id must be unique")
    }
    if (anyDuplicated(self@chunks$chunk_id)) {
      problems <- c(problems, "chunks$chunk_id must be unique")
    }
    if (anyDuplicated(self@markings$mark_id)) {
      problems <- c(problems, "markings$mark_id must be unique")
    }
    if (
      nrow(self@markings) > 0 &&
        !all(self@markings$chunk_id %in% self@chunks$chunk_id)
    ) {
      problems <- c(problems, "markings$chunk_id must reference chunks")
    }
    if (
      nrow(self@markings) > 0 &&
        !all(self@markings$code_id %in% self@codes$code_id)
    ) {
      problems <- c(problems, "markings$code_id must reference codes")
    }
    .kwallm_problems_or_null(problems)
  }
)


# 5 Mode config classes --------------------------------------------------------

# Defines the mode-specific settings stored alongside results.
# These capture how a run was configured, not what it produced.

# Base class for mode-specific run configuration.
ModeConfig <- S7::new_class("ModeConfig", abstract = TRUE)

# Stores categorization-specific settings.
CategorizationConfig <- S7::new_class(
  "CategorizationConfig",
  parent = ModeConfig,
  properties = list(
    assign_multiple_categories = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    human_in_the_loop = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    write_paragraphs = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    paragraph_style_prompt = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string(allow_empty = TRUE)
    )
  )
)

# Stores scoring-specific settings.
ScoringConfig <- S7::new_class(
  "ScoringConfig",
  parent = ModeConfig,
  properties = list(
    scoring_characteristic = S7::new_property(
      S7::class_character,
      validator = .kwallm_validate_scalar_string()
    )
  )
)

# Stores topic-extraction-specific settings.
TopicConfig <- S7::new_class(
  "TopicConfig",
  parent = ModeConfig,
  properties = list(
    assign_multiple_categories = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    human_in_the_loop = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    write_paragraphs = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    paragraph_style_prompt = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string(allow_empty = TRUE)
    ),
    topic_generation_settings = S7::new_property(
      S7::class_data.frame,
      default = quote(.kwallm_empty_topic_generation_settings()),
      validator = .kwallm_validate_df_columns(c("setting", "value"))
    )
  )
)

# Stores marking-specific settings.
MarkingConfig <- S7::new_class(
  "MarkingConfig",
  parent = ModeConfig,
  properties = list(
    write_paragraphs = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = .kwallm_validate_scalar_logical
    ),
    paragraph_style_prompt = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = .kwallm_validate_optional_scalar_string(allow_empty = TRUE)
    ),
    text_size_tokens = S7::new_property(
      S7::class_numeric,
      default = 0,
      validator = .kwallm_validate_scalar_numeric(min = 0)
    ),
    overlap_size_tokens = S7::new_property(
      S7::class_numeric,
      default = 0,
      validator = .kwallm_validate_scalar_numeric(min = 0)
    )
  )
)


# 6 Top-level result class -----------------------------------------------------

# Defines the full typed contract used across exports and reports.
# This is the object built by result_builders.R and consumed by serializers.

#' Top-level typed result for one analysis run
#'
#' Bundles run metadata, input provenance, text lineage, model and prompt
#' provenance, mode-specific results, optional paragraph and reliability data,
#' and non-fatal issues for exports.
AnalysisResult <- S7::new_class(
  "AnalysisResult",
  properties = list(
    metadata = S7::new_property(AnalysisMetadata),
    input = S7::new_property(
      AnalysisInput,
      default = quote(AnalysisInput())
    ),
    text_lineage = S7::new_property(TextLineage),
    stage_models = S7::new_property(
      StageModelTable,
      default = quote(StageModelTable())
    ),
    stage_prompts = S7::new_property(
      StagePromptTable,
      default = quote(StagePromptTable())
    ),
    stage_executions = S7::new_property(
      StageExecutionTable,
      default = quote(StageExecutionTable())
    ),
    results = S7::new_property(ResultPayload),
    paragraphs = S7::new_property(
      ParagraphSet,
      default = quote(ParagraphSet())
    ),
    reliability = S7::new_property(
      NULL | ReliabilityResult,
      default = NULL
    ),
    issues = S7::new_property(
      IssueTable,
      default = quote(IssueTable())
    ),
    mode_config = S7::new_property(ModeConfig)
  ),
  validator = function(self) {
    expected <- list(
      categorization = c("CategorizationResult", "CategorizationConfig"),
      scoring = c("ScoringResult", "ScoringConfig"),
      topic_extraction = c("TopicResult", "TopicConfig"),
      marking = c("MarkingResult", "MarkingConfig")
    )
    pair <- expected[[self@metadata@mode_id]]
    problems <- character()

    if (!inherits(self@results, pair[[1]])) {
      problems <- c(
        problems,
        sprintf(
          "results must inherit from %s for mode_id '%s'",
          pair[[1]],
          self@metadata@mode_id
        )
      )
    }
    if (!inherits(self@mode_config, pair[[2]])) {
      problems <- c(
        problems,
        sprintf(
          "mode_config must inherit from %s for mode_id '%s'",
          pair[[2]],
          self@metadata@mode_id
        )
      )
    }

    if (
      nrow(self@stage_executions@rows) > 0 &&
        !all(
          self@stage_executions@rows$stage_id %in%
            self@stage_models@rows$stage_id
        )
    ) {
      problems <- c(
        problems,
        "stage_executions$stage_id must reference stage_models"
      )
    }

    valid_document_ids <- self@text_lineage@documents$document_id
    if (
      nrow(self@paragraphs@paragraph_sources) > 0 &&
        !all(
          self@paragraphs@paragraph_sources$document_id %in% valid_document_ids
        )
    ) {
      problems <- c(
        problems,
        "paragraph_sources$document_id must reference text_lineage@documents"
      )
    }

    if (nrow(self@paragraphs@paragraphs) > 0) {
      paragraph_kind <- unique(self@paragraphs@paragraphs$subject_kind)

      if (self@metadata@mode_id %in% c("categorization", "topic_extraction")) {
        valid_ids <- self@results@labels$label_id
        if (!all(paragraph_kind %in% "label")) {
          problems <- c(
            problems,
            "paragraphs subject_kind must be 'label' for categorization/topic results"
          )
        }
        if (!all(self@paragraphs@paragraphs$subject_id %in% valid_ids)) {
          problems <- c(
            problems,
            "paragraphs$subject_id must reference result labels"
          )
        }
      }

      if (self@metadata@mode_id == "marking") {
        valid_ids <- self@results@codes$code_id
        if (!all(paragraph_kind %in% "code")) {
          problems <- c(
            problems,
            "paragraphs subject_kind must be 'code' for marking results"
          )
        }
        if (!all(self@paragraphs@paragraphs$subject_id %in% valid_ids)) {
          problems <- c(
            problems,
            "paragraphs$subject_id must reference marking codes"
          )
        }
      }

      if (self@metadata@mode_id == "scoring") {
        problems <- c(problems, "scoring results should not contain paragraphs")
      }
    }

    .kwallm_problems_or_null(problems)
  }
)


# 7 Mode helper ----------------------------------------------------------------

# Provides the small shared mode-name conversion used by builders.
# We keep it here because the canonical mode ids belong to the result model.

# Converts a display-mode label to the canonical mode id.
# We use this when runtime UI values are turned into typed result metadata.
.kwallm_mode_id_from_display <- function(mode) {
  switch(
    mode,
    Categorisatie = "categorization",
    Scoren = "scoring",
    Onderwerpextractie = "topic_extraction",
    Markeren = "marking",
    mode
  )
}
