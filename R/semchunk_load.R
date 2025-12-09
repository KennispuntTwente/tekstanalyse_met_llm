#### 1 Load semantic chunker ####

# Allows to load Python & then interrupt R without fatal R crash:
Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

semchunk_load_chunker <- function(
  tokenizer = "gpt-4", # name, HF repo, tiktoken encoding, or a custom Python object
  chunk_size = 64, # tokens per chunk (remember to subtract special tokens)
  test_chunker = FALSE, # run a quick round-trip on sample text?
  queue = NULL # 'ipc' queue for reactive logs
) {
  #### ───────── Argument checks ──────────────────────────────────────────────
  stopifnot(
    is.character(tokenizer) || inherits(tokenizer, "python.builtin.object"),
    is.numeric(chunk_size) && length(chunk_size) == 1 && chunk_size > 0,
    is.logical(test_chunker) && length(test_chunker) == 1,
    is.null(queue) || inherits(queue, "Queue")
  )

  #### ───────── Helper: log to console (+ shiny queue) ─────────────
  print_message <- async_message_printer(
    queue = queue,
    reactive_value_name = "semchunk_message"
  )

  ## ── Load Python & tiktoken module ───────────────────────────────
  print_message("Loading Python and semchunk module...")

  Sys.unsetenv("RETICULATE_PYTHON")
  reticulate:::uv_exec("sync")
  reticulate::use_virtualenv("./.venv")
  semchunk <- reticulate::import("semchunk")

  #### ───────── Build the chunker ────────────────────────────────
  print_message("Constructing chunker …")

  # AutoTokenizer or tiktoken only imported lazily if needed
  if (inherits(tokenizer, "python.builtin.object")) {
    tokenizer_obj <- tokenizer
  } else {
    tokenizer_obj <- tokenizer # let semchunk decide what to do with the string
  }

  chunker <- semchunk$chunkerify(tokenizer_obj, as.integer(chunk_size))

  #### ───────── Test ────────────────────────────────────────
  if (test_chunker) {
    print_message("Testing chunker on sample sentence …")
    sample_text <- "The quick brown fox jumps over the lazy dog."
    # Returns a Python list, automatically converted
    chunks <- chunker(sample_text)
    print_message(paste0("Chunks: ", paste(chunks, collapse = " | ")))
  }

  print_message("semchunk chunker ready!", type = "success")
  invisible(chunker)
}

#### 2 Example/development usage ####

if (FALSE) {
  chunker <- semchunk_load_chunker(chunk_size = 32)

  chunker(
    "Interviewer: So, can you tell me a little about your background and how you got into this field?

    Interviewee: Yeah, absolutely. Um, I started out studying environmental science at college, and—uh—what really drew me in was, like, the intersection between data and sustainability.

    Interviewer: Interesting. Could you elaborate on that?

    Interviewee: Sure. During my final year, I worked on this project where we used satellite imagery to monitor deforestation patterns. That kind of real-world application just clicked for me, you know?

    Interviewer: Mhm. And what happened after graduation?

    Interviewee: I joined a non-profit based in Colorado. We were doing field research, but I found myself gravitating toward the data side of things—cleaning it, analyzing trends, that sort of stuff.

    Interviewer: So more behind the scenes?

    Interviewee: Yeah, exactly. Although, I still did some fieldwork. I guess I like having that balance.

    Interviewer: Got it. Uh, let’s shift gears a bit. Can you talk about a challenge you faced and how you handled it?

    Interviewee: Sure thing. At one point, our dataset had, like, hundreds of inconsistencies—misspelled place names, duplicated entries, the whole mess. I ended up writing an R script to clean and standardize the entries. Took a while, but it was worth it.

    Interviewer: Sounds like you’re pretty comfortable coding?

    Interviewee: Yeah, I'd say so. R is kind of my go-to.

    Interviewer: Last question—where do you see yourself in five years?

    Interviewee: Hmm, hopefully leading a data science team that’s tackling real environmental problems. Either that or running a goat farm. laughs"
  )

  # Fake interviews
  interview_texts <- c(
    "Interviewer: Let's start with your background.\nInterviewee: Sure, I studied marine biology and moved into conservation work after that.\nInterviewer: What drew you to that?\nInterviewee: I've always loved the ocean, and data lets us protect it better.",

    "Interviewer: What’s your current role?\nInterviewee: I'm a data analyst at a healthcare startup.\nInterviewer: And your path there?\nInterviewee: Started with epidemiology, then moved into R programming and dashboards.",

    "Interviewer: Tell me about a challenge you faced.\nInterviewee: Sure, we had a noisy dataset with missing entries. I wrote a pipeline in R to clean it, visualize gaps, and patch using external sources.\nInterviewer: Sounds intense.\nInterviewee: It was, but satisfying.",

    "Interviewer: Where do you see yourself in five years?\nInterviewee: Hopefully leading a research team or consulting in climate tech.\nInterviewer: Interesting.\nInterviewee: Yeah, maybe also teaching part-time if time allows.",

    "Interviewer: What’s one project you’re proud of?\nInterviewee: A survey tool I built in Shiny to collect real-time feedback from field teams. It synced to a database and auto-generated reports. Made life easier for everyone."
  )

  interview_df <- tibble::tibble(
    id = paste0("resp_", seq_along(interview_texts)),
    text = interview_texts
  )

  # Apply chunking to each text row
  chunked_df <- interview_df %>%
    dplyr::mutate(
      chunks = purrr::map(text, function(x) {
        chunker(x, overlap = TRUE)
      })
    ) %>%
    dplyr::select(id, chunks) %>%
    tidyr::unnest_longer(chunks, values_to = "chunk_text")

  # View the chunked result
  print(chunked_df)
}
