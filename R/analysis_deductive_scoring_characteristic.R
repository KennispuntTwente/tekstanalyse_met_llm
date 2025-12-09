# Function to build a prompt for scoring a text based on a characteristic

#' Build prompt for scoring a text
#'
#' @param text Text to score
#' @param research_background Background information about the research
#' @param scoring_characteristic Characteristic to score the text on
#' (e.g., "emotional load", "clarity")
#'
#' @return A prompt object that can be used with `tidyprompt::send_prompt`
#' @export
prompt_score <- function(
  text,
  research_background,
  scoring_characteristic
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(scoring_characteristic),
    length(text) == 1,
    length(research_background) == 1,
    length(scoring_characteristic) == 1
  )

  instruction <- glue::glue(
    "You need to score a text for a research project.\n\n",
    "Research background:\n  {research_background}\n\n",
    "Text:\n  '{text}'",
    "\n\n",
    "Characteristic to score the text on:\n  {scoring_characteristic}",
    "\n\n",
    "Respond with a score (0-100) which tells how well the text fits the characteristic.",
    "\n",
    "(Where 0 means the text does not fit the characteristic at all and 100 means it fits perfectly.)",
    "\n",
    "(Use no other words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(x)
        score <- suppressWarnings(as.numeric(normalized))
        if (!is.na(score) && score >= 0 && score <= 100) {
          return(score)
        }
        return(tidyprompt::llm_feedback(instruction))
      }
    )

  return(prompt)
}
