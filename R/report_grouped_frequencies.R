# Helper functions for generating grouped frequency tables in reports
# Used by report_*.Rmd templates when a by_column is specified

# Internal: join by_values to df by stable row identity when available.
# by_values may contain either 'document_id' and 'by_value', or the older
# fallback shape with 'text' and 'by_value'.
.join_by_group <- function(df, by_values) {
  if (!is.data.frame(by_values)) {
    stop(
      paste(
        "by_values must be a data frame with either",
        "'document_id' and 'by_value' or 'text' and 'by_value'"
      )
    )
  }

  if (
    "document_id" %in%
      names(df) &&
      all(c("document_id", "by_value") %in% names(by_values))
  ) {
    join_key <- "document_id"
    lookup <- by_values[c("document_id", "by_value")]
  } else if (
    "text" %in% names(df) && all(c("text", "by_value") %in% names(by_values))
  ) {
    join_key <- "text"
    lookup <- by_values[c("text", "by_value")]
  } else {
    stop(
      paste(
        "by_values must contain join columns compatible with df:",
        "either shared document_id or shared text plus by_value"
      )
    )
  }

  # Use left_join so every result row gets at least one group.
  # relationship = "many-to-many" keeps the intentional fan-out when one row
  # belongs to multiple groups, while document_id prevents false cross products
  # for duplicate texts in the same group.
  df |>
    dplyr::left_join(
      lookup,
      by = join_key,
      relationship = "many-to-many"
    ) |>
    dplyr::rename(.by_group = by_value)
}

#' Generate grouped frequency table for single-category results
#' @param df data frame with 'result' column containing category assignments
#' @param by_values data frame with 'text' and 'by_value' columns
#' @param by_column_name name of the grouping column for display
#' @param categories vector of all possible categories
#' @param language "en" or "nl"
#' @return DT::datatable with frequencies per group
generate_grouped_freq_table_single <- function(
  df,
  by_values,
  by_column_name,
  categories,
  language = "en"
) {
  # Join group column to df
  df_grouped <- .join_by_group(df, by_values)

  all_groups <- unique(df_grouped$.by_group)

  # Count per group and category
  freq_table <- df_grouped |>
    dplyr::group_by(.by_group, result) |>
    dplyr::summarise(Number = dplyr::n(), .groups = "drop") |>
    tidyr::complete(
      .by_group = all_groups,
      result = categories,
      fill = list(Number = 0)
    )

  # Calculate percentages within each group
  freq_table <- freq_table |>
    dplyr::group_by(.by_group) |>
    dplyr::mutate(
      Group_Total = sum(Number),
      Percentage = dplyr::if_else(
        Group_Total > 0,
        round(Number / Group_Total * 100, 2),
        0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"Group_Total")

  # Rename columns based on language
  if (language == "nl") {
    freq_table <- freq_table |>
      dplyr::rename(
        Groep = .by_group,
        Categorie = result,
        Aantal = Number,
        Percentage = Percentage
      )
  } else {
    freq_table <- freq_table |>
      dplyr::rename(
        Group = .by_group,
        Category = result
      )
  }

  # Return datatable
  if (language == "nl") {
    DT::datatable(
      freq_table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_datatable_options(),
      caption = paste0("Frequenties per ", by_column_name)
    )
  } else {
    DT::datatable(
      freq_table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_datatable_options_en(),
      caption = paste0("Frequencies per ", by_column_name)
    )
  }
}

#' Generate grouped frequency table for multi-category results
#' @param df data frame with binary category columns
#' @param by_values data frame with 'text' and 'by_value' columns
#' @param by_column_name name of the grouping column for display
#' @param categories vector of category column names
#' @param language "en" or "nl"
#' @return DT::datatable with frequencies per group
generate_grouped_freq_table_multi <- function(
  df,
  by_values,
  by_column_name,
  categories,
  language = "en"
) {
  # Join group column to df
  df_grouped <- .join_by_group(df, by_values)

  # Count TRUE values per group and category
  freq_table <- df_grouped |>
    dplyr::group_by(.by_group) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(categories),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(categories),
      names_to = "Category",
      values_to = "Number"
    )

  # Calculate group totals for percentage calculation
  group_totals <- df_grouped |>
    dplyr::count(.by_group, name = "Group_Total")

  freq_table <- freq_table |>
    dplyr::left_join(group_totals, by = ".by_group") |>
    dplyr::mutate(
      Percentage = dplyr::if_else(
        Group_Total > 0,
        round(Number / Group_Total * 100, 2),
        0
      )
    ) |>
    dplyr::select(-"Group_Total")

  # Rename columns based on language
  if (language == "nl") {
    freq_table <- freq_table |>
      dplyr::rename(
        Groep = .by_group,
        Categorie = Category,
        Aantal = Number,
        Percentage = Percentage
      )
  } else {
    freq_table <- freq_table |>
      dplyr::rename(
        Group = .by_group
      )
  }

  # Return datatable
  if (language == "nl") {
    DT::datatable(
      freq_table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_datatable_options(),
      caption = paste0("Frequenties per ", by_column_name)
    )
  } else {
    DT::datatable(
      freq_table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_datatable_options_en(),
      caption = paste0("Frequencies per ", by_column_name)
    )
  }
}

#' Generate grouped frequency table for scoring results
#' @param df data frame with 'result' column containing numeric scores
#' @param by_values data frame with 'text' and 'by_value' columns
#' @param by_column_name name of the grouping column for display
#' @param language "en" or "nl"
#' @return DT::datatable with score statistics per group
generate_grouped_score_table <- function(
  df,
  by_values,
  by_column_name,
  language = "en"
) {
  # Join group column to df
  df_grouped <- .join_by_group(df, by_values)

  # Calculate statistics per group
  stats_table <- df_grouped |>
    dplyr::group_by(.by_group) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = round(mean(result, na.rm = TRUE), 2),
      SD = round(sd(result, na.rm = TRUE), 2),
      Min = min(result, na.rm = TRUE),
      Max = max(result, na.rm = TRUE),
      .groups = "drop"
    )

  # Rename columns based on language
  if (language == "nl") {
    stats_table <- stats_table |>
      dplyr::rename(
        Groep = .by_group,
        Gemiddelde = Mean
      )
  } else {
    stats_table <- stats_table |>
      dplyr::rename(
        Group = .by_group
      )
  }

  # Return datatable
  if (language == "nl") {
    DT::datatable(
      stats_table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_datatable_options(),
      caption = paste0("Scores per ", by_column_name)
    )
  } else {
    DT::datatable(
      stats_table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_datatable_options_en(),
      caption = paste0("Scores per ", by_column_name)
    )
  }
}

#' Generate grouped frequency table for topic extraction results
#' Same logic as categorization
generate_grouped_topic_table_single <- generate_grouped_freq_table_single
generate_grouped_topic_table_multi <- generate_grouped_freq_table_multi
