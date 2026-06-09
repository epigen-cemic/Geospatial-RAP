build_table_filter_ui <- function(ns, id_prefix, df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  column_choices <- names(df)
  selected_col <- column_choices[[1]]

  div(
    class = "table-filter-bar",
    tags$label(
      class = "table-filter-inline-label",
      "Search by:"
    ),
    selectInput(
      ns(paste0(id_prefix, "_filter_col")),
      label = NULL,
      choices = column_choices,
      selected = selected_col,
      width = "220px"
    ),
    uiOutput(ns(paste0(id_prefix, "_filter_value_ui")), inline = TRUE)
  )
}

build_table_filter_value_ui <- function(ns, id_prefix, df, selected_col) {
  if (is.null(df) || is.null(selected_col) || !selected_col %in% names(df)) {
    return(NULL)
  }

  values <- df[[selected_col]]

  if (is.numeric(values)) {
    range_values <- range(values, na.rm = TRUE)

    if (!all(is.finite(range_values))) {
      return(NULL)
    }

    return(
      div(
        class = "table-filter-values",
        numericInput(
          ns(paste0(id_prefix, "_filter_min")),
          label = NULL,
          value = range_values[[1]],
          min = range_values[[1]],
          max = range_values[[2]],
          width = "130px"
        ),
        tags$span(class = "table-filter-separator", "to"),
        numericInput(
          ns(paste0(id_prefix, "_filter_max")),
          label = NULL,
          value = range_values[[2]],
          min = range_values[[1]],
          max = range_values[[2]],
          width = "130px"
        )
      )
    )
  }

  textInput(
    ns(paste0(id_prefix, "_filter_text")),
    label = NULL,
    value = "",
    placeholder = paste("Search", selected_col),
    width = "260px"
  )
}

apply_table_filter <- function(df, selected_col, text_value = NULL, min_value = NULL, max_value = NULL) {
  if (is.null(df) || is.null(selected_col) || !selected_col %in% names(df)) {
    return(df)
  }

  values <- df[[selected_col]]

  if (is.numeric(values)) {
    if (!is.null(min_value) && is.finite(min_value)) {
      df <- df[is.na(df[[selected_col]]) | df[[selected_col]] >= min_value, , drop = FALSE]
    }

    if (!is.null(max_value) && is.finite(max_value)) {
      df <- df[is.na(df[[selected_col]]) | df[[selected_col]] <= max_value, , drop = FALSE]
    }

    return(df)
  }

  if (is.null(text_value) || !nzchar(text_value)) {
    return(df)
  }

  keep <- grepl(text_value, as.character(values), ignore.case = TRUE, fixed = TRUE)
  df[keep, , drop = FALSE]
}

#' Register Geospatial module logic: helpers.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_helpers <- function(state) {
  evalq({
    ns <- session$ns

    format_uploaded_file_size <- function(bytes) {
      if (is.null(bytes) || is.na(bytes)) return("")
      if (bytes < 1024) return(paste0(bytes, " B"))
      if (bytes < 1024^2) return(paste0(round(bytes / 1024, 1), " KB"))
      paste0(round(bytes / 1024^2, 1), " MB")
    }

    format_uploaded_timestamp <- function(x) {
      if (is.null(x) || is.na(x) || !nzchar(as.character(x))) return("")
      format(as.POSIXct(x), "%Y-%m-%d %H:%M")
    }

    current_font_size_key <- reactive({
      value <- input$font_size
      if (is.null(value) || !nzchar(value)) "small" else value
    })

    display_digits <- reactive({
      value <- suppressWarnings(as.integer(input$display_digits))
      if (is.na(value)) 4L else max(0L, min(8L, value))
    })

    observe({
      size_class <- paste0("font-size-", current_font_size_key())
      shinyjs::runjs(sprintf(
        "document.body.classList.remove('font-size-small','font-size-medium','font-size-large','font-size-xlarge'); document.body.classList.add('%s');",
        size_class
      ))
    })

    geospatial_file_metadata <- function(path, country = active_country) {
      empty <- list(rows = NA_integer_, year_week_range = "Not available")

      tryCatch({
        raw <- api_pop_read_file(path)
        rows <- nrow(raw)

        std <- tryCatch(
          api_pop_standardise_columns(
            raw,
            config = config,
            active_country = country,
            require_population = FALSE
          ),
          error = function(e) raw
        )

        names(std) <- tolower(trimws(names(std)))
        if (!all(c("year", "week") %in% names(std))) {
          empty$rows <- rows
          return(empty)
        }

        time_df <- std %>%
          dplyr::mutate(
            year = suppressWarnings(as.integer(.data$year)),
            week = suppressWarnings(as.integer(.data$week))
          ) %>%
          dplyr::filter(!is.na(.data$year), !is.na(.data$week))

        if (nrow(time_df) == 0) {
          empty$rows <- rows
          return(empty)
        }

        min_row <- time_df %>% dplyr::arrange(.data$year, .data$week) %>% dplyr::slice(1)
        max_row <- time_df %>% dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$week)) %>% dplyr::slice(1)

        list(
          rows = rows,
          year_week_range = sprintf(
            "Y%s W%s to Y%s W%s",
            min_row$year[[1]], min_row$week[[1]],
            max_row$year[[1]], max_row$week[[1]]
          )
        )
      }, error = function(e) empty)
    }

    format_display_table <- function(df) {
      dt <- DT::datatable(
        df,
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searchHighlight = TRUE,
          dom = "Blrtip",
          buttons = list(
            list(
              extend = "colvis",
              text = "Show / hide columns"
            )
          ),
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          )
        ),
        rownames = FALSE
      )

      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      decimal_cols <- numeric_cols[vapply(numeric_cols, function(col) {
        values <- df[[col]]
        values <- values[!is.na(values)]
        if (length(values) == 0) return(FALSE)
        any(abs(values - round(values)) > .Machine$double.eps^0.5)
      }, logical(1))]

      if (length(decimal_cols) > 0) {
        dt <- DT::formatRound(dt, columns = decimal_cols, digits = display_digits())
      }

      dt
    }
  }, envir = state)
  invisible(state)
}
