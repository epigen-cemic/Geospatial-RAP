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
        options = list(pageLength = 15, scrollX = TRUE),
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
