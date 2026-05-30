#' Register Geospatial module logic: uploads.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_uploads <- function(state) {
  evalq({
    active_uploaded_files <- reactive({
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)
      files <- files[isTRUE(files$active) | files$active, , drop = FALSE]
      req(nrow(files) > 0)
      files
    })

    active_uploaded_file_paths <- reactive({
      files <- active_uploaded_files()
      setNames(files$datapath, seq_along(files$datapath))
    })

    observeEvent(input$file_upload, {
      req(input$file_upload)

      uploaded <- input$file_upload
      upload_country <- isolate({
        if (length(active_countries) > 1 && !is.null(input$active_country)) input$active_country else active_country
      })

      metadata <- lapply(uploaded$datapath, geospatial_file_metadata, country = upload_country)
      rows <- vapply(metadata, function(x) if (is.null(x$rows)) NA_integer_ else as.integer(x$rows), integer(1))
      ranges <- vapply(metadata, function(x) if (is.null(x$year_week_range)) "Not available" else as.character(x$year_week_range), character(1))
      modified <- file.info(uploaded$datapath)$mtime

      new_files <- data.frame(
        id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", seq_len(nrow(uploaded)), "_", make.names(uploaded$name)),
        name = uploaded$name,
        datapath = uploaded$datapath,
        size = uploaded$size,
        rows = rows,
        year_week_range = ranges,
        last_modified = as.character(modified),
        uploaded_at = as.character(Sys.time()),
        active = TRUE,
        stringsAsFactors = FALSE
      )

      existing <- uploaded_file_state()
      if (is.null(existing)) {
        uploaded_file_state(new_files)
      } else {
        uploaded_file_state(dplyr::bind_rows(existing, new_files))
      }
    }, ignoreInit = TRUE)

    output$uploaded_files_ui <- renderUI({
      files <- uploaded_file_state()
      uploaded_count <- if (is.null(files)) 0 else nrow(files)
      active_count <- if (is.null(files)) 0 else sum(files$active, na.rm = TRUE)

      tagList(
        div(
          class = "uploaded-files-summary",
          div(
            class = "uploaded-files-summary-row",
            span(class = "uploaded-files-label", "Uploaded files"),
            span(class = "uploaded-files-count", uploaded_count)
          ),
          div(
            class = "uploaded-files-summary-row",
            span(class = "uploaded-files-label muted", "Active files"),
            span(class = "uploaded-files-count active", active_count)
          )
        ),
        if (!is.null(files) && nrow(files) > 0) {
          actionButton(ns("manage_uploaded_files"), "Manage files", class = "dashboard-small-button")
        }
      )
    })

    observeEvent(input$manage_uploaded_files, {
      files <- uploaded_file_state()

      if (is.null(files) || nrow(files) == 0) {
        showModal(modalDialog(
          title = "Uploaded files",
          p("No files have been uploaded."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }

      table_rows <- lapply(seq_len(nrow(files)), function(i) {
        tags$tr(
          tags$td(
            checkboxInput(
              ns(paste0("file_active_", files$id[i])),
              label = NULL,
              value = isTRUE(files$active[i]),
              width = "20px"
            )
          ),
          tags$td(files$name[i]),
          tags$td(ifelse(is.na(files$rows[i]), "", files$rows[i])),
          tags$td(files$year_week_range[i]),
          tags$td(format_uploaded_timestamp(files$last_modified[i])),
          tags$td(format_uploaded_file_size(files$size[i]))
        )
      })

      showModal(modalDialog(
        title = "Uploaded files",
        size = "l",
        easyClose = TRUE,
        tags$p("Uncheck files that should not be included in the analysis. Use 'Remove inactive files' to remove unchecked files from this session."),
        div(
          class = "uploaded-files-table-wrapper",
          tags$table(
            class = "uploaded-files-table",
            tags$thead(tags$tr(
              tags$th("Active"),
              tags$th("File name"),
              tags$th("Rows"),
              tags$th("Year-week range"),
              tags$th("Last modified"),
              tags$th("Size")
            )),
            tags$tbody(table_rows)
          )
        ),
        footer = tagList(
          actionButton(ns("apply_file_selection"), "Apply selection", class = "dashboard-primary-button"),
          actionButton(ns("remove_inactive_files"), "Remove inactive files", class = "dashboard-danger-button"),
          actionButton(ns("clear_all_files_modal"), "Clear all files", class = "dashboard-danger-button"),
          modalButton("Close")
        )
      ))
    })

    observeEvent(input$apply_file_selection, {
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)

      files$active <- vapply(
        files$id,
        function(id) isTRUE(input[[paste0("file_active_", id)]]),
        logical(1)
      )

      uploaded_file_state(files)
      removeModal()
      showNotification(paste(sum(files$active), "active file(s) selected."), type = "message")
    })

    observeEvent(input$remove_inactive_files, {
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)

      files$active <- vapply(
        files$id,
        function(id) isTRUE(input[[paste0("file_active_", id)]]),
        logical(1)
      )
      files <- files[files$active, , drop = FALSE]

      if (nrow(files) == 0) {
        uploaded_file_state(NULL)
      } else {
        uploaded_file_state(files)
      }

      removeModal()
      showNotification("Inactive files were removed.", type = "message")
    })

    observeEvent(input$clear_all_files_modal, {
      uploaded_file_state(NULL)
      prepared_geo_data(NULL)
      prepared_geo_log(character(0))
      removeModal()
      showNotification("All uploaded files were removed.", type = "message")
    })
  }, envir = state)
  invisible(state)
}
