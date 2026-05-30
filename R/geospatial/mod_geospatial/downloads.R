#' Register Geospatial module logic: downloads.R
#'
#' @param state Internal module environment created by `geospatialServer()`.
#' @return Invisibly returns `state`.
geospatial_server_downloads <- function(state) {
  evalq({
    output$download_prepared_data <- downloadHandler(
      filename = function() {
        paste0("geospatial_prepared_data_", input$geo_level, "_", input$selected_year, ".csv")
      },
      content = function(file) {
        readr::write_csv(prepared_geo_data(), file)
      }
    )

    output$download_results <- downloadHandler(
      filename = function() {
        paste0("geospatial_results_", input$geo_level, "_", input$selected_year, ".csv")
      },
      content = function(file) {
        raw_tbl <- prepare_results_table(analyzed_sf(), target_var = target_var)
        readr::write_csv(raw_tbl, file)
      }
    )
  }, envir = state)
  invisible(state)
}
