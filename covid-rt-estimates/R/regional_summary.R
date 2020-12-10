#' Regional Summary Output
#'
#' @description `r lifecycle::badge("maturing")`
#' Used to produce summary output either internally in `regional_epinow` or externally.
#' @param summary_dir A character string giving the directory
#'  in which to store summary of results.
#' @param target_date A character string giving the target date for which to extract results
#' (in the format "yyyy-mm-dd"). Defaults to latest available estimates.
#' @param all_regions Logical, defaults to `TRUE`. Should summary plots for all regions be returned 
#' rather than just regions of interest.
#' @return A list of summary measures and plots
#' @export
#' @seealso regional_epinow
#' @inheritParams summarise_results
#' @inheritParams plot_summary
#' @inheritParams summarise_key_measures
#' @inheritParams regional_epinow
#' @inheritParams get_regional_results
#' @inheritParams report_plots
#' @inheritParams epinow
#' @importFrom purrr map_chr compact
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave ggplot_build
#' @importFrom cowplot get_legend
#' @importFrom data.table setDT
#' @importFrom futile.logger flog.info
#' @examples
#' \donttest{
#' # example delays
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- estimate_delay(rlnorm(100, log(6), 1), max_value = 30)
#'                         
#' # example case vector from EpiSoon
#' cases <- example_confirmed[1:30]
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' # run basic nowcasting pipeline
#' out <- regional_epinow(reported_cases = cases,
#'                        generation_time = generation_time,
#'                        delays = delay_opts(incubation_period, reporting_delay),
#'                        output = "region",
#'                        rt = NULL)
#'
#' regional_summary(regional_output = out$regional,
#'                  reported_cases = cases)
#' } 
regional_summary <- function(regional_output = NULL,
                             reported_cases,
                             results_dir = NULL, 
                             summary_dir = NULL,
                             target_date = NULL,
                             region_scale = "Region",
                             all_regions = TRUE,
                             return_output = FALSE, 
                             max_plot = 10) {
    
  reported_cases <- data.table::setDT(reported_cases)
  
  if (is.null(summary_dir)) {
    futile.logger::flog.info("No summary directory specified so returning summary output")
    return_output <- TRUE
  }else{
    futile.logger::flog.info("Saving summary to : %s", summary_dir)
  }

  if (!is.null(results_dir) & !is.null(regional_output)) {
    stop("Only one of results_dir and regional_output should be specified")
  }
  
  if (is.null(regional_output)) {
    if (!is.null(results_dir)) {
     futile.logger::flog.info("Extracting results from: %s", results_dir)
     regions <- EpiNow2::get_regions(results_dir)
       if (is.null(target_date)) {
         target_date <- "latest"
       }
    }
  }else{
    regions <- names(regional_output)
    regional_output <- purrr::compact(regional_output)
  }
  
  futile.logger::flog.trace("Getting regional results")
  # get estimates
  results <- get_regional_results(regional_output,
                                  results_dir = results_dir,
                                  samples = FALSE,
                                  forecast = FALSE)
  
  # get latest date
  latest_date <- unique(reported_cases[confirm > 0][date == max(date)]$date)
  
  if (!is.null(summary_dir)) {
    # make summary directory
    if (!dir.exists(summary_dir)) {
      dir.create(summary_dir, recursive = TRUE)
    }
    saveRDS(latest_date, file.path(summary_dir, "latest_date.rds"))
    data.table::fwrite(reported_cases, file.path(summary_dir, "reported_cases.csv"))
  }
  
  if (!is.null(regional_output)) {
    regional_summaries <- purrr::map(regional_output, ~ .$summary)
  }else{
    regional_summaries <- NULL
  }
  futile.logger::flog.trace("Summarising results")
  
  # summarise results to csv
  sum_key_measures <- summarise_key_measures(regional_results = results,
                                             results_dir = results_dir, 
                                             summary_dir = summary_dir, 
                                             type = tolower(region_scale),
                                             date = target_date) 
  
  # summarise results as a table
  summarised_results <- summarise_results(regions, 
                                          summaries = regional_summaries,
                                          results_dir = results_dir,
                                          target_date = target_date,
                                          region_scale = region_scale)
  
  force_factor <- function(df) {
    df[,`Expected change in daily cases` :=
         factor(`Expected change in daily cases`,
                levels = c("Increasing", "Likely increasing", "Unsure", 
                           "Likely decreasing", "Decreasing"))] 
    
  }
  summarised_results$table <- force_factor(summarised_results$table)
  summarised_results$data <- force_factor(summarised_results$data)
  
  cases_reported_estimated <- copy(reported_cases)
  setnames(cases_reported_estimated, "confirm", "median")
  cases_reported_estimated$type <- "reported"
  cases_reported_estimated <- rbind(cases_reported_estimated, sum_key_measures$cases_by_report,
				    fill = TRUE) 
  setorderv(cases_reported_estimated, cols = c("region", "date"))
  cases_reported_estimated[startsWith(type, "estimate"), type := "estimate"]
  
  if (!is.null(summary_dir)) {
    data.table::fwrite(summarised_results$table, file.path(summary_dir, "summary_table.csv"))
    data.table::fwrite(summarised_results$data,  file.path(summary_dir, "summary_data.csv"))
    data.table::fwrite(cases_reported_estimated, file.path(summary_dir, "cases_reported_estimated.csv"))
  }

  # adaptive add a logscale to the summary plot based on range of observed cases
  current_inf <- summarised_results$data[metric %in% "New confirmed cases by infection date"]
  uppers <- grepl("upper_", colnames(current_inf))
  lowers <- grepl("lower_", colnames(current_inf))
  log_cases <- (max(current_inf[,..uppers], na.rm = TRUE) / 
                  min(current_inf[, ..lowers], na.rm = TRUE)) > 1000

  max_reported_cases <- round(max(reported_cases$confirm, na.rm = TRUE) * max_plot, 0)
  
  # summarise cases and Rts
  summary_plot <- plot_summary(summarised_results$data,
                               x_lab = region_scale, 
                               log_cases = log_cases,
                               max_cases = max_reported_cases)
  
  if (!is.null(summary_dir)) {
    save_ggplot <- function(plot, name, height = 12, width = 12, ...) {
      suppressWarnings(
        suppressMessages(
          ggplot2::ggsave(file.path(summary_dir, name),
                          plot, dpi = 300, width = width, 
                          height = height, ...)
        ))
    }
    save_ggplot(summary_plot, "summary_plot.png",
                width = ifelse(length(regions) > 60, 
                               ifelse(length(regions) > 120, 36, 24),
                               12))
  }
  # extract regions with highest number of reported cases in the last week
  most_reports <- get_regions_with_most_reports(reported_cases, 
                                                time_window = 7, 
                                                no_regions = 6)
  
  high_plots <- report_plots(
    summarised_estimates = results$estimates$summarised[region %in% most_reports], 
    reported = reported_cases[region %in% most_reports],
    max_plot = max_plot
  )
  
  high_plots$summary <- NULL
  high_plots <- 
    purrr::map(high_plots, ~ . + ggplot2::facet_wrap(~ region, scales = "free_y", ncol = 2))
  
  if (!is.null(summary_dir)) {
    save_ggplot(high_plots$R, "high_rt_plot.png")
    save_ggplot(high_plots$infections, "high_infections_plot.png")
    save_ggplot(high_plots$reports, "high_reported_cases_plot.png")
  }
  
  if (all_regions) {
    plots_per_row <- ifelse(length(regions) > 60, 
                            ifelse(length(regions) > 120, 8, 5), 3)
    
    plots <- report_plots(summarised_estimates = results$estimates$summarised, 
                          reported = reported_cases,
                          max_plot = max_plot)
    
    plots$summary <- NULL
    plots <- purrr::map(plots,~ . + ggplot2::facet_wrap(~ region, scales = "free_y",
                                                        ncol = plots_per_row))
    
    if (!is.null(summary_dir)) {
      save_big_ggplot <- function(plot, name){
        save_ggplot(plot, name, 
                    height =  3 * round(length(regions) / plots_per_row, 0),
                    width = 24, 
                    limitsize = FALSE)
      }
      save_big_ggplot(plots$R, "rt_plot.png")
      save_big_ggplot(plots$infections, "infections_plot.png")
      save_big_ggplot(plots$reports, "reported_cases_plot.png")
    }
  }
  if (return_output) {
    out <- list()
    out$latest_date <- latest_date
    out$results <- results
    out$summarised_results <- summarised_results
    out$summary_plot <- summary_plot
    out$summarised_measures <- sum_key_measures
    out$reported_cases <- reported_cases
    out$high_plots <- high_plots
    
    if (all_regions) {
      out$plots <- plots
    }
    return(out)
  }else{
    return(invisible(NULL))
  }
}

#' Summarise rt and cases
#'
#' @description `r lifecycle::badge("maturing")`
#' Produces summarised data frames of output across regions. Used internally by `regional_summary`.
#' @param regional_results A list of dataframes as produced by `get_regional_results`
#' @param results_dir Character string indicating the directory from which to extract results.
#' @param summary_dir Character string the directory into which to save results as a csv.
#' @param type Character string, the region identifier to apply (defaults to region).
#' @inheritParams get_regional_results
#' @seealso regional_summary
#' @return A list of summarised Rt, cases by date of infection and cases by date of report
#' @export
#' @importFrom data.table setnames fwrite setorderv
summarise_key_measures <- function(regional_results = NULL,
                                   results_dir = NULL, summary_dir = NULL,
                                   type = "region", date = "latest") {

  futile.logger::flog.info(paste("Results dir", results_dir, "Summary dir", summary_dir, "Date", date))
  if (is.null(regional_results)) {
    if (is.null(results_dir)) {
      stop("Missing results directory")
    }
    timeseries <- EpiNow2::get_regional_results(results_dir = results_dir,
                                                date = date, forecast = FALSE,
                                                samples = FALSE)
  }else{
    timeseries <- regional_results
  }
  summarise_variable <- function(df, dof = 0) {
    cols <- setdiff(names(df), c("region", "date", "type", "strat"))
    df[,(cols) := round(.SD, dof), .SDcols = cols]
    #data.table::setorderv(df, cols = cols)
    data.table::setorderv(df, cols = c("region", "date"))
    data.table::setnames(df, "region", type)
    return(df)
  }

  save_variable <- function(df, name) {
      if (!is.null(summary_dir)) {
        data.table::fwrite(df, paste0(summary_dir, "/", name, ".csv"))
      }
  }
  out <- list()
  sum_est <- timeseries$estimates$summarised
  # clean and save Rt estimates
  out$rt <- summarise_variable(sum_est[variable == "R"][, variable := NULL], 4)
  out$rt[startsWith(type, "estimate"), type := "estimate"]
  #setorder(out$rt, date, region)
  save_variable(out$rt, "rt")

  # clean and save growth rate estimates
  out$growth_rate <- summarise_variable(sum_est[variable == "growth_rate"][,
                                                variable := NULL], 3)
  #setorder(out$growth_rate, date, region)
  save_variable(out$growth_rate, "growth_rate")

  # clean and save case estimates
  out$cases_by_infection <- summarise_variable(sum_est[variable == "infections"][,
                                                       variable := NULL], 0)
  #setorder(out$cases_by_infection, date, region)
  save_variable(out$cases_by_infection, "cases_by_infection")

  # clean and save case estimates
  #setorder(out$cases_by_report, date, region)
  out$cases_by_report <- summarise_variable(sum_est[variable == "reported_cases"][,
                                                    variable := NULL], 0)
  save_variable(out$cases_by_report, "cases_by_report")

  return(out)
}

