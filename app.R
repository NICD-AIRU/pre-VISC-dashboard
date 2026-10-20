## ============================================================================
##  global.R  -  Bio-Plex / Luminex RAW export -> long-format BAMA table
##  Processing logic ported 1:1 from report_michaela_7.R (version 2026-10-06f)
##  so the dashboard and the command-line script give identical tables.
## ============================================================================

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

SCRIPT_VERSION <- "2026-10-06f-ordered-cols (dashboard)"

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

## ---- all 95 export columns (same list as the script) -------------------------
BAMA_COLS <- c(
  "analysis_software",
  "analyte",
  "analyte_lot",
  "area_under_curve",
  "assay_platform",
  "avidity_index",
  "avidity_index_calculated",
  "avidity_index_source",
  "avidity_index_treatment_buffer",
  "avidity_index_treatment_buffer_ph",
  "bama_fits_transform_version",
  "bama_positivity_transform_version",
  "bama_processing_transform_version",
  "baseline_visit",
  "bead_count",
  "bead_type",
  "coefficient_of_variation",
  "coefficient_of_variation_source",
  "coefficient_of_variation_type",
  "concentration_method",
  "concentration_percent_recovery",
  "concentration_standard",
  "concentration_standard_extinction_coefficient",
  "concentration_units",
  "curve_location",
  "dataset_creation_date",
  "dataset_version",
  "description",
  "dilution",
  "dilution_selection_method",
  "dilution_selection_performed",
  "exclude",
  "expected_concentration",
  "experiment_date",
  "experiment_identifier",
  "first_detection",
  "fluorescence_intensity",
  "fluorescence_intensity_background_negative_subtracted",
  "fluorescence_intensity_background_subtracted",
  "global_identifier",
  "half_maximal_effective_concentration",
  "isotype",
  "lab",
  "linear_range_max",
  "linear_range_min",
  "linear_range_units",
  "lower_limit_of_detection",
  "lower_limit_of_detection_units",
  "lower_limit_of_quantitation",
  "lower_limit_of_quantitation_units",
  "machine_serial_number",
  "negative_bead",
  "network",
  "observed_concentration",
  "plate_identifier",
  "plate_type",
  "positivity",
  "positivity_calculated",
  "positivity_fold_change",
  "positivity_method",
  "positivity_threshold",
  "positivity_threshold_method",
  "record_identifier",
  "ruminex_standard",
  "ruminex_version",
  "run_identifier",
  "sample_contamination",
  "sample_date_collected",
  "sample_date_processed",
  "sample_date_received",
  "sample_identifier",
  "sample_role",
  "sample_species",
  "sample_type",
  "sampling_timepoint",
  "sampling_timepoint_reference",
  "sampling_timepoint_reference_product",
  "sampling_timepoint_units",
  "second_detection",
  "source_file_short",
  "standard_deviation",
  "standard_deviation_source",
  "standard_deviation_type",
  "study",
  "summary",
  "summary_method",
  "summary_type",
  "third_detection",
  "timepoint",
  "timepoint_baseline",
  "timepoint_units",
  "titration_identifier",
  "visit_identifier",
  "well",
  "well_group"
)

## ---- constants written to every row (editable in the dashboard) --------------
DEFAULT_CONSTS <- list(
  analysis_software = "Bio-plex Manager Software", assay_platform = "BAMA",
  avidity_index_calculated = FALSE, exclude = FALSE, bead_type = "MAGNETIC",
  dilution = 50, expected_concentration = 1,
  first_detection = "DIRECT IgG-PE", global_identifier = "guspec", isotype = "IgG",
  lab = "AIRU", network = "CAVD", plate_type = 96L, sample_role = "SAMPLE",
  sample_species = "HUMAN", sample_type = "SER", study = "301, Moore_989",
  timepoint_baseline = FALSE, timepoint_units = "Week")

## columns that are filled from the RAW file (everything else is left empty)
DERIVED_COLS <- c("analyte", "baseline_visit", "bead_count", "dataset_creation_date", "experiment_date",
                  "experiment_identifier", "fluorescence_intensity",
                  "fluorescence_intensity_background_subtracted", "machine_serial_number",
                  "negative_bead", "plate_identifier", "sample_identifier", "summary",
                  "timepoint", "visit_identifier", "well", "well_group")

## Column order used in the reviewed example export: populated columns first
## (A-Z), then the columns that are not derivable from the RAW file (A-Z).
ordered_cols <- function(consts = DEFAULT_CONSTS) {
  filled <- sort(union(names(consts), DERIVED_COLS))
  c(filled, sort(setdiff(BAMA_COLS, filled)))
}

## Export column order: all columns A-Z (C-locale sort, so the order is identical on every machine)
alpha_cols <- function(df) df[, sort(names(df), method = "radix"), drop = FALSE]

TRAILING_COLS <- c("Region", "% Agg Beads", "Sampling Errors", "Plate ID", "Bead Count", "Acquisition Time")
strip_region  <- function(x) str_trim(str_remove(x, "\\s*\\(\\d+\\)\\s*$"))

read_raw_all <- function(raw_path) {
  suppressMessages(read_excel(raw_path, col_names = FALSE, col_types = "text"))
}

## first valid YYYYMMDD (19xx / 20xx, not part of a longer number) found in a piece of text
date_from_text <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))
  for (s in str_extract_all(x, "(?<![0-9])(?:19|20)[0-9]{6}(?![0-9])")[[1]]) {
    d <- as.Date(s, format = "%Y%m%d")
    if (!is.na(d)) return(d)
  }
  as.Date(NA)
}
## "Acquisition Date: 05-Aug-2026, 02:51 PM" -> 2026-08-05 (English month names, independent of the PC's locale)
date_from_acq <- function(x) {
  m <- str_match(as.character(x)[1], "(\\d{1,2})-([A-Za-z]{3})-(\\d{4})")
  mon <- match(tolower(m[, 3]), tolower(month.abb))
  if (is.na(mon)) return(as.Date(NA))
  as.Date(sprintf("%s-%02d-%02d", m[, 4], mon, as.integer(m[, 2])))
}

read_header_block <- function(raw_path) {
  top <- suppressMessages(read_excel(raw_path, col_names = FALSE, col_types = "text", n_max = 12))[[1]]
  grab <- function(key) str_trim(str_remove(top[str_detect(top, fixed(key))][1], fixed(key)))
  file_name_line <- grab("File Name:")
  plate_full     <- grab("Plate ID:")
  ## experiment_date: the date in the File Name (normal case); if that is missing / has no date,
  ## fall back to the Plate ID, then to the Acquisition Date line, and record which one was used
  cands <- list(`File Name`        = date_from_text(basename(str_replace_all(file_name_line, "\\\\", "/"))),
                `Plate ID`         = date_from_text(plate_full),
                `Acquisition Date` = date_from_acq(grab("Acquisition Date:")))
  ok <- which(!vapply(cands, is.na, logical(1)))
  list(
    experiment_date = if (length(ok)) cands[[ok[1]]] else as.Date(NA),
    date_source = if (length(ok)) names(cands)[ok[1]] else NA_character_,
    machine_serial_number = grab("Reader Serial Number:"),
    plate_full = plate_full)
}

## header block + analyte list of one RAW file (used for the upload preview)
inspect_raw <- function(raw_path) {
  h <- read_header_block(raw_path)
  all <- read_raw_all(raw_path)
  hdr_row <- which(all[[1]] == "Well")[1]
  if (is.na(hdr_row)) stop("no 'Well' header row found - is this a Bio-Plex RAW export?")
  hdr <- as.character(unlist(all[hdr_row, ]))
  end_idx <- which(str_trim(hdr) %in% TRAILING_COLS & seq_along(hdr) > 3)[1]
  if (is.na(end_idx)) stop("could not find the end of the analyte columns (looked for: ",
                           paste(TRAILING_COLS, collapse = ", "), ")")
  h$analytes <- strip_region(hdr[4:(end_idx - 1)])
  h$n_wells  <- sum(!is.na(all[(hdr_row + 1):nrow(all), ][[1]]))
  h
}

## ---- process ONE raw file -> data.frame with the final columns ---------------
process_raw <- function(raw_path, analytes, consts = DEFAULT_CONSTS,
                        source_name = basename(raw_path), log = function(...) NULL) {
  raw_path <- normalizePath(raw_path, mustWork = TRUE)

  h <- read_header_block(raw_path)
  experiment_date       <- h$experiment_date
  if (is.na(experiment_date)) warning("experiment_date could not be found in ", source_name, " (no date in File Name, Plate ID or Acquisition Date)")
  else if (!identical(h$date_source, "File Name")) warning("experiment_date for ", source_name, " was taken from the ", h$date_source, " (no date in the RAW File Name)")
  machine_serial_number <- h$machine_serial_number
  plate_full            <- h$plate_full
  plate_identifier      <- str_split(plate_full, "_")[[1]][3]      # e.g. P1

  ## data table
  all <- read_raw_all(raw_path)
  hdr_row <- which(all[[1]] == "Well")[1]
  hdr <- as.character(unlist(all[hdr_row, ]))
  dat <- all[(hdr_row + 1):nrow(all), ]
  colnames(dat) <- hdr
  dat <- dat[!is.na(dat$Well), ]
  dat$.row <- seq_len(nrow(dat))          # original plate/acquisition order, used as tie-breaker

  ## analyte columns run from column 4 up to the first non-analyte trailing column
  end_idx <- which(str_trim(hdr) %in% TRAILING_COLS & seq_along(hdr) > 3)[1]
  if (is.na(end_idx)) stop("could not find the end of the analyte columns (looked for: ",
                           paste(TRAILING_COLS, collapse = ", "), ")")
  analyte_cols <- hdr[4:(end_idx - 1)]
  clean_name <- strip_region(analyte_cols)
  is_blank_col <- tolower(clean_name) == "blank"
  chosen <- tolower(clean_name) %in% tolower(analytes) & !is_blank_col        # analytes ticked by the user
  keep <- analyte_cols[chosen | is_blank_col]                                  # ticked analytes + BLANK
  if (!any(chosen)) return(NULL)
  if (!any(is_blank_col)) warning("No BLANK analyte found in ", source_name, "; only the selected analytes were exported")
  log("  Analytes kept (file order): ", paste(keep, collapse = " | "))

  ## summary: wells sharing the same Type are repeats; ordered by Well (A1 before A2 ...)
  ## first well = "individual", later wells with the same Type = "replicate"
  dat <- dat %>%
    mutate(.wcol = as.integer(str_extract(Well, "\\d+")), .wrow = str_extract(Well, "^[A-Za-z]+")) %>%
    arrange(.wcol, .wrow) %>%
    group_by(Type) %>%
    mutate(summary = ifelse(is.na(Type), NA_character_, ifelse(row_number() == 1, "individual", "replicate"))) %>%
    ungroup()

  long <- dat %>%
    select(Well, Type, Description, .row, summary, all_of(keep)) %>%
    pivot_longer(all_of(keep), names_to = "analyte_raw", values_to = "cell") %>%
    mutate(
      analyte = strip_region(analyte_raw),
      negative_bead = tolower(analyte) == "blank",                    # TRUE for the BLANK analyte, FALSE otherwise
      analyte = factor(analyte, levels = unique(analyte)),            # keep file order
      fluorescence_intensity = as.numeric(str_replace(str_extract(cell, "^[-0-9.,]+"), ",", ".")),
      bead_count = as.integer(str_match(cell, "\\((\\d+)\\)")[, 2]),
      Description = ifelse(is.na(Description), "", Description),
      sample_identifier = ifelse(Description == "", NA, str_split_fixed(Description, "_", 3)[, 1]),
      visit_identifier  = ifelse(str_detect(Description, "_"), str_split_fixed(Description, "_", 3)[, 2], NA),
      experiment_identifier = suppressWarnings(as.integer(str_replace_all(visit_identifier, "\\D", "")))
    )

  ## background subtraction: per analyte, average the Type "B" wells, subtract from every reading
  ## of the same analyte; negative results are kept as-is
  long <- long %>%
    group_by(analyte) %>%
    mutate(.bg = mean(fluorescence_intensity[!is.na(Type) & Type == "B"], na.rm = TRUE),
           .bg = ifelse(is.nan(.bg), NA_real_, .bg),
           fluorescence_intensity_background_subtracted = fluorescence_intensity - .bg) %>%
    ungroup()
  if (any(is.na(long$.bg)))
    warning("No Type 'B' wells found for some analyte(s) in ", source_name,
            "; background-subtracted values left empty")
  log("  B-well averages: ", paste(unique(paste0(long$analyte, "=", long$.bg)), collapse = " | "))

  out <- long %>%
    mutate(experiment_date = experiment_date,
           machine_serial_number = machine_serial_number,
           plate_identifier = plate_identifier,
           well = Well, well_group = Type,
           analyte = as.character(analyte)) %>%
    arrange(factor(analyte, levels = unique(analyte)), sample_identifier, experiment_identifier, .row) %>%
    as.data.frame()

  out$experiment_identifier <- plate_full      # full "Plate ID:" from the RAW header
  out$timepoint <- NA                          # set from sample_role in the dashboard (apply_timepoint)
  out$dataset_creation_date <- Sys.Date()      # date the export is created
  for (n in names(consts)) out[[n]] <- consts[[n]]
  for (m in setdiff(BAMA_COLS, names(out))) out[[m]] <- NA
  out[, ordered_cols(consts)]
}

## flag baseline visits: timepoint_baseline = TRUE where visit_identifier is one of the chosen visits
apply_baseline <- function(df, baseline_visits) {
  bv <- toupper(str_trim(as.character(baseline_visits %||% character())))
  bv <- bv[bv != ""]
  df$timepoint_baseline <- !is.na(df$visit_identifier) & toupper(df$visit_identifier) %in% bv
  df
}

## ---- shipment metadata: PID/ Sample name + Visit number  ->  Other Spec ID ----------
read_shipment <- function(path) {
  x  <- suppressMessages(read_excel(path, col_types = "text"))
  nm <- gsub("[^a-z0-9]", "", tolower(names(x)))
  want <- c(sample = "pidsamplename", spec = "otherspecid", visit = "visitnumber")
  idx  <- vapply(want, function(k) which(nm == k)[1], 1L)
  if (anyNA(idx))
    stop("metadata file is missing column(s): ",
         paste(c("PID/ Sample name", "Other Spec ID", "Visit number")[is.na(idx)], collapse = ", "))
  out <- data.frame(sample = str_trim(x[[idx["sample"]]]),
                    spec   = str_trim(x[[idx["spec"]]]),
                    visit  = str_extract(str_trim(x[[idx["visit"]]]), "\\d+"),
                    stringsAsFactors = FALSE)
  out <- out[!is.na(out$sample) & out$sample != "" & !is.na(out$visit) & !is.na(out$spec) & out$spec != "", ]
  out$visit <- as.character(as.integer(out$visit))                 # "01" / "1.0" -> "1"
  dup <- duplicated(paste(toupper(out$sample), out$visit))
  if (any(dup)) warning(sum(dup), " duplicate PID/visit row(s) in the metadata; first Other Spec ID kept")
  out[!dup, ]
}

## match key from the RAW Description parts: sample_identifier + the NUMBER of visit_identifier (V104 -> 104)
metadata_key <- function(sample, visit_identifier) {
  v <- suppressWarnings(as.character(as.integer(str_extract(visit_identifier, "\\d+"))))
  ifelse(is.na(sample) | is.na(v), NA_character_, paste(toupper(str_trim(sample)), v, sep = "|"))
}

## global_identifier <- Other Spec ID where sample name AND visit number both match
apply_metadata <- function(df, meta) {
  if (is.null(meta) || nrow(meta) == 0) return(df)
  m <- match(metadata_key(df$sample_identifier, df$visit_identifier), paste(toupper(meta$sample), meta$visit, sep = "|"))
  hit <- !is.na(m)
  df$global_identifier[hit] <- meta$spec[m[hit]]
  df
}

## one row per sample x visit found in the RAW files, with the match result
metadata_match <- function(df, meta) {
  p <- unique(df[!is.na(df$sample_identifier), c("sample_identifier", "visit_identifier")])
  if (nrow(p) == 0) return(data.frame())
  m <- if (is.null(meta)) rep(NA_integer_, nrow(p)) else
    match(metadata_key(p$sample_identifier, p$visit_identifier), paste(toupper(meta$sample), meta$visit, sep = "|"))
  data.frame(Sample = p$sample_identifier, Visit = p$visit_identifier,
             Matched = !is.na(m), `Other Spec ID` = if (is.null(meta)) NA_character_ else meta$spec[m],
             check.names = FALSE, stringsAsFactors = FALSE)
}

## sample_role from the RAW Type: C* -> control role (CONTROL_MAB or CONTROL), X* -> SAMPLE, B -> BLANK
## (any other Type is left empty). Based on the WELL's Type, so BLANK-analyte rows follow their well too.
apply_sample_role <- function(df, control_role = "CONTROL_MAB") {
  t <- toupper(str_trim(as.character(df$well_group)))
  df$sample_role <- ifelse(grepl("^C\\d*$", t), control_role,
                    ifelse(grepl("^X\\d*$", t), "SAMPLE",
                    ifelse(grepl("^B\\d*$", t), "BLANK", NA_character_)))
  df
}

## sample_type from the RAW Type: X* -> SER; C* / B* (and anything else) -> empty.
## Based on the WELL's Type, so BLANK-analyte rows follow their well too.
apply_sample_type <- function(df) {
  t <- toupper(str_trim(as.character(df$well_group)))
  df$sample_type <- ifelse(grepl("^X\\d*$", t), "SER", NA_character_)
  df
}

## roles that carry no sample information
NO_INFO_ROLES <- c("BLANK", "CONTROL_MAB", "CONTROL")     # BLANK + controls: no global_identifier / baseline_visit
INFO_ROLES    <- c("SAMPLE", "CONTROL_MAB", "CONTROL")    # roles that get timepoint = TRUE

## global_identifier: left empty for BLANK and CONTROL wells (everything else keeps "guspec" / the metadata Other Spec ID)
apply_global_identifier <- function(df) {
  df$global_identifier[df$sample_role %in% NO_INFO_ROLES] <- NA_character_
  df
}

## expected_concentration: 1 on CONTROL / CONTROL_MAB wells only; empty for samples, BLANK wells,
## unknown wells and the Blank analyte (negative_bead = TRUE)
apply_expected_concentration <- function(df) {
  ctrl <- df$sample_role %in% c("CONTROL_MAB", "CONTROL") & !(df$negative_bead %in% TRUE)
  df$expected_concentration <- ifelse(ctrl, 1, NA_real_)
  df
}

## baseline_visit: TRUE on the chosen baseline visit(s), FALSE on every other visit,
## empty for BLANK / CONTROL wells and for rows without a visit_identifier
apply_baseline_visit <- function(df, baseline_visits) {
  bv <- toupper(str_trim(as.character(baseline_visits %||% character())))
  bv <- bv[bv != ""]
  v  <- toupper(str_trim(as.character(df$visit_identifier)))
  eligible <- !is.na(df$visit_identifier) & nzchar(v) & !(df$sample_role %in% NO_INFO_ROLES)
  df$baseline_visit <- ifelse(eligible, v %in% bv, NA)
  df
}

## timepoint: pre-set TRUE where sample_role is SAMPLE / CONTROL_MAB / CONTROL; empty for BLANK and unknown
apply_timepoint <- function(df) {
  df$timepoint <- ifelse(df$sample_role %in% INFO_ROLES, TRUE, NA)
  df
}

## summary: "samples" keeps individual / replicate (from the Type column); "boolean" sets every row to TRUE or FALSE
apply_summary <- function(df, mode = "samples", bool = "TRUE") {
  if (identical(mode, "boolean")) df$summary <- identical(toupper(as.character(bool)), "TRUE")
  df
}

## sample-level value assignments (isotype only; exclude is always FALSE).
## rules = list(list(column=, value=, samples=<sample_identifier vector>, plates=<experiment_identifier vector>), ...);
## applied in order, so a later rule overrides an earlier one for the same sample.
apply_rules <- function(df, rules) {
  for (r in rules) {
    on_plate <- if (is.null(r$plates)) TRUE else df$experiment_identifier %in% r$plates     # NULL = all plates
    idx <- !is.na(df$sample_identifier) & df$sample_identifier %in% r$samples & on_plate
    if (!any(idx)) next
    val <- r$value
    if (r$column == "exclude") val <- as.logical(val)
    df[[r$column]][idx] <- val
  }
  df
}

## ============================================================================
##  Bio-Plex BAMA Dashboard  (styled after the NAb Assay Dashboard)
##  Upload Bio-Plex RAW .xlsx files -> tick analytes -> Run -> export table
## ============================================================================
suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(DT)
  library(openxlsx)
  library(plotly)
})

options(shiny.maxRequestSize = 500 * 1024^2)   # allow large RAW batches

DATE_COLS <- c("experiment_date", "dataset_creation_date")

## ---- write the export exactly like the script does (xlsx + tab-delimited txt) --
write_export_xlsx <- function(final, path) {
  final <- alpha_cols(final)
  wb <- createWorkbook()
  addWorksheet(wb, "export")
  writeData(wb, "export", final)
  addStyle(wb, "export", createStyle(numFmt = "yyyy-mm-dd"),
           cols = which(names(final) %in% DATE_COLS), rows = 2:(nrow(final) + 1), gridExpand = TRUE)
  freezePane(wb, "export", firstRow = TRUE)
  saveWorkbook(wb, path, overwrite = TRUE)
}
write_export_txt <- function(final, path) {
  txt <- alpha_cols(final)
  for (d in intersect(DATE_COLS, names(txt))) txt[[d]] <- format(txt[[d]], "%Y-%m-%d")
  write.table(txt, path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
}

## ---- plate map (plotly heatmap) -------------------------------------------------------
## metric = MFI / background-subtracted MFI: continuous heat colours, black x on wells below the bead threshold
## metric = bead_count: RED if bead count < min_beads, GREEN otherwise, grey = no value
make_plate_map <- function(f, metric, min_beads) {
  f$r <- toupper(sub("\\d+$", "", f$well))
  f$c <- suppressWarnings(as.integer(sub("^[A-Za-z]+", "", f$well)))
  rows <- LETTERS[1:8]; cols <- 1:12
  val <- matrix(NA_real_, 8, 12, dimnames = list(rows, cols))
  hov <- matrix("", 8, 12, dimnames = list(rows, cols))
  nz  <- function(x) ifelse(is.na(x), "-", as.character(x))
  for (i in seq_len(nrow(f))) {
    ri <- match(f$r[i], rows); ci <- f$c[i]
    if (is.na(ri) || is.na(ci) || !(ci %in% cols)) next
    val[ri, ci] <- suppressWarnings(as.numeric(f[[metric]][i]))
    hov[ri, ci] <- paste0(f$well[i], " | type ", nz(f$well_group[i]), "<br>", nz(f$sample_identifier[i]), " ", nz(f$visit_identifier[i]),
                          "<br>MFI ", nz(f$fluorescence_intensity[i]), " | beads ", nz(f$bead_count[i]))
  }
  validate(need(any(!is.na(val)),
                "No values for this metric on this plate/analyte (for bead count: the RAW file has no bead count for these wells)."))
  ax <- list(yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "", side = "top", type = "category"))

  if (identical(metric, "bead_count")) {
    z <- val; z[] <- ifelse(is.na(val), NA_real_, ifelse(val < min_beads, 0, 1))      # 0 = below (red), 1 = ok (green)
    hov[!is.na(val)] <- paste0(hov[!is.na(val)], ifelse(val[!is.na(val)] < min_beads, "<br><b>BELOW threshold</b>", "<br>OK"))
    return(plot_ly(x = as.character(cols), y = rows, z = z, text = hov, type = "heatmap",
                   hovertemplate = "%{text}<extra></extra>", zmin = 0, zmax = 1, showscale = FALSE,
                   colorscale = list(c(0, "#d9534f"), c(0.5, "#d9534f"), c(0.5, "#5cb85c"), c(1, "#5cb85c")),
                   xgap = 2, ygap = 2) %>% layout(yaxis = ax$yaxis, xaxis = ax$xaxis))
  }

  p <- plot_ly(x = as.character(cols), y = rows, z = val, text = hov, type = "heatmap",
               hovertemplate = "%{text}<extra></extra>",
               colorscale = list(c(0, "lightgrey"), c(0.4, "orange"), c(0.7, "red"), c(1, "darkred")),
               xgap = 2, ygap = 2) %>% layout(yaxis = ax$yaxis, xaxis = ax$xaxis)
  low <- which(!is.na(f$bead_count) & f$bead_count < min_beads & !is.na(f$c) & !is.na(match(f$r, rows)))
  if (length(low))
    p <- p %>% add_markers(x = as.character(f$c[low]), y = f$r[low], inherit = FALSE,
                           marker = list(symbol = "x", size = 9, color = "black"),
                           name = paste0("beads < ", min_beads), hoverinfo = "skip")
  p
}
## ---- end make_plate_map ------------------------------------------------------------------

## ═════════════════════════════════════════════════════════════════════════════
##  UI
## ═════════════════════════════════════════════════════════════════════════════
logo_tag <- if (file.exists("www/airu_logo.png"))
  tags$img(src = "airu_logo.png", height = "38px", style = "margin-right:8px; vertical-align:middle;") else NULL

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(logo_tag, "Bio-Plex BAMA Dashboard"),
    titleWidth = 340
  ),

  dashboardSidebar(
    width = 270,
    useShinyjs(),
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo { background-color:#1a3a6b; }
      .skin-blue .main-header .navbar { background-color:#1a3a6b; }
      .skin-blue .main-sidebar { background-color:#1e2d40; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a { background:#2c4a7c; border-left:3px solid #F0C419; }
      .box.box-primary { border-top-color:#1a3a6b; }
      .box-header { background:#f7f9fc; }
      .shiny-notification { top:60px; right:10px; }
      .main-sidebar .selectize-input, .main-sidebar .form-control { font-size:13px; }
      .main-sidebar label { color:#ddd; }
    "))),

    sidebarMenu(
      id = "tabs",
      menuItem(" Overview",         tabName = "overview", icon = icon("home")),
      menuItem(" Raw File Upload",  tabName = "upload",   icon = icon("upload")),
      menuItem(" Export Settings",  tabName = "settings", icon = icon("cog")),
      menuItem(" Plate Review",     tabName = "review",   icon = icon("flask")),
      menuItem(" Export Table",     tabName = "table",    icon = icon("table")),
      menuItem(" Export",           tabName = "export",   icon = icon("download"))
    ),

    hr(),

    div(style = "padding:0 15px;",
      fileInput("raw_files", "📂 Upload Bio-Plex RAW files (.xlsx)",
                multiple = TRUE, accept = c(".xlsx", ".XLSX"), placeholder = "Select one or more…"),
      fileInput("meta_file", "📋 Shipment metadata (.xlsx) - optional", multiple = FALSE,
                accept = c(".xlsx", ".XLSX"), placeholder = "Pre-fills global_identifier"),
      h5("Run Parameters", style = "color:#aaa; font-size:12px; text-transform:uppercase; margin-top:10px;"),
      p(style = "font-size:12px; color:#bbb;", "Tick the analytes to include under ", tags$b("Raw File Upload → Analytes Detected"), "."),
      actionButton("run", "▶  Run Processing", class = "btn-primary btn-block", style = "margin-top:8px;")
    )
  ),

  dashboardBody(
    tabItems(

      ## ── OVERVIEW ───────────────────────────────────────────────────────────
      tabItem("overview",
        fluidRow(
          valueBoxOutput("vb_plates",   width = 3),
          valueBoxOutput("vb_analytes", width = 3),
          valueBoxOutput("vb_samples",  width = 3),
          valueBoxOutput("vb_rows",     width = 3)
        ),
        fluidRow(
          box(title = "Workflow Guide", width = 6, status = "primary", solidHeader = TRUE,
            tags$ol(style = "padding-left:20px; line-height:2;",
              tags$li("Upload one or more ", tags$b("Bio-Plex RAW .xlsx"), " files in the sidebar"),
              tags$li("Optionally upload the ", tags$b("shipment metadata .xlsx"), " to pre-fill ", tags$code("global_identifier")),
              tags$li("In the ", tags$b("Raw File Upload"), " tab, tick the analytes you want in ", tags$b("Analytes Detected"), " (BLANK is always added)"),
              tags$li("Optionally set ", tags$b("Baseline visit(s)"), " and review ", tags$b("Export Settings")),
              tags$li("Click ", tags$b("▶ Run Processing")),
              tags$li("Inspect ", tags$b("Plate Review"), " and the ", tags$b("Export Table")),
              tags$li("Download the XLSX + TXT from the ", tags$b("Export"), " tab")
            )
          ),
          box(title = "About This Dashboard", width = 6, status = "info", solidHeader = TRUE,
            tags$ul(style = "line-height:1.8;",
              tags$li("Runs the ", tags$b("report_michaela_7.R"), " logic on every uploaded file"),
              tags$li("Long format: one row per well × analyte"),
              tags$li("MFI + bead count split from e.g. ", tags$code("185,00 (50)")),
              tags$li("Background subtraction from Type ", tags$code("B"), " wells, per analyte"),
              tags$li("Technical replicates flagged individual / replicate"),
              tags$li("Same 95 columns as the reviewed example export, sorted A-Z on export"),
              tags$li(textOutput("version_txt", inline = TRUE))
            )
          )
        ),
        fluidRow(
          box(title = "Run Summary", width = 12, status = "warning", solidHeader = TRUE,
              uiOutput("run_msgs_ui"), DTOutput("dt_run_summary"))
        )
      ),

      ## ── RAW FILE UPLOAD ────────────────────────────────────────────────────
      tabItem("upload",
        fluidRow(
          box(title = "Uploaded RAW Files", width = 7, status = "primary", solidHeader = TRUE,
              DTOutput("dt_uploaded")),
          box(title = "Analytes Detected", width = 5, status = "info", solidHeader = TRUE,
              p(style = "font-size:12px; color:#666;",
                "Click a row to select an analyte - a tick appears under ", tags$b("Selected"),
                ". Only ticked analytes are exported; the BLANK analyte is always included automatically."),
              tags$div(style = "margin-bottom:6px;", actionLink("an_all", "Select all"), " | ", actionLink("an_none", "Clear selection")),
              DTOutput("dt_analytes"))
        ),
        fluidRow(
          box(title = "Raw Preview", width = 12, status = "info",
              selectInput("preview_file", "Select file to preview:", choices = NULL),
              DTOutput("dt_raw_preview"))
        )
      ),

      ## ── EXPORT SETTINGS ────────────────────────────────────────────────────
      tabItem("settings",
        fluidRow(
          box(title = "Study", width = 4, status = "primary", solidHeader = TRUE,
            p(style = "font-size:12px; color:#666;", "Written to every row. Applies the next time you click Run."),
            textInput("c_study", "study", DEFAULT_CONSTS$study),
            p(style = "font-size:12px; color:#888;",
              "Fixed defaults (not editable here): lab = AIRU, network = CAVD, first_detection = DIRECT IgG-PE, sample_species = HUMAN, exclude = FALSE, dilution = 50, expected_concentration = 1. sample_role and sample_type come from the RAW Type (see below).")
          ),
          box(title = "How the baseline / isotype settings work", width = 8, status = "info", solidHeader = TRUE,
            tags$ul(style = "line-height:1.8; font-size:13px;",
              tags$li("Run processing first so the visit list (", tags$code("visit_identifier"), ") and sample list (", tags$code("sample_identifier"), ") can be filled in."),
              tags$li("Defaults for every sample: global_identifier = ", tags$code(DEFAULT_CONSTS$global_identifier), " (or the metadata Other Spec ID when the shipment file matches; empty for BLANK and CONTROL wells), exclude = ", tags$code("FALSE"), " (always), isotype = ", tags$code(DEFAULT_CONSTS$isotype), "."),
              tags$li(tags$code("baseline_visit"), " = TRUE on the visit(s) picked below, FALSE on every other visit, and empty for BLANK / CONTROL wells. ",
                      tags$code("timepoint"), " = TRUE whenever ", tags$code("sample_role"), " is SAMPLE, CONTROL_MAB or CONTROL, and empty for BLANK / unknown."),
              tags$li("The isotype is locked to IgG. Switch ", tags$b("Do you want to change the isotype?"), " to Yes to unlock it, then pick the isotype, the plate(s) and the samples it applies to, and click ", tags$b("Add"), "."),
              tags$li("The sample list only shows samples found on the selected plate(s). The same sample can carry different isotypes on different plates; the later assignment wins."),
              tags$li("Changes show in the Export Table and downloads immediately - no re-run needed.")
            )
          )
        ),
        fluidRow(
          box(title = "Shipment metadata → global_identifier", width = 12, status = "warning", solidHeader = TRUE,
            p(style = "font-size:12px; color:#666;",
              "Upload the shipment .xlsx in the sidebar. A RAW well gets ", tags$code("global_identifier"), " = ", tags$b("Other Spec ID"),
              " when its Description's SampleName equals ", tags$b("PID/ Sample name"), " and the number in its VisitNumber (V104 → 104) equals ",
              tags$b("Visit number"), ". Everything else keeps the default."),
            uiOutput("meta_status_ui"),
            checkboxInput("meta_unmatched_only", "Show only sample/visit combinations without a match", TRUE),
            DTOutput("dt_meta_match")
          )
        ),
        fluidRow(
          box(title = "sample_role & sample_type - from the RAW Type column", width = 6, status = "primary", solidHeader = TRUE,
            tags$table(class = "table table-condensed", style = "font-size:13px; width:auto;",
              tags$thead(tags$tr(tags$th("RAW Type"), tags$th("sample_role"), tags$th("sample_type"))),
              tags$tbody(
                tags$tr(tags$td(tags$code("C"), " (C1, C2, …)"), tags$td("CONTROL_MAB or CONTROL - your choice below"), tags$td("(empty)")),
                tags$tr(tags$td(tags$code("X"), " (X1, X2, …)"), tags$td("SAMPLE"), tags$td(tags$code("SER"))),
                tags$tr(tags$td(tags$code("B"), " (B1, B2, …)"), tags$td("BLANK"), tags$td("(empty)")))),
            radioButtons("control_role", "Value for control wells (Type C)", c("CONTROL_MAB", "CONTROL"), selected = "CONTROL_MAB", inline = TRUE),
            uiOutput("role_status_ui")
          ),
          box(title = "summary - category", width = 6, status = "primary", solidHeader = TRUE,
            radioButtons("summary_mode", "Category",
                         c("Samples - individual / replicate from the RAW Type column" = "samples",
                           "Boolean - TRUE / FALSE for every row" = "boolean"), selected = "samples"),
            conditionalPanel("input.summary_mode == 'boolean'",
              radioButtons("summary_bool", "Boolean value", c("TRUE", "FALSE"), selected = "TRUE", inline = TRUE)),
            conditionalPanel("input.summary_mode == 'samples'",
              p(style = "font-size:12px; color:#666;",
                "The first well of each Type (read in plate order A1, B1 … A2 …) is ", tags$code("individual"),
                "; later wells with the same Type are ", tags$code("replicate"), "."))
          )
        ),
        fluidRow(
          box(title = "Baseline visit(s) & isotype", width = 6, status = "primary", solidHeader = TRUE,
            selectizeInput("baseline_visits", "Baseline visit(s) (visit_identifier) → baseline_visit = TRUE",
                           choices = NULL, multiple = TRUE, width = "100%",
                           options = list(placeholder = "Run processing first, then pick visit(s)…", plugins = list("remove_button"))),
            hr(),
            radioButtons("iso_change", "Do you want to change the isotype? (default: IgG)", c("No", "Yes"), selected = "No", inline = TRUE),
            selectizeInput("iso_val", "isotype - value", choices = c("IgG", "IgG1", "IgG2", "IgG3", "IgG4", "IgA", "IgM"),
                           selected = "IgG", width = "100%", options = list(create = TRUE)),
            conditionalPanel("input.iso_change == 'Yes'",
              selectizeInput("iso_plates", "Plate(s) this applies to (default: all)", choices = NULL, multiple = TRUE, width = "100%",
                             options = list(placeholder = "Run processing first…", plugins = list("remove_button"))),
              actionLink("iso_plates_all", "All plates"),
              selectizeInput("iso_samples", "Samples that carry this isotype (sample_identifier)", choices = NULL, multiple = TRUE, width = "100%",
                             options = list(placeholder = "Run processing first, then pick samples…", plugins = list("remove_button"))),
              actionLink("iso_all", "Select all samples"),
              tags$div(style = "margin-top:8px;", actionButton("iso_add", "➕ Add", class = "btn-primary")),
              hr(),
              h5("Assigned isotypes"),
              DTOutput("dt_rules_iso"),
              actionButton("iso_remove", "Remove selected", class = "btn-default btn-sm", style = "margin-top:6px;")
            )
          )
        )
      ),

      ## ── PLATE REVIEW ───────────────────────────────────────────────────────
      tabItem("review",
        fluidRow(
          box(width = 3, status = "primary",
            selectInput("rv_plate",   "Plate:",   choices = NULL),
            selectInput("rv_analyte", "Analyte:", choices = NULL),
            radioButtons("rv_metric", "Show:",
                         c("MFI" = "fluorescence_intensity",
                           "Background-subtracted MFI" = "fluorescence_intensity_background_subtracted",
                           "Bead count" = "bead_count")),
            numericInput("rv_min_beads", "Flag bead count below:", 30, min = 0, step = 5)
          ),
          box(title = "Plate Map", width = 9, status = "primary", solidHeader = TRUE,
              plotlyOutput("plate_map", height = "430px"), uiOutput("plate_legend"))
        ),
        fluidRow(
          box(title = "Background (Type B) wells & bead counts per plate × analyte", width = 12,
              status = "info", solidHeader = TRUE, DTOutput("dt_bg"))
        )
      ),

      ## ── EXPORT TABLE ───────────────────────────────────────────────────────
      tabItem("table",
        fluidRow(
          box(title = "Export Table Preview", width = 12, status = "primary", solidHeader = TRUE,
            checkboxInput("only_filled", "Show only columns filled from the RAW file / constants", TRUE),
            DTOutput("dt_final"))
        )
      ),

      ## ── EXPORT ─────────────────────────────────────────────────────────────
      tabItem("export",
        fluidRow(
          box(title = "Export Results", width = 6, status = "primary", solidHeader = TRUE,
            textInput("out_name", "Output file name (without extension)", "bama_export"),
            h4("XLSX Workbook"),
            p("One sheet, ", tags$code("export"), ", frozen header row, dates as yyyy-mm-dd."),
            downloadButton("dl_xlsx", "📥 Download .xlsx", class = "btn-success btn-lg"),
            hr(),
            h4("Tab-delimited TXT"),
            p("The same table as plain text (empty cells for NA)."),
            downloadButton("dl_txt", "📄 Download .txt", class = "btn-info btn-lg"),
            hr(),
            h4("Run Metadata"),
            verbatimTextOutput("export_meta")
          ),
          box(title = "Column Legend", width = 6, status = "info",
            tags$table(class = "table table-condensed", style = "font-size:12px;",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("analyte"), tags$td("Analyte name, region id removed")),
                tags$tr(tags$td("fluorescence_intensity"), tags$td("MFI (value before the bracket)")),
                tags$tr(tags$td("bead_count"), tags$td("Number in brackets after the MFI")),
                tags$tr(tags$td("fluorescence_intensity_background_subtracted"), tags$td("MFI minus per-analyte mean of Type B wells (negatives kept)")),
                tags$tr(tags$td("negative_bead"), tags$td("TRUE for the BLANK analyte rows, FALSE for all other analytes (BLANK is exported with the same columns as any analyte)")),
                tags$tr(tags$td("sample_role"), tags$td("From the well Type: C = CONTROL_MAB or CONTROL (Export Settings), X = SAMPLE, B = BLANK")),
                tags$tr(tags$td("sample_type"), tags$td("From the well Type: X = SER; C or B = empty")),
                tags$tr(tags$td("global_identifier"), tags$td("guspec (or the metadata Other Spec ID); empty for BLANK and CONTROL wells")),
                tags$tr(tags$td("baseline_visit"), tags$td("TRUE for the baseline visit(s) chosen in Export Settings, FALSE for other visits, empty for BLANK / CONTROL")),
                tags$tr(tags$td("timepoint"), tags$td("TRUE when sample_role is SAMPLE, CONTROL_MAB or CONTROL; empty for BLANK / unknown")),
                tags$tr(tags$td("exclude"), tags$td("Always FALSE")),
                tags$tr(tags$td("dilution"), tags$td("Pre-set to 50 on every row")),
                tags$tr(tags$td("expected_concentration"), tags$td("1 for control wells (CONTROL_MAB / CONTROL); empty for samples, BLANK wells and the Blank analyte")),
                tags$tr(tags$td("isotype"), tags$td("IgG unless changed in Export Settings")),
                tags$tr(tags$td("well / well_group"), tags$td("Well position / RAW Type")),
                tags$tr(tags$td("summary"), tags$td("individual (first well of a Type) / replicate, or TRUE / FALSE if the Boolean category is chosen in Export Settings")),
                tags$tr(tags$td("sample_identifier / visit_identifier"), tags$td("Parts 1 and 2 of Description split on _")),
                tags$tr(tags$td("experiment_identifier"), tags$td("Full Plate ID from the RAW header")),
                tags$tr(tags$td("plate_identifier"), tags$td("3rd token of Plate ID (e.g. P1)")),
                tags$tr(tags$td("experiment_date"), tags$td("Date (YYYYMMDD) in the RAW File Name; falls back to the Plate ID, then the Acquisition Date line")),
                tags$tr(tags$td("dataset_creation_date"), tags$td("Date the export was created"))
              )
            )
          )
        )
      )
    )
  )
)

## ═════════════════════════════════════════════════════════════════════════════
##  SERVER
## ═════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  rv <- reactiveValues(files = NULL, info = NULL, final = NULL, summary = NULL, msgs = character(), rules = list(), meta = NULL, meta_name = NULL)

  ## ---- upload: inspect every RAW file ----------------------------------------
  observeEvent(input$raw_files, {
    f <- input$raw_files
    infos <- lapply(seq_len(nrow(f)), function(i) {
      tryCatch(inspect_raw(f$datapath[i]), error = function(e) list(error = conditionMessage(e)))
    })
    rv$files <- f
    rv$info  <- infos
    rv$final <- NULL; rv$summary <- NULL; rv$msgs <- character(); rv$rules <- list()

    updateSelectInput(session, "preview_file", choices = setNames(seq_len(nrow(f)), f$name))
    bad <- sum(vapply(infos, function(x) !is.null(x$error), logical(1)))
    if (bad) showNotification(paste(bad, "file(s) could not be read - see Raw File Upload tab"), type = "warning")
  })

  output$dt_uploaded <- renderDT({
    req(rv$files)
    df <- do.call(rbind, lapply(seq_len(nrow(rv$files)), function(i) {
      x <- rv$info[[i]]
      if (!is.null(x$error)) return(data.frame(File = rv$files$name[i], `Plate ID` = NA, Date = NA, Reader = NA,
                                               Wells = NA, Analytes = NA, Status = paste("Error:", x$error), check.names = FALSE))
      data.frame(File = rv$files$name[i], `Plate ID` = x$plate_full, Date = as.character(x$experiment_date),
                 Reader = x$machine_serial_number, Wells = x$n_wells, Analytes = length(x$analytes[tolower(x$analytes) != "blank"]),
                 Status = "OK", check.names = FALSE)
    }))
    datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tp"))
  })

  ## analytes found in the uploaded files (BLANK is always exported, so it is not listed)
  analyte_tab <- reactive({
    a <- unlist(lapply(rv$info, function(x) x$analytes))
    a <- a[!is.na(a) & tolower(a) != "blank"]
    if (!length(a)) return(data.frame(Analyte = character(), Files = integer(), Selected = character(), stringsAsFactors = FALSE))
    tab <- as.data.frame(table(Analyte = a), stringsAsFactors = FALSE)
    names(tab)[2] <- "Files"
    tab$Selected <- ""
    tab
  })

  output$dt_analytes <- renderDT({
    tab <- analyte_tab()
    req(nrow(tab) > 0)
    datatable(tab, rownames = FALSE, selection = list(mode = "multiple", selected = integer(0)),
              options = list(pageLength = 10, dom = "tp"))
  })
  dt_an_proxy <- dataTableProxy("dt_analytes")

  ## the tick under "Selected" follows the row selection
  observeEvent(input$dt_analytes_rows_selected, {
    tab <- analyte_tab(); req(nrow(tab) > 0)
    sel <- input$dt_analytes_rows_selected
    tab$Selected[sel[sel <= nrow(tab)]] <- "✔"
    replaceData(dt_an_proxy, tab, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
  }, ignoreNULL = FALSE)
  observeEvent(input$an_all,  selectRows(dt_an_proxy, seq_len(nrow(analyte_tab()))))
  observeEvent(input$an_none, selectRows(dt_an_proxy, NULL))

  selected_analytes <- reactive({
    tab <- analyte_tab()
    sel <- input$dt_analytes_rows_selected
    if (!nrow(tab) || !length(sel)) return(character(0))
    tab$Analyte[sel[sel <= nrow(tab)]]
  })

  output$dt_raw_preview <- renderDT({
    req(rv$files, input$preview_file)
    i <- as.integer(input$preview_file)
    df <- as.data.frame(read_raw_all(rv$files$datapath[i]))
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })

  ## ---- shipment metadata upload -----------------------------------------------------
  observeEvent(input$meta_file, {
    f <- input$meta_file
    msgs <- character()
    res <- withCallingHandlers(
      tryCatch(read_shipment(f$datapath),
               error = function(e) { showNotification(paste0("Metadata not loaded: ", conditionMessage(e)), type = "error", duration = 10); NULL }),
      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
    rv$meta <- res
    rv$meta_name <- if (is.null(res)) NULL else f$name
    if (!is.null(res)) showNotification(paste0("Metadata loaded: ", nrow(res), " PID/visit rows", if (length(msgs)) paste0(" (", msgs[1], ")") else ""),
                                        type = if (length(msgs)) "warning" else "message")
  })

  output$role_status_ui <- renderUI({
    if (is.null(rv$final)) return(tags$p(style = "font-size:12px; color:#888;", "Run processing to see how many rows get each role."))
    f <- final_df(); tab <- table(factor(ifelse(is.na(f$sample_role), "(empty)", f$sample_role)))
    unk <- sort(unique(as.character(f$well_group[is.na(f$sample_role) & !is.na(f$well_group)])))
    tags$div(style = "font-size:12px;", tags$b("Rows by sample_role: "), paste(names(tab), tab, sep = " = ", collapse = ", "),
             if (length(unk)) tags$div(class = "alert alert-warning", style = "padding:6px; margin-top:6px;",
                                       "Type(s) not recognised (sample_role left empty): ", paste(unk, collapse = ", ")))
  })

  meta_match_df <- reactive({ req(rv$final); metadata_match(rv$final, rv$meta) })

  output$meta_status_ui <- renderUI({
    if (is.null(rv$meta))
      return(tags$div(class = "alert alert-info", style = "padding:7px; font-size:12px;", "No metadata file loaded - global_identifier stays at the default."))
    base <- paste0(rv$meta_name, ": ", nrow(rv$meta), " PID/visit rows loaded.")
    if (is.null(rv$final))
      return(tags$div(class = "alert alert-success", style = "padding:7px; font-size:12px;", base, " Run processing to see the matches."))
    mm <- meta_match_df()
    n_hit <- sum(mm$Matched); n <- nrow(mm)
    tags$div(class = if (n_hit == n) "alert alert-success" else "alert alert-warning", style = "padding:7px; font-size:12px;",
             base, tags$b(paste0(" Matched ", n_hit, " of ", n, " sample/visit combinations.")),
             if (n_hit < n) " The rest keep the default global_identifier (controls without a visit number cannot match).")
  })

  output$dt_meta_match <- renderDT({
    if (is.null(rv$final)) return(datatable(data.frame(Note = "Run processing to see the match results"), rownames = FALSE, options = list(dom = "t")))
    mm <- meta_match_df()
    if (isTRUE(input$meta_unmatched_only)) mm <- mm[!mm$Matched, , drop = FALSE]
    mm$Matched <- ifelse(mm$Matched, "✔", "✘")
    datatable(mm, rownames = FALSE, options = list(pageLength = 8, dom = "tp", scrollX = TRUE,
              language = list(emptyTable = "Nothing to show")))
  })

  ## ---- Export Settings: plate / sample choices + assignment rules ---------------
  all_plates <- reactive({ req(rv$final); unique(as.character(rv$final$experiment_identifier)) })
  samples_on <- function(plates) {                 # samples found on the selected plate(s)
    if (is.null(rv$final) || !length(plates)) return(character(0))
    d <- rv$final[as.character(rv$final$experiment_identifier) %in% plates, ]
    sort(unique(na.omit(d$sample_identifier)))
  }

  ## clean a vector of choices: character, no NA / empty strings, unique, sorted
  clean_choices <- function(x) {
    x <- unique(trimws(as.character(x)))
    sort(x[!is.na(x) & nzchar(x)])
  }

  refresh_export_dropdowns <- function() {
    req(rv$final)
    plates <- clean_choices(rv$final$experiment_identifier)
    vis    <- clean_choices(rv$final$visit_identifier)
    for (p in "iso") {
      sel <- intersect(isolate(input[[paste0(p, "_plates")]]), plates)
      if (!length(sel)) sel <- plates                                  # default: all plates
      updateSelectizeInput(session, paste0(p, "_plates"), choices = plates, selected = sel, server = FALSE)
      smp <- clean_choices(samples_on(sel))
      updateSelectizeInput(session, paste0(p, "_samples"), choices = smp,
                           selected = intersect(isolate(input[[paste0(p, "_samples")]]), smp), server = FALSE)
    }
    updateSelectizeInput(session, "baseline_visits", choices = vis,
                         selected = intersect(isolate(input$baseline_visits), vis), server = FALSE)
  }

  refresh_review_dropdowns <- function() {
    req(rv$final)
    plates <- clean_choices(rv$final$experiment_identifier)
    cur <- isolate(input$rv_plate)
    if (is.null(cur) || !cur %in% plates) cur <- plates[1]
    updateSelectInput(session, "rv_plate", choices = plates, selected = cur)
    f <- rv$final[as.character(rv$final$experiment_identifier) %in% cur, ]
    an <- unique(as.character(f$analyte)); an <- an[!is.na(an) & nzchar(an)]
    curA <- isolate(input$rv_analyte)
    updateSelectInput(session, "rv_analyte", choices = an, selected = if (!is.null(curA) && curA %in% an) curA else an[1])
  }

  ## fill after a run, and again whenever the user opens the tab (inputs in hidden tabs can miss the first update)
  observeEvent(rv$final, { refresh_export_dropdowns(); refresh_review_dropdowns() }, ignoreNULL = TRUE)
  observeEvent(input$tabs, {
    if (is.null(rv$final)) return()
    if (identical(input$tabs, "settings")) refresh_export_dropdowns()
    if (identical(input$tabs, "review"))   refresh_review_dropdowns()
  })

  ## when the plate selection changes, the sample dropdown follows
  for (p in "iso") local({
    pp <- p
    observeEvent(input[[paste0(pp, "_plates")]], {
      req(rv$final)
      smp <- samples_on(input[[paste0(pp, "_plates")]])
      updateSelectizeInput(session, paste0(pp, "_samples"), choices = smp,
                           selected = intersect(isolate(input[[paste0(pp, "_samples")]]), smp), server = FALSE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(pp, "_plates_all")]],
                 updateSelectizeInput(session, paste0(pp, "_plates"), selected = all_plates(), server = FALSE))
    observeEvent(input[[paste0(pp, "_all")]],
                 updateSelectizeInput(session, paste0(pp, "_samples"), selected = samples_on(input[[paste0(pp, "_plates")]]), server = FALSE))
  })

  add_rule <- function(col, value, samples, plates) {
    value <- trimws(as.character(value %||% ""))
    if (!length(plates))  return(showNotification("Select at least one plate.", type = "warning"))
    if (!length(samples)) return(showNotification("Select at least one sample.", type = "warning"))
    if (!nzchar(value))   return(showNotification("Enter a value first.", type = "warning"))
    rv$rules <- c(rv$rules, list(list(column = col, value = value, samples = samples, plates = plates)))
  }
  plates_label <- function(pl) {
    allp <- unique(as.character(rv$final$experiment_identifier))
    if (length(allp) && setequal(pl, allp)) paste0("All plates (", length(allp), ")") else paste(pl, collapse = ", ")
  }
  rules_df <- function(cols) {
    idx <- which(vapply(rv$rules, function(r) r$column %in% cols, logical(1)))
    data.frame(idx = idx,
               Column  = vapply(rv$rules[idx], function(r) r$column, ""),
               Value   = vapply(rv$rules[idx], function(r) r$value, ""),
               Plates  = vapply(rv$rules[idx], function(r) if (is.null(r$plates)) "All plates" else plates_label(r$plates), ""),
               Samples = vapply(rv$rules[idx], function(r) paste(r$samples, collapse = ", "), ""),
               N       = vapply(rv$rules[idx], function(r) length(r$samples), 1L),
               stringsAsFactors = FALSE)
  }
  remove_rules <- function(cols, sel) {
    if (!length(sel)) return(showNotification("Select row(s) in the table to remove.", type = "warning"))
    drop <- rules_df(cols)$idx[sel]; drop <- drop[!is.na(drop)]
    if (length(drop)) rv$rules <- rv$rules[-drop]
  }
  ISO_COLS <- "isotype"

  observeEvent(input$iso_add, add_rule("isotype", input$iso_val, input$iso_samples, input$iso_plates))
  observeEvent(input$iso_remove, remove_rules(ISO_COLS, input$dt_rules_iso_rows_selected))

  rules_dt <- function(cols) {
    df <- rules_df(cols)[, -1, drop = FALSE]
    datatable(df, rownames = FALSE, selection = "multiple",
              options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                             language = list(emptyTable = "No assignments yet - defaults apply to all samples")))
  }
  output$dt_rules_iso <- renderDT(rules_dt(ISO_COLS))

  ## isotype is locked to IgG unless the user answers Yes to "change the isotype?"
  observeEvent(input$iso_change, {
    if (identical(input$iso_change, "Yes")) {
      shinyjs::enable("iso_val")
    } else {
      updateSelectizeInput(session, "iso_val", selected = "IgG")
      shinyjs::disable("iso_val")
    }
  })

  ## ---- run ----------------------------------------------------------------------
  observeEvent(input$run, {
    if (is.null(rv$files)) return(showNotification("Upload at least one RAW file first.", type = "error"))
    analytes <- selected_analytes()
    if (!length(analytes))
      return(showNotification("Select at least one analyte in Raw File Upload > Analytes Detected.", type = "error"))

    consts <- DEFAULT_CONSTS
    consts$study <- input$c_study

    msgs <- character(); results <- list(); srows <- list()
    withProgress(message = "Processing RAW files…", value = 0, {
      for (i in seq_len(nrow(rv$files))) {
        nm <- rv$files$name[i]
        incProgress(1 / nrow(rv$files), detail = nm)
        res <- withCallingHandlers(
          tryCatch(process_raw(rv$files$datapath[i], analytes, consts, source_name = nm),
                   error = function(e) { msgs <<- c(msgs, paste0("Failed on ", nm, ": ", conditionMessage(e))); NULL }),
          warning = function(w) { msgs <<- c(msgs, paste0(nm, ": ", conditionMessage(w))); invokeRestart("muffleWarning") })
        if (is.null(res)) {
          msgs <- c(msgs, paste0("No rows produced for ", nm, " (none of the selected analytes found, or file failed); skipped"))
        } else {
          results[[nm]] <- res
          srows[[nm]] <- data.frame(File = nm, `Plate ID` = res$experiment_identifier[1],
                                    Date = as.character(res$experiment_date[1]),
                                    Analytes = length(unique(res$analyte[tolower(res$analyte) != "blank"])), Rows = nrow(res),
                                    check.names = FALSE)
        }
      }
    })
    rv$msgs <- msgs
    if (length(results) == 0) {
      rv$final <- NULL; rv$summary <- NULL
      return(showNotification("None of the selected analytes were found in any file.", type = "error"))
    }
    final <- bind_rows(results); rownames(final) <- NULL
    rv$final <- final
    rv$summary <- do.call(rbind, srows)
    showNotification(paste0("Done: ", nrow(final), " rows from ", length(results), " file(s)."), type = "message")
    updateTabItems(session, "tabs", "overview")
  })

  ## final table with the baseline visits and sample-level assignments applied
  final_df <- reactive({
    req(rv$final)
    rules <- if (identical(input$iso_change, "Yes")) rv$rules else list()      # isotype stays IgG unless the user opted to change it
    df <- apply_baseline(apply_metadata(rv$final, rv$meta), input$baseline_visits)
    df <- apply_rules(df, rules)
    df <- apply_sample_role(df, input$control_role %||% "CONTROL_MAB")
    df <- apply_sample_type(df)                                                # X* -> SER, C* / B* -> empty
    df <- apply_global_identifier(df)                                          # empty for BLANK + CONTROL
    df <- apply_baseline_visit(df, input$baseline_visits)                      # TRUE / FALSE per visit, empty for BLANK + CONTROL
    df <- apply_timepoint(df)                                                  # TRUE for SAMPLE / CONTROL(_MAB), empty otherwise
    df <- apply_expected_concentration(df)                                     # 1 for controls only
    df <- apply_summary(df, input$summary_mode %||% "samples", input$summary_bool %||% "TRUE")
    df$exclude <- FALSE                                                        # always FALSE
    df
  })

  ## ---- overview -----------------------------------------------------------------
  output$version_txt <- renderText(paste("Script version:", SCRIPT_VERSION))
  output$vb_plates   <- renderValueBox(valueBox(if (is.null(rv$summary)) 0 else nrow(rv$summary), "Plates Processed", icon = icon("vials"), color = "blue"))
  output$vb_analytes <- renderValueBox(valueBox(if (is.null(rv$final)) 0 else length(unique(rv$final$analyte[tolower(rv$final$analyte) != "blank"])), "Analytes Extracted", icon = icon("virus"), color = "yellow"))
  output$vb_samples  <- renderValueBox(valueBox(if (is.null(rv$final)) 0 else length(unique(na.omit(rv$final$sample_identifier))), "Unique Samples", icon = icon("flask"), color = "green"))
  output$vb_rows     <- renderValueBox(valueBox(if (is.null(rv$final)) 0 else format(nrow(rv$final), big.mark = ","), "Rows in Export", icon = icon("table"), color = "red"))

  output$run_msgs_ui <- renderUI({
    if (is.null(rv$final)) return(tags$p(style = "color:#888; font-size:12px;", "No run yet - upload RAW files and click ▶ Run Processing."))
    if (length(rv$msgs) == 0) return(tags$div(class = "alert alert-success", style = "padding:7px; font-size:12px;", "Processed without warnings."))
    tags$div(class = "alert alert-warning", style = "padding:7px; font-size:12px;",
             tags$b("Warnings:"), tags$ul(lapply(unique(rv$msgs), tags$li)))
  })
  output$dt_run_summary <- renderDT({
    req(rv$summary)
    datatable(rv$summary, rownames = FALSE, options = list(pageLength = 10, dom = "tp", scrollX = TRUE))
  })

  ## ---- plate review -------------------------------------------------------------
  observeEvent(input$rv_plate, {
    req(rv$final, input$rv_plate)
    f <- rv$final[as.character(rv$final$experiment_identifier) %in% input$rv_plate, ]
    an <- unique(as.character(f$analyte)); an <- an[!is.na(an) & nzchar(an)]
    curA <- isolate(input$rv_analyte)
    updateSelectInput(session, "rv_analyte", choices = an, selected = if (!is.null(curA) && curA %in% an) curA else an[1])
  })

  plate_data <- reactive({
    req(rv$final, input$rv_plate, input$rv_analyte)
    f <- rv$final[as.character(rv$final$experiment_identifier) %in% input$rv_plate & as.character(rv$final$analyte) %in% input$rv_analyte, ]
    req(nrow(f) > 0)
    f
  })
  bead_threshold <- reactive({
    thr <- suppressWarnings(as.numeric(input$rv_min_beads))
    validate(need(length(thr) == 1 && !is.na(thr), "Enter a number for 'Flag bead count below'."))
    thr
  })

  output$plate_map <- renderPlotly(make_plate_map(plate_data(), input$rv_metric, bead_threshold()))

  output$plate_legend <- renderUI({
    f <- plate_data(); thr <- bead_threshold()
    bc <- f$bead_count; n_low <- sum(bc < thr, na.rm = TRUE); n_ok <- sum(bc >= thr, na.rm = TRUE); n_na <- sum(is.na(bc))
    sw <- function(col, txt) tags$span(style = "margin-right:16px;", tags$span(style = paste0("display:inline-block;width:12px;height:12px;background:", col, ";margin-right:5px;border:1px solid #999;")), txt)
    if (identical(input$rv_metric, "bead_count"))
      tags$div(style = "font-size:12px; margin-top:6px;",
               sw("#d9534f", paste0("below ", thr, ": ", n_low, " wells")), sw("#5cb85c", paste0(thr, " or more: ", n_ok, " wells")),
               if (n_na) sw("#e6e6e6", paste0("no bead count: ", n_na, " wells")))
    else
      tags$div(style = "font-size:12px; margin-top:6px; color:#666;",
               "✕ marks wells with a bead count below ", thr, " (", n_low, " on this plate/analyte). Switch to 'Bead count' for the red / green view.")
  })

  output$dt_bg <- renderDT({
    req(rv$final)
    f <- rv$final
    g <- split(f, list(f$experiment_identifier, f$analyte), drop = TRUE)
    tab <- do.call(rbind, lapply(g, function(d) {
      b <- d[!is.na(d$well_group) & d$well_group == "B", ]
      data.frame(Plate = d$experiment_identifier[1], Analyte = d$analyte[1], Wells = nrow(d),
                 `B wells` = nrow(b),
                 `Mean B MFI` = if (nrow(b)) round(mean(b$fluorescence_intensity, na.rm = TRUE), 1) else NA,
                 `Mean bead count` = round(mean(d$bead_count, na.rm = TRUE), 1),
                 `Min bead count` = suppressWarnings(min(d$bead_count, na.rm = TRUE)),
                 `Wells below threshold` = sum(d$bead_count < input$rv_min_beads, na.rm = TRUE),
                 check.names = FALSE)
    }))
    rownames(tab) <- NULL
    datatable(tab, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tp"))
  })

  ## ---- export table -------------------------------------------------------------
  output$dt_final <- renderDT({
    df <- final_df()
    if (isTRUE(input$only_filled)) df <- df[, intersect(ordered_cols(), names(df))[seq_len(length(union(names(DEFAULT_CONSTS), DERIVED_COLS)))], drop = FALSE]
    df <- alpha_cols(df)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = "lftip"))
  })

  ## ---- downloads ------------------------------------------------------------------
  out_base <- reactive({
    n <- gsub("[^A-Za-z0-9_.-]", "_", trimws(input$out_name %||% "bama_export"))
    sub("\\.(xlsx|txt)$", "", n, ignore.case = TRUE)
  })
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(out_base(), ".xlsx"),
    content  = function(file) write_export_xlsx(final_df(), file))
  output$dl_txt <- downloadHandler(
    filename = function() paste0(out_base(), ".txt"),
    content  = function(file) write_export_txt(final_df(), file))

  output$export_meta <- renderText({
    if (is.null(rv$final)) return("Nothing to export yet - run the processing first.")
    f <- final_df()
    paste0("Files processed : ", nrow(rv$summary), "\n",
           "Analytes chosen : ", paste(selected_analytes(), collapse = ", "), "\n",
           "Analytes exported: ", paste(unique(f$analyte), collapse = ", "), "\n",
           "Rows x columns  : ", nrow(f), " x ", ncol(f), "\n",
           "Baseline visits : ", if (length(input$baseline_visits)) paste(input$baseline_visits, collapse = ", ") else "(none)", "\n",
           "Metadata        : ", if (is.null(rv$meta)) "(none)" else paste0(rv$meta_name, " - ", sum(metadata_match(rv$final, rv$meta)$Matched), " of ", nrow(metadata_match(rv$final, rv$meta)), " sample/visit combinations matched"), "\n",
           "sample_role      : Type C -> ", input$control_role, "; X -> SAMPLE; B -> BLANK", "\n",
           "summary         : ", if (identical(input$summary_mode, "boolean")) paste0("Boolean (", input$summary_bool, ")") else "Samples (individual / replicate)", "\n",
           "Isotype         : ", if (identical(input$iso_change, "Yes")) paste0("custom (", length(rv$rules), " assignment(s), default IgG)") else "IgG (default)", "\n",
           "Created         : ", format(Sys.time(), "%Y-%m-%d %H:%M"))
  })
}

shinyApp(ui, server)
