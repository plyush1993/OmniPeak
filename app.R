# Sys.setlocale("LC_ALL", "English_United States.1252")
# Sys.setenv(LANG = "en_US.UTF-8")
# options(encoding = "UTF-8")
# options(timeout = 600)
# options(rsconnect.http.timeout = 600)
# rsconnect::deployApp()

# app.R --------------------------------------------------------------------
# OmniPeak for peak table reshaping
# -------------------------------------------------------------------------

library(shiny)
library(vroom)
library(DT)
library(dplyr)
library(data.table)
library(tibble)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(waiter)
library(shinyAce)

options(shiny.maxRequestSize = 1024 * 1024^2)

#..........................................
# utils ---- 
#..........................................
js_copy <- "
shinyjs.copyCode = function(text) {
  var x = document.createElement('textarea');
  x.value = text;
  document.body.appendChild(x);
  x.select();
  document.execCommand('copy');
  document.body.removeChild(x);
  alert('R Script copied to clipboard!');
}
"

# Helper function to safely subset for fast previews
fast_preview <- function(df, max_rows = 50, max_cols = 100) {
    df[1:min(max_rows, nrow(df)), 1:min(max_cols, ncol(df)), drop = FALSE]
  }

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

standardize_peak_table <- function(df, type) {
  type <- match.arg(type, c("mzmine", "default", "xcms", "msdial"))

  df <- as.data.frame(df)
  # Basic name cleanup to start
  names(df) <- trimws(names(df))

  export_template  <- names(df)
  export_colmap    <- c()
  export_rt_factor <- 1
  msdial_preamble <- NULL
  msdial_export_names <- NULL

  # Helper: Normalize strings for soft matching
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  # Helper: Find a column in the CURRENT names matching a target
  find_col <- function(target, current_names) {
    n_target <- norm(target)
    n_curr   <- norm(current_names)
    idx <- match(n_target, n_curr)
    if (!is.na(idx)) current_names[idx] else NA_character_
  }

  if (type == "mzmine") {
    mz_col <- find_col("row m/z", names(df))
    if (is.na(mz_col)) mz_col <- find_col("mz", names(df))

    rt_col <- find_col("row retention time", names(df))
    if (is.na(rt_col)) rt_col <- find_col("rt", names(df))

    export_colmap <- c(mz = mz_col, rt = rt_col)
    export_template <- names(df)

    if (is.na(mz_col) || is.na(rt_col)) {
      stop("MZmine table missing m/z or RT column.")
    }
    df <- dplyr::rename(df, mz = !!mz_col, rt = !!rt_col)

    id_col <- find_col("row id", names(df))
    if (!is.na(id_col)) {
      export_colmap <- c(export_colmap, feature_id = id_col)
      df$feature_id <- df[[id_col]]
    }

  } else if (type == "default") {
    req_cols <- c("Feature", "mz", "rt")
    miss <- setdiff(req_cols, names(df))
    export_colmap <- c(mz = "mz", rt = "rt")
    export_template <- names(df)
    if (length(miss)) stop("DEFAULT table missing: ", paste(miss, collapse = ", "))
    df <- dplyr::rename(df, mz = `mz`, rt = `rt`)

  } else if (type == "msdial") {
    header_keywords <- c("Alignment ID", "Average Mz", "Average Rt")

    hdr_i <- NA
    for (i in 1:min(30, nrow(df))) {
      row_txt <- as.character(unlist(df[i, ]))
      if ("averagemz" %in% norm(row_txt) || "alignmentid" %in% norm(row_txt)) {
        hdr_i <- i
        break
      }
    }

    if (!is.na(hdr_i)) {
      if (hdr_i > 1) msdial_preamble <- df[1:(hdr_i - 1), , drop = FALSE]

      new_names <- trimws(as.character(unlist(df[hdr_i, , drop = TRUE])))
      new_names[is.na(new_names) | new_names == ""] <- paste0("Unknown_", seq_along(new_names))[is.na(new_names) | new_names == ""]
      new_names <- make.unique(new_names, sep = "_")

      if (!is.null(msdial_preamble)) {
        if (ncol(msdial_preamble) == length(new_names)) names(msdial_preamble) <- new_names
      }

      msdial_export_names <- new_names
      export_template <- new_names 

      if (hdr_i < nrow(df)) {
        df <- df[(hdr_i + 1):nrow(df), , drop = FALSE]
        names(df) <- new_names
      } else {
        df <- df[0, , drop = FALSE]
        names(df) <- new_names
      }
    } else {
      msdial_export_names <- names(df)
    }

    mz_col <- find_col("Average Mz", names(df))
    rt_col <- find_col("Average Rt(min)", names(df))

    if (is.na(mz_col) || is.na(rt_col)) {
      stop("MS-DIAL table missing: 'Average Mz' or 'Average Rt(min)'. Check file format.")
    }

    export_colmap <- c(mz = mz_col, rt = rt_col)
    df <- dplyr::rename(df, mz = !!mz_col, rt = !!rt_col)
    df$mz <- suppressWarnings(as.numeric(df$mz))
    df$rt <- suppressWarnings(as.numeric(df$rt))

    attr(df, "msdial_preamble") <- msdial_preamble
    attr(df, "msdial_export_names") <- msdial_export_names

  } else if (type == "xcms") {
    req_cols <- c("mzmed", "rtmed")
    miss <- setdiff(req_cols, names(df))
    if (length(miss)) stop("XCMS table missing: ", paste(miss, collapse = ", "))

    export_template <- names(df)
    export_colmap <- c(mz = "mzmed", rt = "rtmed")

    df <- dplyr::rename(df, mz = mzmed, rt = rtmed)
    export_rt_factor <- 1
    df$rt <- df$rt / 1
  }

  df$mz <- suppressWarnings(as.numeric(df$mz))
  df$rt <- suppressWarnings(as.numeric(df$rt))
  if (!"feature_id" %in% names(df)) df$feature_id <- seq_len(nrow(df))

  attr(df, "export_template")  <- export_template
  attr(df, "export_colmap")    <- export_colmap
  attr(df, "export_rt_factor") <- export_rt_factor

  df
}

format_final_table_as_input <- function(final_df_with_fid, type,
                                        export_template = NULL,
                                        export_colmap = NULL,
                                        export_rt_factor = 1) {
  df <- as.data.frame(final_df_with_fid, check.names = FALSE, stringsAsFactors = FALSE)

  if (identical(type, "mzmine")) {
    df <- df[, setdiff(names(df), ".FID"), drop = FALSE]
  } else {
    df <- df[, setdiff(names(df), c("feature_id", ".FID")), drop = FALSE]
  }

  if (is.finite(export_rt_factor) && export_rt_factor != 1 && "rt" %in% names(df)) {
    df$rt <- suppressWarnings(as.numeric(df$rt)) * export_rt_factor
  }

  if (!is.null(export_colmap) && length(export_colmap)) {
    for (std_nm in names(export_colmap)) {
      orig_nm <- export_colmap[[std_nm]]
      if (std_nm %in% names(df) && nzchar(orig_nm)) {
        names(df)[names(df) == std_nm] <- orig_nm
      }
    }
  }

  if (!is.null(export_template) && length(export_template)) {
    ord  <- intersect(export_template, names(df))
    rest <- setdiff(names(df), ord)
    df <- df[, c(ord, rest), drop = FALSE]
  }

  df
}

multi_sample_idx <- function(cols, kws) {
  kws <- as.character(kws)
  kws <- kws[nzchar(kws)]
  if (!length(kws)) return(integer(0))
  hits <- Reduce(`|`, lapply(kws, function(k) grepl(k, cols, fixed = TRUE)))
  which(hits)
}

labels_from_sample_names <- function(sample_names, token_sep = "_", token_index = 2) {
  token_sep <- token_sep %||% "_"
  token_index <- as.integer(token_index %||% 2)

  parts <- strsplit(sample_names, token_sep, fixed = TRUE)
  has_ix <- vapply(parts, function(v) length(v) >= token_index, logical(1))
  if (!all(has_ix)) stop(sprintf("Token %d missing in some sample names.", token_index))
  labs <- vapply(parts, function(v) v[[token_index]], FUN.VALUE = character(1))
  if (!all(nzchar(labs))) stop("Parsed empty labels — adjust separator/index.")
  labs
}

# --- UPDATED: Handles both CSV and TXT delimiters seamlessly ---
write_final_table <- function(path, df_export, type, msdial_preamble = NULL, sep = ",") {
  df_export <- as.data.frame(df_export, check.names = FALSE, stringsAsFactors = FALSE)

  if (identical(type, "msdial") && !is.null(msdial_preamble) && nrow(msdial_preamble) > 0) {
    pre <- as.data.frame(msdial_preamble, check.names = FALSE, stringsAsFactors = FALSE)

    pre <- pre[, intersect(names(df_export), names(pre)), drop = FALSE]
    pre <- pre[, names(df_export), drop = FALSE]

    cleaned_names <- gsub("_[0-9]+$", "", names(df_export))
    names(df_export) <- cleaned_names

    write.table(pre, file = path, sep = sep,
                row.names = FALSE, col.names = FALSE,
                quote = TRUE, na = "", append = FALSE)

    write.table(df_export, file = path, sep = sep,
                row.names = FALSE, col.names = TRUE,
                quote = TRUE, na = "", append = TRUE)
  } else {
    write.table(df_export, file = path, sep = sep, row.names = FALSE, quote = TRUE, na = "")
  }
}

clean_mzmine_export <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) > 0 && all(is.na(df[[ncol(df)]]))) df <- df[, -ncol(df), drop = FALSE]
  df[is.na(df)] <- ""
  df
}

make_label_table <- function(sample_names, labels) {
  tibble::tibble(
    Sample = as.character(sample_names),
    Label  = trimws(as.character(labels))
  )
}

labels_from_sample_names_or_raw <- function(sample_names, token_sep = "_", token_index = 2) {
  tryCatch(
    labels_from_sample_names(
      sample_names,
      token_sep = token_sep,
      token_index = token_index
    ),
    error = function(e) {
      sample_names
    }
  )
}

parse_suffix_list <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))

  x <- as.character(x)
  x <- x[!is.na(x)]

  out <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  out[nzchar(out)]
}

clean_sample_names_optional <- function(x, enabled = FALSE, remove_suffixes = NULL) {
  x0 <- as.character(x)

  # Always normalize small hidden differences
  out <- x0
  out <- gsub("\u00A0", " ", out, fixed = TRUE)   # non-breaking spaces
  out <- gsub("[[:space:]]+", " ", out)           # repeated spaces/tabs
  out <- trimws(out)
  out <- gsub('^"|"$', "", out)                   # remove wrapping quotes

  if (!isTRUE(enabled)) {
    return(out)
  }

  default_suffixes <- c(
    " Peak area", " Peak Area", "Peak area", "Peak Area",
    " Peak height", " Peak Height", "Peak height", "Peak Height",
    "_Area", "_Height",
    " Area", " Height",
    ".mzML", ".mzXML", ".raw", ".RAW",
    ".cdf", ".CDF",
    ".mzData", ".mzdata",
    ".wiff", ".WIFF",
    ".d", ".D"
  )

  suffixes <- unique(c(parse_suffix_list(remove_suffixes), default_suffixes))
  suffixes <- suffixes[nzchar(suffixes)]

  strip_one_suffix <- function(v, sfx) {
    n <- nchar(sfx)
    if (!is.finite(n) || n < 1) return(v)

    hit <- nchar(v) >= n &
      tolower(substr(v, nchar(v) - n + 1, nchar(v))) == tolower(sfx)

    v[hit] <- substr(v[hit], 1, nchar(v[hit]) - n)
    v <- gsub("[[:space:]]+", " ", v)
    trimws(v)
  }

  # Repeat until stable, because names can end like:
  # sample_01.mzML Peak area
  # first remove "Peak area", then remove ".mzML"
  for (pass in seq_len(20)) {
    old <- out

    for (sfx in suffixes) {
      out <- strip_one_suffix(out, sfx)
    }

    if (identical(old, out)) break
  }

  out[!nzchar(out)] <- x0[!nzchar(out)]
  out
}

make_sample_name_map <- function(sample_cols,
                                 clean_enabled = FALSE,
                                 remove_suffixes = NULL) {
  cleaned <- clean_sample_names_optional(
    sample_cols,
    enabled = clean_enabled,
    remove_suffixes = remove_suffixes
  )

  tibble::tibble(
    OriginalSample = as.character(sample_cols),
    Sample = make.unique(as.character(cleaned), sep = "_")
  )
}

guess_metadata_sample_col <- function(cols) {
  candidates <- c(
    "Sample", "sample",
    "SampleName", "sample_name",
    "Filename", "FileName", "filename",
    "File", "Name", "Injection", "Run"
  )

  hit <- candidates[candidates %in% cols]
  if (length(hit)) hit[1] else cols[1]
}

guess_metadata_label_col <- function(cols, sample_col = NULL) {
  cols2 <- setdiff(cols, sample_col)

  candidates <- c(
    "Condition", "condition",
    "Label", "label",
    "Group", "group",
    "Treatment", "treatment",
    "Class", "class"
  )

  hit <- candidates[candidates %in% cols2]
  if (length(hit)) hit[1] else cols2[1]
}

make_unique_nonconflicting_names <- function(nms, existing) {
  out <- nms

  for (i in seq_along(out)) {
    if (out[i] %in% existing || out[i] %in% out[seq_len(i - 1)]) {
      base <- paste0(out[i], "_meta")
      new <- base
      k <- 1

      while (new %in% existing || new %in% out[seq_len(i - 1)]) {
        k <- k + 1
        new <- paste0(base, "_", k)
      }

      out[i] <- new
    }
  }

  out
}

omni_status_box <- function(type = c("error", "warning", "success", "info"), text) {
  type <- match.arg(type)

  col <- switch(
    type,
    error   = "#e74c3c",
    warning = "#f39c12",
    success = "#18bc9c",
    info    = "#3498db"
  )

  bg <- switch(
    type,
    error   = "#fdecea",
    warning = "#fff4e5",
    success = "#eafaf1",
    info    = "#eef6fb"
  )

  ic <- switch(
    type,
    error   = "exclamation-triangle",
    warning = "exclamation-circle",
    success = "check-circle",
    info    = "info-circle"
  )

  div(
    style = paste0(
      "background:", bg, ";",
      "border-left:5px solid ", col, ";",
      "padding:10px; margin-top:10px; margin-bottom:10px;",
      "border-radius:5px; color:#2c3e50; font-weight:bold;"
    ),
    icon(ic),
    tags$span(style = "margin-left:6px;", text)
  )
}

#..........................................
# UI ----
#..........................................
ui <- fluidPage(
  use_waiter(),
  useShinyjs(),
  extendShinyjs(text = js_copy, functions = c("copyCode")),
  theme = shinytheme("cerulean"),
  
  tags$style(HTML("
    .highlight { background:#f8f9fa; border-left: 4px solid #2c3e50; padding:8px; border-radius:4px; font-weight:bold; }
  ")),
  
  tags$style(HTML("
    .highlight { background:#f8f9fa; border-left: 4px solid #2c3e50; padding:8px; border-radius:4px; font-weight:bold; }
    
    /* This increases the font size of the tabs */
    .nav-tabs > li > a {
      font-size: 20px !important;
      font-weight: bold;
    }
  ")),
  
  tags$style(HTML("
  .source-box {
    background: #ffffff;
    border: 1px solid #dfe6e9;
    border-left: 5px solid #007BA7;
    border-radius: 6px;
    padding: 10px 12px;
    margin-bottom: 10px;
  }

  .source-box-metadata {
    border-left-color: #18bc9c;
  }

  .source-box-samplename {
    border-left-color: #3498db;
  }

  .source-box-title {
    font-weight: bold;
    color: #2c3e50;
    margin-bottom: 5px;
  }

  .source-box-text {
    font-size: 14px;
    color: #555;
    margin-bottom: 0;
  }
")),
  
  tags$head(tags$style(HTML("
  /* Editable Labels table: prevent white-on-white editing issue */

  #labels_table.html-widget.datatables {
    background-color: transparent !important;
  }

  #labels_table .dataTables_wrapper,
  #labels_table table.dataTable,
  #labels_table .dataTables_scroll,
  #labels_table .dataTables_scrollHead,
  #labels_table .dataTables_scrollBody {
    background-color: #ffffff !important;
    color: #2c3e50 !important;
  }

  #labels_table table.dataTable th,
  #labels_table table.dataTable td {
    background-color: #ffffff !important;
    color: #2c3e50 !important;
  }

  /* Cell when focused / double-clicked / edited */
  #labels_table table.dataTable tbody td.focus,
  #labels_table table.dataTable tbody td:focus,
  #labels_table table.dataTable tbody tr.selected td,
  #labels_table table.dataTable tbody td.selected {
    background-color: #ffffff !important;
    color: #000000 !important;
    box-shadow: inset 0 0 0 2px #007BA7 !important;
  }

  /* Input box created during editing */
  #labels_table input,
  #labels_table textarea,
  #labels_table .dataTables_wrapper input,
  #labels_table .dataTables_wrapper textarea {
    background-color: #ffffff !important;
    color: #000000 !important;
    -webkit-text-fill-color: #000000 !important;
    caret-color: #000000 !important;
    border: 1px solid #007BA7 !important;
  }

  /* Keep search / info / pagination readable if shown */
  #labels_table .dataTables_length,
  #labels_table .dataTables_filter,
  #labels_table .dataTables_info,
  #labels_table .dataTables_paginate {
    color: #000000 !important;
    font-weight: bold;
    padding: 5px;
  }
"))),
  
  tags$head(tags$style(HTML("
    /* Existing Footer and layout styles */
    .app-footer { position: fixed; left:0; right:0; bottom:0; 
                  text-align:center; font-size:12px; opacity:0.75;
                  padding:8px; background: rgba(255,255,255,0.8);
                  border-top: 1px solid #ddd; z-index: 9999; }
    body { padding-bottom: 45px; }

    /* --- NEW: Thicker Upload Progress Bar --- */
    .progress.shiny-file-input-progress {
      height: 20px !important;
      margin-top: 10px !important;
      border-radius: 5px !important;
    }
    
    .progress.shiny-file-input-progress .progress-bar {
      line-height: 20px !important;
      font-size: 14px !important;
      font-weight: bold !important;
      background-color: #007BA7 !important; /* Matches your app's theme color */
    }
    /* ---------------------------------------- */

    .tooltip-inner {
      max-width: none !important;
      white-space: nowrap;
      text-align: left !important;
      font-size: 18px;
    }
    
    .shiny-output-error-validation {
      color: #e74c3c !important;  /* Flatly red */
      font-size: 18px !important; /* Make it larger */
      font-weight: bold !important;
      margin-top: 10px;
    }
    
  "))),
  
  tags$head(
    tags$title("OmniPeak"),
    tags$link(rel = "icon", type = "image/png",
              href = "https://raw.githubusercontent.com/plyush1993/OmniPeak/main/omnipeak.png")
  ),

  tags$head(tags$style(HTML("
    /* make disabled download links truly inactive */
    a.shiny-download-link.disabled, 
    .shiny-download-link.disabled {
      pointer-events: none !important;
      opacity: 0.5 !important;
      cursor: not-allowed !important;
    }
  "))),

  tags$head(
    tags$style(HTML("
      .help-block {
        color: #2c3e50 !important; 
        font-size: 16px;
        font-style: italic;
        background-color: #f8f9fa;
        padding: 5px;
        border-left: 3px solid #007bff;
      }
    "))
  ),
  
  div(
    class = "app-footer",
    HTML('Created by: Ivan Plyushchenko &nbsp;|&nbsp;
         <a href="https://github.com/plyush1993/OmniPeak" target="_blank">GitHub repository</a>')
  ),
  
  div(
    style = "width: 100%; display: flex; align-items: center; justify-content: center; margin-bottom: 20px;",
    tags$img(src = 'https://raw.githubusercontent.com/plyush1993/OmniPeak/main/omnipeak.png', height = '120px', style = 'margin-right: 20px;'),
    div(style = 'font-size: 32px; font-weight: 900; color: #007BA7; text-align: center;', "OmniPeak - Reshape Metabolomics Peak Table")
  ), 
  
  sidebarLayout(
    sidebarPanel(
      h3(class = "highlight", "1. Upload & Parse"),
      selectInput("data_type", "Data table type:", 
                  choices = c("mzMine" = "mzmine", "xcms" = "xcms", "MS-DIAL" = "msdial", "Default" = "default"),
                  selected = "mzmine"),
      fileInput("raw_file", "Upload Peak Table (*.csv)", accept = ".csv"),
      helpText(HTML("<i class='fa fa-info-circle'></i> Need data to test? Check examples in <a href='https://github.com/plyush1993/OmniPeak' target='_blank'>GitHub</a>.")),
      uiOutput("upload_tab_error"),
      
      uiOutput("global_controls"),
      
      tags$hr(),
h3(class = "highlight", "2. Metadata & Labels"),

div(
  class = "source-box source-box-metadata",

  div(class = "source-box-title", "Source: metadata file"),

prettyCheckbox(
  "add_metadata_csv",
  "Add metadata from CSV",
  value = FALSE,
  status = "primary",
  icon = icon("check"),
  animation = "jelly"
),

conditionalPanel(
  condition = "input.add_metadata_csv == true || (input.add_labels == true && input.label_source == 'from_metadata')",

  fileInput(
    "metadata_csv",
    "Upload metadata CSV with column names",
    accept = ".csv"
  ),

  uiOutput("metadata_sample_col_ui"),
  uiOutput("metadata_label_col_ui"),

  tags$hr(style = "margin-top: 10px; margin-bottom: 15px;"),
  h4("Sample Name Cleaning", style = "margin-top:0px; font-weight:bold;"),

  prettyCheckbox(
    "clean_sample_names_export",
    "Clean sample names",
    value = FALSE,
    status = "primary",
    icon = icon("check"),
    animation = "jelly"
  ),

  conditionalPanel(
    condition = "input.clean_sample_names_export == true",

    selectizeInput(
      "sample_remove_suffixes",
      "Remove suffixes/extensions:",
      choices = c(
        ".mzML", ".mzXML", ".raw", ".RAW",
        ".cdf", ".CDF",
        ".mzData", ".mzdata",
        ".wiff", ".WIFF",
        ".d", ".D",
        " Peak area", " Peak Area",
        " Peak height", " Peak Height",
        "_Area", "_Height",
        " Area", " Height"
      ),
      selected = c(
        " Peak area", " Peak height",
        "_Area", "_Height",
        " Area", " Height"
      ),
      multiple = TRUE,
      options = list(
        create = TRUE,
        createOnBlur = TRUE,
        placeholder = "Type custom suffix and press Enter"
      )
    )
  ),

uiOutput("metadata_match_message")
)),

tags$hr(),

div(
  class = "source-box source-box-samplename",
  div(class = "source-box-title", "Source: sample names or selected column"),
  
prettyCheckbox(
  "add_labels",
  "Add Label Column",
  value = FALSE,
  icon = icon("check"),
  status = "primary",
  animation = "jelly"
),

conditionalPanel(
  condition = "input.add_labels",

   radioButtons(
      "label_source",
      "Label source:",
      choiceNames = list(
        HTML("<b>From sample names</b><br><span style='font-size:12px;color:#666;'>Parse labels from file/sample names using tokens.</span>"),
        HTML("<b>From uploaded metadata column</b><br><span style='font-size:12px;color:#666;'>Select one metadata file column from above as Label.</span>"),
        HTML("<b>From custom one-column CSV</b><br><span style='font-size:12px;color:#666;'>One label per sample, same order as detected samples.</span>"),
        HTML("<b>Manual editable table</b><br><span style='font-size:12px;color:#666;'>Edit labels directly in the app.</span>")
      ),
      choiceValues = c(
        "from_rows",
        "from_metadata",
        "from_custom",
        "manual"
      ),
      selected = "from_rows"
    ),

  uiOutput("label_message"),
  
  conditionalPanel(
    condition = "input.label_source == 'from_rows' || input.label_source == 'manual'",
    numericInput("token_idx", "Main Label Token index", value = 2, min = 1),
    textInput("token_sep", "Token separator (used for all name parsing)", value = "_")
  ),

  conditionalPanel(
    condition = "input.label_source == 'from_custom'",
    fileInput("meta_csv", "Upload labels CSV", accept = ".csv")
  ),

  conditionalPanel(
    condition = "input.label_source == 'manual'",

    div(
      style = "display: inline-flex; align-items: center; gap: 6px; margin-bottom: 10px;",

      actionButton(
        "fill_manual_labels",
        label = tags$span(
          HTML("Fill editable table from<br>current token labels"),
          style = "line-height: 1.1;"
        ),
        class = "btn-primary",
        style = "
          font-size: 12px;
          padding: 4px 8px;
          line-height: 1.1;
          width: 145px;
          white-space: normal;
        "
      )
    ),

    div(
      class = "help-block",
      "Double-click cells in the Label column to edit group names."
    ),

    tags$hr(),
    DTOutput("labels_table")
  )
),
      
      prettyCheckbox("add_run_order", "Add Order by Sequence", value = FALSE, status = "primary", icon = icon("check"), animation = "jelly"),
      prettyCheckbox("add_extra_meta", "Extract Extra Variables", value = FALSE, status = "primary", icon = icon("check"), animation = "jelly"),
      conditionalPanel(
        condition = "input.add_extra_meta",
        textInput("extra_meta_names", "Variable Name(s) (comma-separated):", placeholder = "Batch, Genotype"),
        textInput("extra_meta_indices", "Token Index(es) (comma-separated):", placeholder = "1, 4")
      )),
      
      tags$hr(),
      h3(class = "highlight", "3. Export Data"),
      uiOutput("export_ui"), 
      
      tags$hr(),
      h3(class = "highlight", "4. Restore to Native"),
      fileInput("processed_file", "1. Upload Processed Tidy Data (.csv or .txt)", accept = c(".csv", ".txt")),
      fileInput("dict_file_in", "2. Upload Dictionary (.rds)", accept = ".rds"),
      uiOutput("restore_ui"),
      tags$hr(),
      actionButton("reset_app", "Reset App", icon = icon("rotate-right"), class = "btn btn-danger", style = "width: 100%; margin-bottom: 20px; font-weight: bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Preview", 
                 tags$br(),
                 uiOutput("quick_stats_ui"),
                 uiOutput("help_raw"),
                 DTOutput("preview_raw") ),
        tabPanel("Export Preview",
                 tags$br(),
                 uiOutput("quick_stats_tidy"),
                 uiOutput("help_tidy"),
                 DTOutput("preview_tidy") ),
        tabPanel("Restored Preview", 
                 tags$br(),
                 uiOutput("quick_stats_restored"),
                 uiOutput("help_restored"),
                 DTOutput("preview_restored") ),
        tabPanel("R Code Snippet", 
         tags$br(),
         div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
             downloadButton("dl_script", "Download R Script (.R)", class = "btn btn-primary"),
             actionButton("copy_script", "Copy to Clipboard", icon = icon("copy"), class = "btn btn-success")
         ),
         shinyAce::aceEditor(
           outputId = "code_display",
           value = "# Load your data to see the generated script...",
           mode = "r",               # Enables R syntax highlighting
           theme = "cobalt",         #  'RStudio' style theme
           readOnly = TRUE,          # Prevents user from typing in it
           height = "800px",
           showPrintMargin = FALSE,
           fontSize = 14
         ))
      ),
      tags$hr(),
      tabsetPanel(
        tabPanel("Instructions", icon = icon("info-circle"),
          div(style = "max-width: 900px; margin: 0 auto; padding-top: 20px;",
            h2("Welcome to OmniPeak", style = "color: #007BA7; font-weight: bold; margin-bottom: 15px;"),
            p(style = "font-size: 16px; color: #555; line-height: 1.6;", 
  HTML("
    <i style='color: #555; font-weight: bold;'>OmniPeak</i> 
    is designed to seamlessly reshape metabolomics peak tables into an analysis-ready 
    <span style='background-color: #e8f4f8; color: #007BA7; padding: 2px 6px; border-radius: 4px; font-weight: bold;'>Tidy</span> 
    format for downstream processing, and restore them back to their native formats for continuous workflows. 
  ")
),
            tags$hr(style = "border-top: 2px solid #eee;"),
            
            h3(icon("cogs"), " Workflow Steps (Sidebar)", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
            
            div(class = "well", style = "background-color: #f8f9fa; border-left: 5px solid #3498db; padding: 15px; margin-bottom: 15px;",
              h4(tags$b("1. Upload & Parse"), style = "margin-top: 0; color: #3498db;"),
              p(style = "margin-bottom: 0;", 
              HTML("Select your software source (<b><i>mzMine</i></b>, <b><i>MS-DIAL</i></b>, <b><i>xcms</i></b>, etc.) and upload your <code>.csv</code> peak table. OmniPeak automatically standardizes the columns by selected names and detects your sample data by provided keywords. You can also specify Feature ID column (which becomes Tidy headers), by default: 'mz_rt'.")
            )),
            
            div(
  class = "well",
  style = "background-color: #f8f9fa; border-left: 5px solid #18bc9c; padding: 15px; margin-bottom: 15px;",

  h4(
    tags$b("2. Metadata & Labels"),
    style = "margin-top: 0; color: #18bc9c;"
  ),

  p(
  style = "margin-bottom: 10px;",
  "Use this section to add sample information to the exported tidy table. ",
  "Metadata columns and the ",
  tags$code("Label"),
  " column are related, but they are not the same."
),

div(
  style = "background:#ffffff; border-left:4px solid #18bc9c; padding:10px; margin-bottom:10px; border-radius:5px;",
  tags$b("Metadata from CSV", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "Upload a metadata table with column names and one sample-name column. ",
    "OmniPeak matches rows by sample name and adds the remaining columns to the tidy output, for example ",
    tags$code("Condition"),
    ", ",
    tags$code("Batch"),
    ", ",
    tags$code("Patient"),
    ", or ",
    tags$code("Timepoint"),
    "."
  )
),

div(
  style = "background:#ffffff; border-left:4px solid #18bc9c; padding:10px; margin-bottom:10px; border-radius:5px;",
  tags$b("Labels from sample names", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "The optional ",
    tags$code("Label"),
    " column can be parsed directly from sample names using a separator and token index. ",
    "This is useful when group names are already encoded in the file names."
  )
),

div(
  style = "background:#ffffff; border-left:4px solid #18bc9c; padding:10px; margin-bottom:10px; border-radius:5px;",
  tags$b("Labels from metadata", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "Alternatively, the ",
    tags$code("Label"),
    " column can be taken from one selected column in the uploaded metadata table, for example ",
    tags$code("Condition"),
    " or ",
    tags$code("Treatment"),
    "."
  )
),

div(
  style = "background:#ffffff; border-left:4px solid #18bc9c; padding:10px; margin-bottom:10px; border-radius:5px;",
  tags$b("Order and extra variables from sample names", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "You can also add ",
    tags$code("Order"),
    " by detected sample sequence and extract additional variables directly from sample names, such as ",
    tags$code("Batch"),
    ", ",
    tags$code("Genotype"),
    ", or ",
    tags$code("Treatment"),
    ", using token indices."
  )
),

p(
  style = "margin-bottom: 0;",
  tags$b("Optional cleaning: ", style = "color: #18bc9c;"),
  "Use sample-name cleaning only when sample names in the peak table and metadata file differ by suffixes such as ",
  tags$code(".mzML"),
  ", ",
  tags$code("Peak area"),
  ", or ",
  tags$code("_Area"),
  "."
)
),
            
            div(class = "well", style = "background-color: #f8f9fa; border-left: 5px solid #008B8B; padding: 15px; margin-bottom: 15px;",
              h4(tags$b("3. Export Data"), style = "margin-top: 0; color: #008B8B;"),
              p("Download your shaped tidy dataset: features (peaks) as columns, samples as rows in ", tags$code(".csv")," or ",tags$code(".txt"), ". Directly compatible with ", HTML("<b><i>MetaboAnalyst</i></b> .")),
              p(style = "margin-bottom: 0;", tags$b("Crucial: ", style = "color: #d35400;"), "Always download the Dictionary ", tags$code(".rds"), " file! This acts as a memory bank for your chemical features (m/z, RT) needed for restoration later.")
            ),
          
            div(class = "well", style = "background-color: #f8f9fa; border-left: 5px solid #8968CD; padding: 15px; margin-bottom: 25px;",
              h4(tags$b("4. Restore to Native"), style = "margin-top: 0; color: #8968CD;"),
              p(style = "margin-bottom: 0;", "Upload your exported tidy dataset (", tags$code(".csv")," or ",tags$code(".txt"), ") along with your saved Dictionary", tags$code(".rds")," file. OmniPeak will reconstruct the exact native format required by your original metabolomics software, and you can download it in ", tags$code(".csv")," or ",tags$code(".txt"), ".")
            ),
            
            h3(icon("desktop"), " Understanding the Tabs", style = "color: #2c3e50; margin-bottom: 15px;"),
            tags$ul(style = "font-size: 16px; line-height: 1.8; color: #444;",
              tags$li(tags$b("Raw Preview: "), "A quick look at your initial uploaded data and parsed sample/feature counts."),
              tags$li(tags$b("Export Preview: "), "Your analysis-ready Tidy table. Verify that your labels and metadata columns look correct here before exporting."),
              tags$li(tags$b("Restored Preview: "), "Verifies that your processed data has been successfully mapped back to the original software's structure."),
              tags$li(tags$b("R Code Snippet: "), "An automatically generated R script for reading output tailored to your specific dataset.")
            ),
            tags$br(), tags$br()
          )
        ))
      
    )
  )
)

#..........................................
# Server ----
#..........................................
server <- function(input, output, session) {
  upload_error <- reactiveVal(NULL)
  
  state <- reactiveValues(
    raw_data_true = NULL,
    raw_std = NULL,
    dictionary = NULL,
    attributes = NULL,
    restored_df = NULL,
    base_name = NULL,
    restore_base_name = NULL
  )
  
  output$upload_tab_error <- renderUI({
    if (is.null(upload_error())) return(NULL)
    div(style = "color: red; font-weight: bold; margin-bottom: 10px;", upload_error())
  })
  
  # ---------------------------------------------------------
  # STEP 1: Parse Upload
  # ---------------------------------------------------------
  observeEvent(list(input$raw_file, input$data_type), {
    req(input$raw_file)
    w_up <- Waiter$new(html = spin_6(), color = "rgba(44,62,80,0.8)")
    w_up$show()
    
    tryCatch({
      type <- input$data_type
      if (type == "msdial") {
        df0 <- as.data.frame(vroom::vroom(input$raw_file$datapath, delim = ",", col_names = FALSE, col_types = vroom::cols(.default = "c"), na = ""))
      } else {
        df0 <- as.data.frame(vroom::vroom(input$raw_file$datapath, delim = ",", show_col_types = FALSE))
      }
      
      state$raw_data_true <- df0
      df_std <- standardize_peak_table(df0, type = type)
      state$raw_std <- df_std
      state$base_name <- tools::file_path_sans_ext(input$raw_file$name)
      
      state$attributes <- list(
        type = type,
        export_template = attr(df_std, "export_template"),
        export_colmap = attr(df_std, "export_colmap"),
        export_rt_factor = attr(df_std, "export_rt_factor") %||% 1,
        msdial_preamble = attr(df_std, "msdial_preamble")
      )
      upload_error(NULL)
    }, error = function(e) {
      upload_error(paste0("Parsing error: ", e$message))
      state$raw_std <- NULL
    }, finally = {
      w_up$hide()
    })
  })
  
  # ---------------------------------------------------------
  # QUICK STATS DASHBOARD & RESET BUTTON
  # ---------------------------------------------------------
  
  # 1. Reset Button Logic
  observeEvent(input$reset_app, {
    shinyjs::refresh() # Instantly refreshes the session
  })
  
  # 2. Quick Stats UI (Rendered on the 1st Tab)
  output$quick_stats_ui <- renderUI({
    # Wait until the data is fully processed before showing stats
    req(processed_std(), sample_cols())
    
    n_features <- format(nrow(processed_std()), big.mark = ",")
    n_samples  <- format(length(sample_cols()), big.mark = ",")
    
    # Render two side-by-side dashboard metric cards
    div(style = "display: flex; gap: 20px; margin-bottom: 5px;",
        div(style = "flex: 1; background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 6px solid #18bc9c; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
            h4("Total Samples Detected", style = "margin-top: 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h2(n_samples, style = "margin: 0; color: #18bc9c; font-weight: 900; font-size: 36px;")
        ),
        div(style = "flex: 1; background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 6px solid #3498db; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
            h4("Total Features (Peaks)", style = "margin-top: 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h2(n_features, style = "margin: 0; color: #3498db; font-weight: 900; font-size: 36px;")
        )
    )
  })
  
  # 3. Quick Stats for Tidy (Export) Tab
  output$quick_stats_tidy <- renderUI({
    req(tidy_data())
    df <- tidy_data()
    
    # In Tidy data, Samples are rows and Features (FIDs) are columns
    # We subtract metadata columns to get the true feature count
    meta_cols <- c("Sample", "Order", "Label", trimws(unlist(strsplit(input$extra_meta_names %||% "", ","))))
    actual_features <- length(setdiff(names(df), meta_cols))
    
    n_features <- format(actual_features, big.mark = ",")
    n_samples  <- format(nrow(df), big.mark = ",")
    
    div(style = "display: flex; gap: 20px; margin-bottom: 5px;",
        div(style = "flex: 1; background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 6px solid #18bc9c; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
            h4("Tidy Samples (Rows)", style = "margin-top: 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h2(n_samples, style = "margin: 0; color: #18bc9c; font-weight: 900; font-size: 36px;")
        ),
        div(style = "flex: 1; background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 6px solid #3498db; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
            h4("Tidy Features (Columns)", style = "margin-top: 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h2(n_features, style = "margin: 0; color: #3498db; font-weight: 900; font-size: 36px;")
        )
    )
  })

  # 4. Quick Stats for Restored Tab
  output$quick_stats_restored <- renderUI({
    req(state$restored_df)
    df <- state$restored_df
    
    n_features <- format(nrow(df), big.mark = ",")
    n_cols     <- format(ncol(df), big.mark = ",")
    
    actual_samples <- intersect(state$restored_sample_names, names(df))
    n_samples  <- format(length(actual_samples), big.mark = ",")
    
    div(style = "display: flex; gap: 20px; margin-bottom: 5px;",
        div(style = "flex: 1; background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 6px solid #f39c12; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
            h4("Restored Samples", style = "margin-top: 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h2(n_samples, style = "margin: 0; color: #f39c12; font-weight: 900; font-size: 36px;")
        ),
        div(style = "flex: 1; background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 6px solid #9b59b6; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
            h4("Restored Features", style = "margin-top: 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h2(n_features, style = "margin: 0; color: #9b59b6; font-weight: 900; font-size: 36px;")
        )
    )
  })
  
  # ---------------------------------------------------------
  # GLOBAL PARSING SETTINGS & SAMPLE MAPPING UI
  # ---------------------------------------------------------
  output$global_controls <- renderUI({
    req(state$raw_std)
    cols <- names(state$raw_std)
    
    def_id <- if (".FID" %in% cols) ".FID" else if ("feature_id" %in% cols) "Combine m/z and RT" else "feature_id"
    def_mz <- grep("(?i)^(mz|m.z|average.mz)$", cols, value = TRUE)[1]
    def_rt <- grep("(?i)^(rt|retention.time|average.rt)$", cols, value = TRUE)[1]
    
    tagList(
      tags$hr(style = "margin-top: 5px; margin-bottom: 15px;"),
      h4("Global Parsing Settings", style = "margin-top:0px; font-weight:bold;"),
      fluidRow(
        column(12, selectizeInput("id_col", "Feature ID column (becomes Tidy headers):", 
                                  choices = c("Combine m/z and RT", "Auto-generate (feat_1)", cols), 
                                  selected = def_id))
      ),
      conditionalPanel(
        condition = "input.id_col == 'Combine m/z and RT'",
        fluidRow(
          column(12, textInput("mz_rt_sep", "Separator for m/z and RT:", value = "_"))
        )
      ),
      fluidRow(
        column(6, selectizeInput("mz_col", "m/z column:", choices = c("None", cols), selected = def_mz %||% "None")),
        column(6, selectizeInput("rt_col", "RT column:", choices = c("None", cols), selected = def_rt %||% "None"))
      ),
      
      tags$hr(style = "margin-top: 5px; margin-bottom: 15px;"),
      h4("Sample Column Mapping", style = "margin-top:0px; font-weight:bold;"),
      radioButtons("sample_mode", "How to define sample columns?",
                   choices = c("Auto-detect numeric sample columns" = "auto",
                               "By keyword match" = "kws",
                               "Pick columns manually" = "manual"),
                   selected = "kws"),
      conditionalPanel(
        condition = "input.sample_mode == 'kws'",
        selectizeInput("sample_kws", "Sample column keywords:",
        choices  = c(".mzML", ".mzXML", ".raw", "_Area", "_Height", "Area", "Height"),
          selected = c(".mzML", ".mzXML"),
          multiple = TRUE,
          options  = list(create = TRUE, createOnBlur = TRUE,
                          placeholder = "Type to add (e.g. _Area) and press Enter")
        )),
      conditionalPanel(
        condition = "input.sample_mode == 'manual'",
        selectizeInput("sample_cols_manual", "Pick sample columns:",
                       choices = cols, selected = NULL, multiple = TRUE)
      )
    )
  })
  
  processed_std <- reactive({
    req(state$raw_std)
    df <- state$raw_std
    
    id_choice <- input$id_col %||% "Auto-generate (feat_1)"
    
    if (id_choice == "Combine m/z and RT") {
      mz_c <- input$mz_col
      rt_c <- input$rt_col
      sep <- input$mz_rt_sep %||% "_"
      
      if (!is.null(mz_c) && mz_c != "None" && !is.null(rt_c) && rt_c != "None") {
        mz_vals <- suppressWarnings(as.numeric(as.character(df[[mz_c]])))
        rt_vals <- suppressWarnings(as.numeric(as.character(df[[rt_c]])))
        mz_str <- ifelse(is.na(mz_vals), "NA", as.character(round(mz_vals, 4)))
        rt_str <- ifelse(is.na(rt_vals), "NA", as.character(round(rt_vals, 2)))
        df$.FID <- paste(mz_str, rt_str, sep = sep)
      } else {
        df$.FID <- paste0("feat_", seq_len(nrow(df)))
      }
    } else if (id_choice != "Auto-generate (feat_1)" && id_choice %in% names(df)) {
      df$.FID <- as.character(df[[id_choice]])
    } else {
      df$.FID <- paste0("feat_", seq_len(nrow(df)))
    }
    
    df$.FID <- make.unique(df$.FID)
    df
  })
  
  sample_cols <- reactive({
    req(processed_std())
    df <- processed_std()
    cols <- names(df)
    mode <- input$sample_mode %||% "auto"
    meta <- c(".FID", input$id_col, input$mz_col, input$rt_col, "feature_id", "mz", "rt")
    
    if (mode == "manual") {
      validate(need(length(input$sample_cols_manual) > 0, "Pick sample columns."))
      return(intersect(input$sample_cols_manual, cols))
    }
    
    if (mode == "kws") {
      kws <- input$sample_kws %||% character(0)
      idx <- multi_sample_idx(cols, kws)
      validate(need(length(idx) > 0, "No sample columns matched the keywords."))
      sc <- cols[idx]
      return(setdiff(sc, meta))
    }
    
    cand <- setdiff(cols, meta)
    cand <- cand[!grepl("^row\\b", cand, ignore.case = TRUE)] 
    prop_num <- vapply(df[cand], function(x) {
      x2 <- suppressWarnings(as.numeric(as.character(x)))
      mean(is.finite(x2), na.rm = TRUE)
    }, numeric(1))
    sc <- cand[prop_num >= 0.7] 
    validate(need(length(sc) > 0, "Auto-detect found no numeric columns."))
    sc
  })
  
  sample_name_map <- reactive({
  req(sample_cols())

  make_sample_name_map(
    sample_cols = sample_cols(),
    clean_enabled = isTRUE(input$clean_sample_names_export),
    remove_suffixes = input$sample_remove_suffixes %||% ""
  )
})

uploaded_metadata_raw <- reactive({
  req(
    isTRUE(input$add_metadata_csv) ||
      (isTRUE(input$add_labels) && identical(input$label_source, "from_metadata")),
    input$metadata_csv
  )

  as.data.frame(
    vroom::vroom(
      input$metadata_csv$datapath,
      delim = ",",
      col_names = TRUE,
      show_col_types = FALSE
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})

output$metadata_sample_col_ui <- renderUI({
  req(uploaded_metadata_raw())

  cols <- names(uploaded_metadata_raw())

  selectInput(
    "metadata_sample_col",
    "Metadata sample-name column:",
    choices = cols,
    selected = guess_metadata_sample_col(cols)
  )
})

output$metadata_label_col_ui <- renderUI({
  req(uploaded_metadata_raw())

  if (!isTRUE(input$add_labels) || !identical(input$label_source, "from_metadata")) {
    return(NULL)
  }

  cols <- names(uploaded_metadata_raw())

  sample_col <- input$metadata_sample_col %||% guess_metadata_sample_col(cols)

  choices <- setdiff(cols, sample_col)

  validate(
    need(length(choices) > 0, "Metadata file has no column available for labels.")
  )

  selectizeInput(
    "metadata_label_col",
    "Metadata column to use as Label/Condition:",
    choices = choices,
    selected = guess_metadata_label_col(cols, sample_col),
    multiple = FALSE,
    options = list(
      placeholder = "Select metadata column for labels"
    )
  )
})

uploaded_metadata_aligned <- reactive({
  req(uploaded_metadata_raw(), sample_name_map(), input$metadata_sample_col)

  meta <- uploaded_metadata_raw()
  smap <- sample_name_map()

  validate(
    need(input$metadata_sample_col %in% names(meta),
         "Selected metadata sample column was not found.")
  )

  meta_key <- clean_sample_names_optional(
    meta[[input$metadata_sample_col]],
    enabled = isTRUE(input$clean_sample_names_export),
    remove_suffixes = input$sample_remove_suffixes %||% ""
  )

  app_key <- smap$Sample

  validate(
    need(!anyDuplicated(meta_key),
         "Metadata sample names are duplicated after optional cleaning.")
  )

  idx <- match(app_key, meta_key)

  if (any(is.na(idx))) {
    missing_samples <- app_key[is.na(idx)]

    validate(
      need(
        FALSE,
        paste0(
          "Metadata file is missing these sample names: ",
          paste(head(missing_samples, 10), collapse = ", "),
          if (length(missing_samples) > 10) " ..." else ""
        )
      )
    )
  }

  meta[idx, , drop = FALSE]
})

output$metadata_match_message <- renderUI({
  active <- isTRUE(input$add_metadata_csv) ||
    (isTRUE(input$add_labels) && identical(input$label_source, "from_metadata"))

  if (!active) return(NULL)

  if (is.null(input$raw_file)) {
    return(omni_status_box(
      "warning",
      "Upload a peak table first, so OmniPeak can detect sample names."
    ))
  }

  if (is.null(input$metadata_csv)) {
    return(omni_status_box(
      "warning",
      "Upload a metadata CSV file with sample names and metadata columns."
    ))
  }

  if (is.null(input$metadata_sample_col) || !nzchar(input$metadata_sample_col)) {
    return(omni_status_box(
      "warning",
      "Select the metadata column that contains sample names."
    ))
  }

  err <- NULL

  aligned <- tryCatch(
    uploaded_metadata_aligned(),
    shiny.silent.error = function(e) {
      err <<- conditionMessage(e)
      NULL
    },
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )

  if (!is.null(err) && nzchar(err)) {
    return(omni_status_box("error", err))
  }

  if (!is.null(err)) {
    return(omni_status_box(
      "warning",
      "Metadata cannot be matched yet. Check the selected sample-name column."
    ))
  }

  n_meta_cols <- max(0, ncol(aligned) - 1)

  omni_status_box(
    "success",
    sprintf(
      "Metadata matched successfully: %d samples. Metadata columns available: %d.",
      nrow(aligned),
      n_meta_cols
    )
  )
})

uploaded_metadata_to_add <- reactive({
  req(uploaded_metadata_aligned())

  meta <- uploaded_metadata_aligned()
  sample_col <- input$metadata_sample_col

  meta <- meta[, setdiff(names(meta), sample_col), drop = FALSE]

  as.data.frame(meta, check.names = FALSE, stringsAsFactors = FALSE)
})
  
  output$sample_cols_status <- renderUI({
    sc <- try(sample_cols(), silent = TRUE)
    if (!inherits(sc, "try-error")) div(style="color:green; font-weight:bold;", sprintf("Detected %d sample columns.", length(sc)))
  })
  
 
  output$labels_table <- DT::renderDT({
  req(sample_cols())

  if (!isTRUE(input$add_labels)) return(NULL)
  if (!identical(input$label_source, "manual")) return(NULL)

  tbl <- manual_labels()
  req(tbl)

  DT::datatable(
    tbl,
    editable = list(
      target = "cell",
      disable = list(columns = c(0)) # lock Sample column
    ),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "250px",
      ordering = FALSE,
      searching = FALSE
    ),
    rownames = FALSE
  )
}, server = FALSE)
  
  manual_labels <- reactiveVal(NULL)

auto_label_table <- reactive({
  req(sample_name_map())

  smap <- sample_name_map()

  make_label_table(
    smap$Sample,
    labels_from_sample_names_or_raw(
      smap$Sample,
      token_sep = input$token_sep %||% "_",
      token_index = input$token_idx %||% 2
    )
  )
})

observeEvent(sample_name_map(), {
  req(auto_label_table())
  manual_labels(auto_label_table())
}, ignoreInit = FALSE)

observeEvent(input$fill_manual_labels, {
  req(auto_label_table())

  manual_labels(auto_label_table())

  showNotification(
    "Editable label table was filled from current token labels.",
    type = "message",
    duration = 3
  )
}, ignoreInit = TRUE)

observeEvent(input$labels_table_cell_edit, {
  info <- input$labels_table_cell_edit

  tbl <- manual_labels()
  req(tbl)

  row_i <- as.integer(info$row)

  if (!is.finite(row_i) || row_i < 1 || row_i > nrow(tbl)) {
    showNotification("Edited row is outside label table.", type = "error", duration = 3)
    return(NULL)
  }

  tbl$Label[row_i] <- trimws(as.character(info$value))

  manual_labels(tbl)

  showNotification(
    paste0("Label updated: ", tbl$Sample[row_i], " -> ", tbl$Label[row_i]),
    type = "message",
    duration = 2
  )
}, ignoreInit = TRUE)
  
labels_vec <- reactive({
  req(sample_name_map())

  if (!isTRUE(input$add_labels)) return(NULL)

  smap <- sample_name_map()
  src <- input$label_source %||% "from_rows"

  if (identical(src, "from_custom")) {

    req(input$meta_csv)

    vec <- vroom::vroom(
      input$meta_csv$datapath,
      col_names = FALSE,
      delim = ",",
      show_col_types = FALSE
    ) |>
      dplyr::pull(1)

    validate(
      need(length(vec) == nrow(smap), "Label count mismatch.")
    )

    trimws(as.character(vec))

  } else if (identical(src, "from_metadata")) {

    req(uploaded_metadata_aligned(), input$metadata_label_col)

    meta <- uploaded_metadata_aligned()

    validate(
      need(input$metadata_label_col %in% names(meta),
           "Selected metadata label column was not found.")
    )

    vec <- trimws(as.character(meta[[input$metadata_label_col]]))

    validate(
      need(!any(is.na(vec) | vec == ""),
           "Selected metadata label column contains empty values.")
    )

    vec

  } else if (identical(src, "manual")) {

    tbl <- manual_labels()
    req(tbl)

    validate(
      need(nrow(tbl) == nrow(smap),
           "Manual label table must match the number of samples."),
      need(identical(as.character(tbl$Sample), as.character(smap$Sample)),
           "Manual label table does not match current sample names."),
      need(!any(is.na(tbl$Label) | trimws(tbl$Label) == ""),
           "All samples must have labels.")
    )

    trimws(as.character(tbl$Label))

  } else {

    labels_from_sample_names(
      smap$Sample,
      token_sep = input$token_sep,
      token_index = input$token_idx
    )
  }
})

output$label_message <- renderUI({
  if (!isTRUE(input$add_labels)) return(NULL)

  src <- input$label_source %||% "from_rows"

  if (identical(src, "from_custom") && is.null(input$meta_csv)) {
    return(omni_status_box(
      "warning",
      "Upload a one-column labels CSV file, one label per detected sample."
    ))
  }

  if (identical(src, "from_metadata") && is.null(input$metadata_csv)) {
    return(omni_status_box(
      "warning",
      "Upload a metadata CSV file first, then choose a metadata column for Label."
    ))
  }

  if (identical(src, "from_metadata") &&
      (is.null(input$metadata_label_col) || !nzchar(input$metadata_label_col))) {
    return(omni_status_box(
      "warning",
      "Select the metadata column to use as Label/Condition."
    ))
  }

  err <- NULL

  vec <- tryCatch(
    labels_vec(),
    shiny.silent.error = function(e) {
      err <<- conditionMessage(e)
      NULL
    },
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )

  if (!is.null(err) && nzchar(err)) {
    return(omni_status_box("error", err))
  }

  if (!is.null(err) || is.null(vec)) return(NULL)

  vec <- trimws(as.character(vec))

  if (any(is.na(vec) | vec == "")) {
    return(omni_status_box(
      "error",
      "Some labels are empty. Check token parsing, metadata column, custom CSV, or manual table."
    ))
  }

  omni_status_box(
    "success",
    sprintf(
      "Labels ready: %d samples, %d unique label(s).",
      length(vec),
      length(unique(vec))
    )
  )
})

  # ---------------------------------------------------------
  # BUILD TIDY EXPORT
  # ---------------------------------------------------------
  tidy_data <- reactive({
    req(processed_std(), sample_cols())
    
    # 1. Show the spinner when transposition and tidying begins
    waiter_show(html = spin_6(), color = "rgba(44,62,80,0.8)")
    
    # 2. Guarantee it hides the moment this reactive finishes or fails
    on.exit(waiter_hide())
    
    df <- processed_std()
    smap <- sample_name_map()
    sc <- smap$OriginalSample
    sample_out <- smap$Sample
    
    meta_cols <- setdiff(names(df), sc)
    state$dictionary <- df[, meta_cols, drop = FALSE]
    
    mat_only <- df[, c(".FID", sc), drop = FALSE]
    tidy_mat <- as.data.frame(data.table::transpose(mat_only[, -1]), stringsAsFactors = FALSE)
    colnames(tidy_mat) <- mat_only$.FID
    rownames(tidy_mat) <- sample_out
    
    tidy_mat[] <- lapply(tidy_mat, function(x) {
      num_val <- suppressWarnings(as.numeric(as.character(x)))
      #num_val[is.na(num_val)] <- 0 # replace NA
      num_val
    })
    tidy_df <- tibble::rownames_to_column(tidy_mat, var = "Sample")
    
    if (isTRUE(input$add_labels)) {
  labs <- labels_vec()

  validate(
    need(!is.null(labs), "Label generation failed. Check label settings."),
    need(
      length(labs) == nrow(tidy_df),
      sprintf("Label count (%d) must match number of samples (%d).",
              length(labs), nrow(tidy_df))
    )
  )

  tidy_df <- tibble::add_column(tidy_df, Label = labs, .after = "Sample")
}
    
if (isTRUE(input$add_metadata_csv)) {
  uploaded_meta <- uploaded_metadata_to_add()

  validate(
    need(
      nrow(uploaded_meta) == nrow(tidy_df),
      sprintf("Metadata rows (%d) must match number of samples (%d).",
              nrow(uploaded_meta), nrow(tidy_df))
    )
  )

  uploaded_meta <- as.data.frame(
    uploaded_meta,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  names(uploaded_meta) <- make_unique_nonconflicting_names(
    names(uploaded_meta),
    existing = names(tidy_df)
  )

  insert_after <- if ("Label" %in% names(tidy_df)) "Label" else "Sample"

  for (nm in rev(names(uploaded_meta))) {
    tidy_df <- tibble::add_column(
      tidy_df,
      !!!setNames(list(uploaded_meta[[nm]]), nm),
      .after = insert_after
    )
  }
}
    
    if (isTRUE(input$add_run_order)) {
      insert_after <- if ("Label" %in% names(tidy_df)) "Label" else "Sample"
      tidy_df <- tibble::add_column(tidy_df, Order = seq_along(sc), .after = insert_after)
    }
    
    if (isTRUE(input$add_extra_meta) && nzchar(input$extra_meta_names) && nzchar(input$extra_meta_indices)) {
      meta_names <- trimws(unlist(strsplit(input$extra_meta_names, ",")))
      meta_idx <- as.integer(trimws(unlist(strsplit(input$extra_meta_indices, ","))))
      
      if (length(meta_names) == length(meta_idx) && !any(is.na(meta_idx))) {
        sep <- input$token_sep %||% "_"
        for (i in seq_along(meta_names)) {
          extracted_vals <- vapply(sample_out, function(s) {
            tokens <- unlist(strsplit(s, split = sep, fixed = TRUE))
            if (meta_idx[i] <= length(tokens)) tokens[meta_idx[i]] else "Unknown"
          }, character(1))
          
          last_col <- if (i == 1) {
             if ("Order" %in% names(tidy_df)) "Order" else if ("Label" %in% names(tidy_df)) "Label" else "Sample"
          } else { meta_names[i - 1] }
          
          tidy_df <- tibble::add_column(tidy_df, !!sym(meta_names[i]) := unname(extracted_vals), .after = last_col)
        }
      }
    }
    tidy_df
  })
  
metadata_df <- reactive({
  req(tidy_data(), processed_std())

  df <- tidy_data()
  feature_ids <- as.character(processed_std()$.FID)

  feature_cols <- intersect(names(df), feature_ids)
  meta_cols <- setdiff(names(df), feature_cols)

  df[, meta_cols, drop = FALSE]
})
  
  # --- UPDATED: Split export buttons for Tidy CSV/TXT ---
  output$export_ui <- renderUI({
    req(tidy_data(), state$dictionary, state$attributes)
    tagList(
      fluidRow(
        column(6, downloadButton("dl_tidy_csv", "1. Tidy CSV", class = "btn btn-info", style="width:100%; margin-bottom:5px;")),
        column(6, downloadButton("dl_tidy_txt", "1. Tidy TXT", class = "btn btn-info", style="width:100%; margin-bottom:5px;"))
      ),
      downloadButton("dl_dict", "2. Download Dictionary (.rds)", class = "btn btn-info", style="width:100%;"),
      tags$hr(),
      fluidRow(
        column(6, downloadButton("dl_meta_csv", "Metadata CSV", class = "btn btn-info", style="width:100%; margin-bottom:5px;")),
        column(6, downloadButton("dl_meta_txt", "Metadata TXT", class = "btn btn-info", style="width:100%; margin-bottom:5px;"))
      )
    )
  })
  
  output$dl_tidy_csv <- downloadHandler(
    filename = function() {
      req(state$base_name)
      has_extra_meta <- isTRUE(input$add_run_order) || isTRUE(input$add_extra_meta)
      has_label <- isTRUE(input$add_labels)
      suffix <- if (has_extra_meta) "_tidy_meta.csv" else if (has_label) "_tidy_label.csv" else "_tidy.csv"
      paste0(state$base_name, suffix)
    },
    content = function(file) { write.csv(tidy_data(), file, row.names = FALSE) }
  )

  output$dl_tidy_txt <- downloadHandler(
    filename = function() {
      req(state$base_name)
      has_extra_meta <- isTRUE(input$add_run_order) || isTRUE(input$add_extra_meta)
      has_label <- isTRUE(input$add_labels)
      suffix <- if (has_extra_meta) "_tidy_meta.txt" else if (has_label) "_tidy_label.txt" else "_tidy.txt"
      paste0(state$base_name, suffix)
    },
    content = function(file) { write.table(tidy_data(), file, sep = "\t", row.names = FALSE, quote = TRUE) }
  )
  
output$dl_dict <- downloadHandler(
  filename = function() {
    req(state$base_name)
    paste0(state$base_name, "_dictionary.rds")
  },
  content = function(file) {
    saveRDS(
      list(
        dictionary = state$dictionary,
        attributes = state$attributes,
        base_name = state$base_name,
        orig_sample_names = sample_cols(),
        sample_name_map = sample_name_map()
      ),
      file
    )
  }
)
  
  output$dl_meta_csv <- downloadHandler(
    filename = function() {
      req(state$base_name)
      paste0(state$base_name, "_metadata.csv")
    },
    content = function(file) { 
      write.csv(metadata_df(), file, row.names = FALSE) 
    }
  )

  output$dl_meta_txt <- downloadHandler(
    filename = function() {
      req(state$base_name)
      paste0(state$base_name, "_metadata.txt")
    },
    content = function(file) { 
      write.table(metadata_df(), file, sep = "\t", row.names = FALSE, quote = TRUE) 
    }
  )
  
  # ---------------------------------------------------------
  # RESTORE FROM ANY SESSION
  # ---------------------------------------------------------
  observeEvent(list(input$processed_file, input$dict_file_in), {
    req(input$processed_file, input$dict_file_in)
    
    # 1. Initialize and show the spinner
    w_res <- Waiter$new(html = spin_6(), color = "rgba(44,62,80,0.8)")
    w_res$show()
    
    tryCatch({
      dict_data <- readRDS(input$dict_file_in$datapath)
      saved_dict <- dict_data$dictionary
      saved_attr <- dict_data$attributes
      
      if (!is.null(dict_data$base_name)) {
        state$restore_base_name <- dict_data$base_name
      } else {
        clean_name <- tools::file_path_sans_ext(input$processed_file$name)
        state$restore_base_name <- gsub("_tidy(_label|_meta)?$", "", clean_name)
      }
      
      proc_tidy <- as.data.frame(vroom::vroom(input$processed_file$datapath, show_col_types = FALSE))
      
      samp_col_name <- if ("Sample" %in% names(proc_tidy)) "Sample" else names(proc_tidy)[1]
      sample_names <- proc_tidy[[samp_col_name]]
      
      sample_names_exported <- as.character(sample_names)
      sample_names_native <- sample_names_exported
      
      if (!is.null(dict_data$sample_name_map)) {
        smap <- as.data.frame(dict_data$sample_name_map, stringsAsFactors = FALSE)
      
        if (all(c("OriginalSample", "Sample") %in% names(smap))) {
          mi <- match(sample_names_exported, as.character(smap$Sample))
          ok <- !is.na(mi)
      
          sample_names_native[ok] <- as.character(smap$OriginalSample[mi[ok]])
        }
      }
      
      state$restored_sample_names <- sample_names_native
      
      valid_features <- intersect(names(proc_tidy), saved_dict$.FID)
      if (length(valid_features) == 0) stop("No matching feature columns found.")
      
      mat_only <- proc_tidy[, valid_features, drop = FALSE]
      
      restored_mat <- as.data.frame(data.table::transpose(mat_only), stringsAsFactors = FALSE)
      colnames(restored_mat) <- sample_names_native
      restored_mat$.FID <- valid_features
      
      rebuilt_df <- dplyr::inner_join(saved_dict, restored_mat, by = ".FID")
      
      final_native <- format_final_table_as_input(
        final_df_with_fid = rebuilt_df,
        type = saved_attr$type,
        export_template = saved_attr$export_template,
        export_colmap = saved_attr$export_colmap,
        export_rt_factor = saved_attr$export_rt_factor
      )
      
      attr(final_native, "msdial_preamble") <- saved_attr$msdial_preamble
      state$restored_df <- final_native
      showNotification("Data perfectly restored using Dictionary!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Restore failed:", e$message), type = "error")
    }, finally = {
      # 2. Make sure the spinner ALWAYS hides, even if an error crashes the restore
      w_res$hide()
    })
  })
  
  # --- UPDATED: Split export buttons for Restored CSV/TXT ---
  output$restore_ui <- renderUI({
    req(state$restored_df)
    fluidRow(
      column(6, downloadButton("dl_restored_csv", "Restored CSV", class = "btn btn-info", style="width:100%; margin-top:10px;")),
      column(6, downloadButton("dl_restored_txt", "Restored TXT", class = "btn btn-info", style="width:100%; margin-top:10px;"))
    )
  })
  
  output$dl_restored_csv <- downloadHandler(
    filename = function() { req(state$restore_base_name); paste0(state$restore_base_name, "_restored_native.csv") },
    content = function(file) {
      attr_mem <- readRDS(input$dict_file_in$datapath)$attributes
      write_final_table(path = file, df_export = state$restored_df, type = attr_mem$type, msdial_preamble = attr_mem$msdial_preamble, sep = ",")
    }
  )

  output$dl_restored_txt <- downloadHandler(
    filename = function() { req(state$restore_base_name); paste0(state$restore_base_name, "_restored_native.txt") },
    content = function(file) {
      attr_mem <- readRDS(input$dict_file_in$datapath)$attributes
      write_final_table(path = file, df_export = state$restored_df, type = attr_mem$type, msdial_preamble = attr_mem$msdial_preamble, sep = "\t")
    }
  )
  
  # ---------------------------------------------------------
  # PREVIEWS
  # ---------------------------------------------------------
  output$preview_raw <- renderDT({ 
    validate(need(is.null(upload_error()), upload_error()))
    req(state$raw_data_true)
    datatable(fast_preview(state$raw_data_true), options = list(scrollX = TRUE))
  })
  
  output$preview_tidy <- renderDT({ 
    validate(need(is.null(upload_error()), upload_error()))
    req(tidy_data())
    datatable(fast_preview(tidy_data()), options = list(scrollX = TRUE)) 
  })
  
  output$preview_restored <- renderDT({ 
    req(state$restored_df)
    df <- state$restored_df
    
    display_names <- colnames(df)
    
    # Check if there is an MS-DIAL preamble to attach
    preamble <- attr(df, "msdial_preamble")
    if (!is.null(preamble) && nrow(preamble) > 0) {
      pre <- as.data.frame(preamble, check.names = FALSE, stringsAsFactors = FALSE)
      
      # Force column names to match so we can rbind smoothly
      if (ncol(pre) == ncol(df)) {
        colnames(pre) <- colnames(df)
      } else {
        common <- intersect(colnames(pre), colnames(df))
        pre <- pre[, common, drop = FALSE]
        missing_cols <- setdiff(colnames(df), colnames(pre))
        if (length(missing_cols) > 0) pre[missing_cols] <- ""
        pre <- pre[, colnames(df), drop = FALSE]
      }
      
      # Convert everything to character to avoid class mismatch errors during rbind
      pre_char <- pre
      pre_char[] <- lapply(pre_char, as.character)
      
      df_char <- df
      df_char[] <- lapply(df_char, as.character)
      
      # Bind the preamble rows on top of the data rows
      df <- rbind(pre_char, df_char)
      
      # Clean up MS-DIAL display names (remove the _1, _2 suffixes we added to prevent duplicates)
      display_names <- gsub("_[0-9]+$", "", colnames(df))
    }
    
    datatable(fast_preview(df), colnames = display_names, options = list(scrollX = TRUE)) 
  })
  
  # ---------------------------------------------------------
  # DYNAMIC HELP TEXTS
  # ---------------------------------------------------------
  output$help_raw <- renderUI({
    # Only render if data is loaded and there are no parsing errors
    req(state$raw_data_true)
    if (!is.null(upload_error())) return(NULL)
    
    tagList(
      tags$hr(style = "margin-top: 10px; margin-bottom: 20px;"),
      helpText("Preview only (truncated)")
    )
  })
  
  output$help_tidy <- renderUI({
    # Make sure we have raw data and no parsing errors first
    req(state$raw_data_true)
    if (!is.null(upload_error())) return(NULL)
    
    # Check if tidy_data evaluates successfully (no validation errors)
    td <- try(tidy_data(), silent = TRUE)
    req(!inherits(td, "try-error"))
    
    tagList(
      tags$hr(style = "margin-top: 10px; margin-bottom: 20px;"),
      helpText("Preview only (truncated)")
    )
  })
  
  output$help_restored <- renderUI({
    # Only render if a dataset has been successfully restored
    req(state$restored_df)
    
    tagList(
      tags$hr(style = "margin-top: 10px; margin-bottom: 20px;"),
      helpText("Preview only (truncated)")
    )
  })
  
  # ---------------------------------------------------------
  # SCRIPT
  # ---------------------------------------------------------
  generated_script <- reactive({
  req(input$raw_file)

  td <- try(tidy_data(), silent = TRUE)
  req(!inherits(td, "try-error"))

  # Detect all metadata columns directly from the final tidy table.
  # This includes:
  # Sample
  # Label
  # Order
  # manually extracted metadata
  # uploaded metadata CSV columns
  feature_ids_script <- try(as.character(processed_std()$.FID), silent = TRUE)

  if (!inherits(feature_ids_script, "try-error")) {
    feature_cols_script <- intersect(names(td), feature_ids_script)
    meta_cols <- setdiff(names(td), feature_cols_script)
  } else {
    # fallback
    meta_cols <- c(
      "Sample",
      "Order",
      "Label",
      trimws(unlist(strsplit(input$extra_meta_names %||% "", ",")))
    )
    meta_cols <- intersect(meta_cols, names(td))
  }

  meta_cols <- unique(meta_cols)
  meta_cols <- meta_cols[nzchar(meta_cols)]

  has_extra_meta <- length(setdiff(meta_cols, c("Sample", "Label"))) > 0
  has_label <- "Label" %in% meta_cols

  base_suffix <- if (has_extra_meta) "_tidy_meta" else if (has_label) "_tidy_label" else "_tidy"

  file_csv <- paste0(state$base_name, base_suffix, ".csv")
  file_txt <- paste0(state$base_name, base_suffix, ".txt")

  meta_csv <- paste0(state$base_name, "_metadata.csv")
  meta_txt <- paste0(state$base_name, "_metadata.txt")

  meta_cols_txt <- paste(shQuote(meta_cols), collapse = ", ")

  paste0(
    "# ..................................................................\n",
    "# Reading OmniPeak Output For: ", state$base_name, " ----\n",
    "# ..................................................................\n\n",

    "# 1. Load required packages\n",
    "if (!require('dplyr', quietly = TRUE)) install.packages('dplyr')\n",
    "if (!require('readr', quietly = TRUE)) install.packages('readr')\n",
    "if (!require('tibble', quietly = TRUE)) install.packages('tibble')\n",
    "library(dplyr)\n",
    "library(readr)\n",
    "library(tibble)\n\n",

    "# 2. Load the tidy dataset and metadata\n",
    "# --- If you downloaded the CSV files: ---\n",
    "df <- read_csv('", file_csv, "', show_col_types = TRUE) %>%\n",
    "  column_to_rownames('Sample')\n\n",

    "meta_df <- read_csv('", meta_csv, "', show_col_types = TRUE) %>%\n",
    "  column_to_rownames('Sample')\n\n",

    "# --- If you downloaded the TXT files: ---\n",
    "df <- read_tsv('", file_txt, "', show_col_types = TRUE) %>%\n",
    "  column_to_rownames('Sample')\n\n",

    "meta_df <- read_tsv('", meta_txt, "', show_col_types = TRUE) %>%\n",
    "  column_to_rownames('Sample')\n\n",

    "# 3. Define metadata columns\n",
    "meta_cols <- c(", meta_cols_txt, ")\n\n",
    "meta_cols_no_sample <- setdiff(meta_cols, 'Sample')\n\n",

    "# 4. Separate metadata from peak table\n",
    "metadata <- df %>% select(any_of(meta_cols_no_sample))\n",
    "ds <- df %>% select(-any_of(meta_cols_no_sample))\n\n"
  )
})
  
  # 2. Update the Ace Editor UI component
  observe({
    updateAceEditor(session, "code_display", value = generated_script())
  })

  # 3. Create the Download Handler for the .R file
  output$dl_script <- downloadHandler(
    filename = function() {
      req(state$base_name)
      paste0(state$base_name, "_analysis_script.R")
    },
    content = function(file) {
      writeLines(generated_script(), file)
    }
  )
  
  # 4. Trigger the JS Copy function
  observeEvent(input$copy_script, {
    # Send the generated R script text directly to the JS clipboard function
    js$copyCode(generated_script())
  })
  
}

#..........................................
# Run ----
#..........................................
shinyApp(ui, server)