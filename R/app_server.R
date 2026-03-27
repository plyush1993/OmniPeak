#' @import shiny
#' @import shinythemes
#' @import shinyjs
#' @import DT
#' @import vroom
#' @import dplyr
#' @import data.table
#' @import shinyWidgets
#' @import shinyAce
#' @import waiter
#' @import tibble
#' @import crayon
app_server <- function(input, output, session) {
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
      ),
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

  output$sample_cols_status <- renderUI({
    sc <- try(sample_cols(), silent = TRUE)
    if (!inherits(sc, "try-error")) div(style="color:green; font-weight:bold;", sprintf("Detected %d sample columns.", length(sc)))
  })

  labels_vec <- reactive({
    req(sample_cols())
    if (!isTRUE(input$add_labels)) return(NULL)
    if (input$label_source == "from_custom") {
      req(input$meta_csv)
      vec <- vroom::vroom(input$meta_csv$datapath, col_names = FALSE, delim = ",") |> dplyr::pull(1)
      validate(need(length(vec) == length(sample_cols()), "Label count mismatch."))
      return(as.character(vec))
    } else {
      labels_from_sample_names(sample_cols(), token_sep = input$token_sep, token_index = input$token_idx)
    }
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
    sc <- sample_cols()

    meta_cols <- setdiff(names(df), sc)
    state$dictionary <- df[, meta_cols, drop = FALSE]

    mat_only <- df[, c(".FID", sc), drop = FALSE]
    tidy_mat <- as.data.frame(data.table::transpose(mat_only[, -1]), stringsAsFactors = FALSE)
    colnames(tidy_mat) <- mat_only$.FID
    rownames(tidy_mat) <- sc

    tidy_mat[] <- lapply(tidy_mat, function(x) {
      num_val <- suppressWarnings(as.numeric(as.character(x)))
      #num_val[is.na(num_val)] <- 0 # replace NA
      num_val
    })
    tidy_df <- tibble::rownames_to_column(tidy_mat, var = "Sample")

    labs <- try(labels_vec(), silent = TRUE)
    if (!inherits(labs, "try-error") && !is.null(labs)) {
      tidy_df <- tibble::add_column(tidy_df, Label = labs, .after = "Sample")
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
          extracted_vals <- vapply(sc, function(s) {
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
    req(tidy_data())
    df <- tidy_data()

    # Identify all possible metadata columns
    meta_cols <- c("Sample", "Order", "Label",
                   trimws(unlist(strsplit(input$extra_meta_names %||% "", ","))))

    # Subset only the ones that actually exist in the current tidy_df
    actual_meta_cols <- intersect(meta_cols, names(df))
    df[, actual_meta_cols, drop = FALSE]
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
    filename = function() { req(state$base_name); paste0(state$base_name, "_dictionary.rds") },
    content = function(file) { saveRDS(list(dictionary = state$dictionary, attributes = state$attributes, base_name = state$base_name, orig_sample_names = sample_cols()), file) }
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
      state$restored_sample_names <- dict_data$orig_sample_names

      if (!is.null(dict_data$base_name)) {
        state$restore_base_name <- dict_data$base_name
      } else {
        clean_name <- tools::file_path_sans_ext(input$processed_file$name)
        state$restore_base_name <- gsub("_tidy(_label|_meta)?$", "", clean_name)
      }

      proc_tidy <- as.data.frame(vroom::vroom(input$processed_file$datapath, show_col_types = FALSE))

      samp_col_name <- if ("Sample" %in% names(proc_tidy)) "Sample" else names(proc_tidy)[1]
      sample_names <- proc_tidy[[samp_col_name]]

      valid_features <- intersect(names(proc_tidy), saved_dict$.FID)
      if (length(valid_features) == 0) stop("No matching feature columns found.")

      mat_only <- proc_tidy[, valid_features, drop = FALSE]

      restored_mat <- as.data.frame(data.table::transpose(mat_only), stringsAsFactors = FALSE)
      colnames(restored_mat) <- sample_names
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

    # Safely evaluate tidy_data so the app doesn't crash on initial load
    td <- try(tidy_data(), silent = TRUE)
    req(!inherits(td, "try-error"))

    meta_cols <- c("Sample", "Order", "Label",
                   trimws(unlist(strsplit(input$extra_meta_names %||% "", ","))))

    has_extra_meta <- isTRUE(input$add_run_order) || isTRUE(input$add_extra_meta)
    has_label <- isTRUE(input$add_labels)

    base_suffix <- if (has_extra_meta) "_tidy_meta" else if (has_label) "_tidy_label" else "_tidy"

    file_csv <- paste0(state$base_name, base_suffix, ".csv")
    file_txt <- paste0(state$base_name, base_suffix, ".txt")

    meta_csv <- paste0(state$base_name, "_metadata.csv")
    meta_txt <- paste0(state$base_name, "_metadata.txt")

    paste0(
      "# ..................................................................\n",
      "# Reading OmniPeak Output For: ", state$base_name, " ----", "\n",
      "# ..................................................................\n\n",

      "# 1. Load Required Packages\n",
      "if (!require('dplyr', quietly = TRUE)) install.packages('dplyr')\n",
      "if (!require('readr', quietly = TRUE)) install.packages('readr')\n",
      "if (!require('tibble', quietly = TRUE)) install.packages('tibble')\n",
      "library(dplyr)\n",
      "library(readr)\n",
      "library(tibble)\n\n",

      "# 2. Load the Tidy dataset and Metadata\n",
      "# --- If you downloaded the CSV files: ---\n",
      "df <- read_csv('", file_csv, "', show_col_types = TRUE) %>%\n",
      "  column_to_rownames('Sample') \n",
      "meta_df <- read_csv('", meta_csv, "', show_col_types = TRUE) %>%\n",
      "  column_to_rownames('Sample') \n",

      "# --- If you downloaded the TXT files: ---\n",
      "df <- read_tsv('", file_txt, "', show_col_types = TRUE) %>% \n",
      "  column_to_rownames('Sample')\n",
      "meta_df <- read_tsv('", meta_txt, "', show_col_types = TRUE) %>% \n",
      "  column_to_rownames('Sample')\n",

      "# 3. Define Metadata Columns\n",
      "meta_cols <- c(", paste(shQuote(intersect(meta_cols, names(td))), collapse=", "), ")\n\n",

      "# 4. Data Type Formatting\n",
      "df <- df %>% mutate(\n",
      "  across(any_of('Label'), as.factor),\n",
      "  across(any_of(c('Order', 'Batch')), as.numeric)\n",
      ")\n",
      "meta_df <- meta_df %>% mutate(\n",
      "  across(any_of('Label'), as.factor),\n",
      "  across(any_of(c('Order', 'Batch')), as.numeric)\n",
      ")\n\n",

      "# 5. Separate Metadata from Peaks for Analysis\n",
      "peaks <- df %>% select(-any_of(meta_cols))\n\n"
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
