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
