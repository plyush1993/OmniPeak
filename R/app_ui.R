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
app_ui <- function() {
fluidPage(
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
              href = "www/omnipeak.png")
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
    tags$img(src = 'www/omnipeak.png', height = '120px', style = 'margin-right: 20px;'),
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
        HTML("<b>From uploaded metadata column</b><br><span style='font-size:12px;color:#666;'>Select one metadata file column from below as Label.</span>"),
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
    value = TRUE,
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
        ".mzML", ".mzXML", ".raw", ".RAW", ".lcd",
        ".wiff", ".WIFF", ".d", ".D",
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
        tabPanel(
              "Export Preview",
              tags$br(),
              uiOutput("quick_stats_tidy"),
              uiOutput("help_tidy"),
              h4(
                "Tidy Table",
                style = "font-weight:bold; color:#007BA7;"
              ),
              DTOutput("preview_tidy"),
              uiOutput("preview_metadata_section"),
              uiOutput("preview_standard_section")
            ),
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
              HTML("Select your software source (<b><i>mzMine</i></b>, <b><i>MS-DIAL</i></b>, <b><i>xcms</i></b>, etc.) and upload your <code>.csv</code> peak table. OmniPeak automatically standardizes the columns by selected names and detects your sample data by provided keywords. You can also specify Feature ID column (which becomes Tidy headers, and also Feature column in Standard Peak Table), by default: 'mz_rt'.")
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
  tags$b("Labels from sample names", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "The optional ",
    tags$code("Label"),
    " column can be parsed directly from sample names using a separator and token index. "
  )
),

div(
  style = "background:#ffffff; border-left:4px solid #18bc9c; padding:10px; margin-bottom:10px; border-radius:5px;",
  tags$b("Labels from metadata", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "Alternatively, the ",
    tags$code("Label"),
    " column can be taken from one selected column in the uploaded metadata table."
  )
),

div(
  style = "background:#ffffff; border-left:4px solid #18bc9c; padding:10px; margin-bottom:10px; border-radius:5px;",
  tags$b("Order and extra variables from sample names", style = "color:#18bc9c;"),
  p(
    style = "margin-bottom: 0; margin-top: 5px;",
    "You can also add ",
    tags$code("Order"),
    " by detected sample sequence and extract additional variables directly from sample names, using token indices."
  )
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
)
),

            div(class = "well", style = "background-color: #f8f9fa; border-left: 5px solid #008B8B; padding: 15px; margin-bottom: 15px;",
              h4(tags$b("3. Export Data"), style = "margin-top: 0; color: #008B8B;"),
              p("Download your shaped tidy dataset: features (peaks) as columns, samples as rows in ", tags$code(".csv")," or ",tags$code(".txt"), ". Directly compatible with ", HTML("<b><i>MetaboAnalyst</i></b> ."),
                "Optionally you can download Metadata (all defines metadata columns) and Standard Peak Table (Feature ID, mz, RT, and Sample columns) files."),
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
}
