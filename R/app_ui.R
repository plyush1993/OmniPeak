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

      prettyCheckbox("add_labels", "Add Label Column", value = TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
      conditionalPanel(
        condition = "input.add_labels",
        radioButtons("label_source", "Label source:",
                     c("From sample names" = "from_rows", "From custom CSV" = "from_custom")),
        conditionalPanel(
          condition = "input.label_source == 'from_rows'",
          numericInput("token_idx", "Main Label Token index", value = 2, min = 1),
          textInput("token_sep", "Token separator (used for all name parsing)", value = "_")
        ),
        conditionalPanel(
          condition = "input.label_source == 'from_custom'",
          fileInput("meta_csv", "Upload labels CSV", accept = ".csv")
        )
      ),

      tags$br(),
      prettyCheckbox("add_run_order", "Add Order by Sequence", value = FALSE, status = "primary", icon = icon("check"), animation = "jelly"),
      prettyCheckbox("add_extra_meta", "Extract Extra Variables", value = FALSE, status = "primary", icon = icon("check"), animation = "jelly"),
      conditionalPanel(
        condition = "input.add_extra_meta",
        textInput("extra_meta_names", "Variable Name(s) (comma-separated):", placeholder = "Batch, Genotype"),
        textInput("extra_meta_indices", "Token Index(es) (comma-separated):", placeholder = "1, 4"),
        helpText("Example: If name is 'B1_KO_Sample', and you want Batch and Genotype, type 'Batch, Genotype' and indices '1, 2' with separator '_'.")
      ),

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

            div(class = "well", style = "background-color: #f8f9fa; border-left: 5px solid #18bc9c; padding: 15px; margin-bottom: 15px;",
              h4(tags$b("2. Metadata & Labels"), style = "margin-top: 0; color: #18bc9c;"),
              p(style = "margin-bottom: 0;", "Extract experimental metadata directly from your sample names or custom", tags$code(".csv"), "(one column no headers). Define a token separator (e.g., '_') and pick which token index represents the label or extra variables (like Batch or Genotype).")
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
}
