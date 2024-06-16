#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## hamstrshiny plan
#
# Upload file
# Select variables for depth, obs_age, obs_err
# pass to hamstr and return hamstr data object
# plot data and priors
# allow modification of parameters
# sample model
# plot age model, diagnostic plots, prior/posteriors
# tables of values
# allow download of age models and accumulation rates

#write_csv(ODP1233.14C, file = "test-ODP1233.14C.csv")


library(shiny)
library(hamstr)

# Define UI for application that draws a histogram
#' Title
#'
#' @return
#' @keywords internal
#' @import shiny
#' @importFrom utils packageVersion
#' @examples
ui <- function() {(fluidPage(
  fluidRow(
    # Application title
    column(6, titlePanel("shiny Hamstr"),
    h5(paste0("Hamstr version (", packageVersion("hamstr"), ")")),
    tags$a(href="https://earthsystemdiagnostics.github.io/hamstr/", "https://earthsystemdiagnostics.github.io/hamstr/")),

    column(6, img(src='www/hex-shiny-hamstr-offset.svg', align = "right", height = 150)),
    ),

    # Sidebar -----------
    sidebarLayout(sidebarPanel(
        fluidRow(
            h4("Load data"),
            column(12,
                   fileInput("upload", NULL, accept = c(".csv", ".tsv"))
            )),
        fluidRow(
          h4("Load hamstr model"),
          column(12,
                 fileInput("load_hamstr", NULL, accept = c(".rdata"))
          )),
        fluidRow(
            column(12,
                   uiOutput("ui.col.select")
            )),
        fluidRow(
            column(12,
                   uiOutput("ui.adj.pars")
            )),
        fluidRow(
            column(12,
                   uiOutput("ui.bioturbation")
            )),
        fluidRow(
          column(12,
                 uiOutput("ui.displacement")
          )),
        fluidRow(
            column(12,
                   uiOutput("ui.start")
            )),
        fluidRow(
          column(12,
                 uiOutput("ui.save")
          ))

    )
    ,

    # main ui panel -----------
    mainPanel(

      h4("Select the \"Instructions\" tab if you need guidance.", style = "color:red"),
              tabsetPanel(
                # instructions panel --------
        tabPanel(
          "Instructions",
          h3("Step 1 - load data"),
          p("First load a data-set of age-control points using the buttons in the
            left-hand control panel. Data can be in a .csv or .tsv file. Age-control
            points should already be calibrated to calendar age."),
          p("Alternatively load a previously fitted Hamstr model and skip to Step 3,
            or Step 5."),
          h3("Step 2 - pick columns"),
          p("Next use the drop-down selection boxes to tell Hamstr which
            columns contains the depths, ages and age-uncertainties."),
          p("A plot of your data should now appear in the \"Model\" tab."),
          h3("Step 3 - adjust parameters"),
          p("Before running the model you can adjust the default parameters.
            By default the age model is created from the highest
            to lowest age-control points, but this range can be extended if
            for example you want to extrapolated to the surface."),
          p("You should not normally need to adjust the other parameters."),
          h3("Step 4 - run Hamstr"),
          p("Click \"Run:\" at the bottom of the left-hand control panel to trigger
            the Stan sampler and fit the Hamstr model (in Bayesian terminology, \"sample from the posterior distribution of the model\").
                    This may take 1-2 minutes"),
          p("Once the sampling is complete the plot should update and show
            the fitted age-depth model."),
          h3("Step 5 - examine the fitted accumulation / age-depth model"),
          p("Now you can switch to the \"Priors and posteriors\" tab to examine
            diagnostic plots or the \"Accumulation rate\" tab to see plots of the
            sediment accumulation rate."),
          h3("Step 6 - export the age-model"),
          p("In the \"Export\" tab you can export the age-depth model as .tsv files."),
          h3("Step 7 - save the full Hamstr fit"),
          p("At the bottom of the control panel you can save the full Hamstr model
            as an RDS file. This can be loaded here in a future session (Step 1) to be re-examined,
            export the age-depth model interpolated to different depths, or to be
            refit with adjusted parameters.")
        ),
        # modelling panel --------
        tabPanel(
            "Model",
            tags$head(
                tags$style("#datafile{overflow-y:scroll; max-height: 200px; min-height: 200px;}")
            ),
            h3("Data"),
            fluidRow(tableOutput("datafile")),
            ## limit the height of the progress message box
            h3("Sampler progress"),
            tags$head(
                tags$style("#progress{overflow-y:scroll; max-height: 200px; min-height: 200px;}")
            ),
            verbatimTextOutput("progress"),
            h3("Age model"),
            conditionalPanel("input.start == '0'",
                             plotOutput("empty_model_plot", width = "600px")),
            plotOutput("plot.agemod", width = "600px"),
            tableOutput("table.pars")
        ),
        # diagnostic figures panel -----------
        tabPanel(
            "Priors and posteriors",
            h3("Overal mean accumulation rate"),
            plotOutput("plot.prior.acc", width = "600px"),
            h3("Memory parameter"),
            plotOutput("plot.mem_prior_post", width = "600px"),
            h3("Bioturbation"),
            plotOutput("plot.L", width = "600px"),
            h3("Displacement"),
            plotOutput("plot.D", width = "600px")

        ),
        # accumulation rate panel --------
        tabPanel(
          "Accumulation rate",
          uiOutput("ui.acc_rate")
        ),
        # export panel -----------
        tabPanel(
            "Export",
            fluidRow(
              column(8,
                     uiOutput("ui.pick.seq.or.file"),
                     uiOutput("ui.define.seq.file"),
                     conditionalPanel(
                       condition = "input.seq_or_file == 'depth.file'",
                       uiOutput("ui.exp.depth.col")
                     ),
            #hr(),
            uiOutput("ui.exp.new.depths")
            ),
            column(4,
                   uiOutput("ui.new.depths")
                   )
            )
        ),
        selected = "Model"
    )))
))}
