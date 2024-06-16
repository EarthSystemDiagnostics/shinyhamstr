# # Ensure shinyapp dependencies are installed
# list_of_packages = c("shiny","vroom", "DT", "hamstr")
#
# lapply(list_of_packages,
#        function(x) if(!require(x,character.only = TRUE)) install.packages(x))
#
# library(shiny)
# library(vroom)
# library(DT)
# library(hamstr)


#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @keywords internal
#' @import shiny
#'
#' @examples
server <- function(input, output, session){

  options(shiny.host = "0.0.0.0")
  options(shiny.port = 9292)

  # load a datafile ----------
  options(shiny.maxRequestSize=300*1024^2)

  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })


  output$datafile <- renderTable({
    data()
  })


  # load a model -------

  r <- reactiveValues(progress_mtime = -1)

  observeEvent(input$load_hamstr, {

    req(input$load_hamstr)

    ext <- tools::file_ext(input$load_hamstr$name)

    r$hamstr_mod <- switch(
      ext,
      rdata = readRDS(input$load_hamstr$datapath),
      validate("Invalid file; Please upload an RDS file")
    )
  })


  # ui elements - data -----------

  # select columns to use for depth and age variables
  output$ui.col.select <- renderUI({
    fluidRow(
      h4("Select columns"),
      column(
        12,
        varSelectInput("depth", "depth:", data(), selected = "depth_cm"),
        varSelectInput("obs_age", "age:", data(), selected = "age.14C.cal"),
        varSelectInput("obs_err", "age uncertainty:", data(), selected = "age.14C.cal.se")
      )

    )
  })


  # ui elements - parameters ----------

  output$ui.adj.pars <- renderUI({

    req(input$depth)
    df <-  data()

    def_pars <- hamstr::hamstr(
      depth = df[[input$depth]],
      obs_age = df[[input$obs_age]],
      obs_err = df[[input$obs_err]],
      sample_posterior = FALSE
    )$data


    fluidRow(h4("Adjust parameters"),

             fluidRow(column(
               12,
               h5("Set top and bottom of modelled core."),
               column(
                 6,
                 numericInput("top_depth", "top_depth:",
                              step = 0.1,
                              value = def_pars$top_depth)
               ),
               column(
                 6,
                 numericInput("bottom_depth", "bottom_depth:",
                              step = 0.1,
                              value = def_pars$bottom_depth)
               )
             )),
             fluidRow(column(
               12,
               h5("Set hierarchical structure of modelled sections."),
               column(
                 12,
                 numericInput("K_fine", "No. of sections [K_fine]:", value = def_pars$K_fine)),
               # column(
               #   6,
               #   numericInput("K_factor", "[K_factor]:", value = def_pars$K_factor, min = 2, max = 10, step = 1))
             )),
             fluidRow(column(
               12,
               h5("Adjust prior hyperparameters."),
               fluidRow(column(
                 12, column(
                   6,
                   numericInput(
                     "acc_mean_prior",
                     "acc_mean_prior:",
                     min = 0,
                     step = 0.1,
                     value = def_pars$acc_mean_prior
                   )
                 ),
                 column(
                   6,
                   numericInput("acc_shape", "acc_shape:",
                                min = 1,
                                step = 0.1,
                                value = def_pars$acc_shape)
                 )
               )),
               fluidRow(column(
                 12, column(
                   6,
                   numericInput("mem_mean", "mem_mean:",
                                min = 0, max = 1,
                                step = 0.1,
                                value = def_pars$mem_mean)
                 ),
                 column(
                   6,
                   numericInput("mem_strength", "mem_strength:",
                                min = 0,
                                step = 0.1,
                                value = def_pars$mem_strength)
                 )
               ))
             )),
             fluidRow(column(
               12,
               h4("Bioturbation"),
               fluidRow(column(
                 12, column(
                   12,
                   checkboxInput(
                     "model_bioturbation",
                     "Model bioturbation?:",
                     value = FALSE
                   )
                 )
               ))
             )),
             fluidRow(column(
               12,
               h4("Displacement"),
               fluidRow(column(
                 12, column(
                   12,
                   checkboxInput(
                     "model_displacement",
                     "Model displacement?:",
                     value = FALSE
                   )
                 )
               ))
             )))
  })

  ## ui - bioturbation parameters -----
  output$ui.bioturbation <- renderUI({


    req(input$model_bioturbation)

    req(input$depth)
    df <-  data()

    def_pars <- hamstr::hamstr(
      depth = df[[input$depth]],
      obs_age = df[[input$obs_age]],
      obs_err = df[[input$obs_err]],
      sample_posterior = FALSE
    )$data


    fluidRow(
      #h4("Bioturbation uncertainty"),

      fluidRow(column(
        12,
        #h5("Model bio"),
        column(
          6,
          numericInput("L_prior_mean", "L_prior_mean:", value = def_pars$L_prior_mean)
        ),
        column(
          6,
          numericInput("L_prior_shape", "L_prior_shape:", value = def_pars$L_prior_shape)
        ),
        fluidRow(column(12, column(12,
                                   varSelectInput("n_ind", "n_ind:", data(), selected = "n.forams"))
        )
        )
      )))
  })


  ## ui - displacement parameters -----
  output$ui.displacement <- renderUI({


    req(input$model_displacement)

    req(input$depth)
    df <-  data()

    def_pars <- hamstr::hamstr(
      depth = df[[input$depth]],
      obs_age = df[[input$obs_age]],
      obs_err = df[[input$obs_err]],
      sample_posterior = FALSE
    )$data


    fluidRow(
      fluidRow(column(
        12,
        column(
          6,
          numericInput("D_prior_mean", "D_prior_mean:", value = def_pars$D_prior_mean)
        ),
        column(
          6,
          numericInput("D_prior_shape", "D_prior_shape:", value = def_pars$D_prior_shape)
        )
      )))
  })


  output$ui.acc_rate <- renderUI({

    req(r$hamstr_mod)

    mod_length <- r$hamstr_mod$data$bottom_depth - r$hamstr_mod$data$top_depth

    fluidRow(column(12,
                    h3("Modelled accumulation rates"),

                    plotOutput("plot.acc_rates", width = "100%"),

                    sliderInput("tau", "Apply running mean:", min = 0,
                                max = ceiling(2*mod_length),
                                value = 0,
                                step = round(mod_length / 10),
                                round = 3
                                ),
                    checkboxGroupInput("acc_x", "x axis",
                                       choices = c("depth", "age"),
                                       selected = "depth")))

  })


  # empty hamstr model -------
  empty_model <- reactive({

    req(input$depth)
    df <- data()

    hamstr::hamstr(
      depth = df[[input$depth]],
      obs_age = df[[input$obs_age]],
      obs_err = df[[input$obs_err]],
      top_depth = input$top_depth,
      bottom_depth = input$bottom_depth,
      K_fine = input$K_fine,
      K_factor = input$K_factor,
      #stan_sampler_args = list(iter = 1000),
      sample_posterior = FALSE
    )


  })

  ## plot empty model -------
  output$empty_model_plot <- renderPlot({
    req(empty_model)
    plot(empty_model(), plot_diagnostics = FALSE)
  })

  output$empty_model_data <- renderPrint({
    req(empty_model)
    empty_model()
  })


  ## ui start -----
  output$ui.start <- renderUI({

    req(input$upload)

    fluidRow(h3("Run hamstr"),
             fluidRow(column(12, numericInput("iter", "No. sampler iterations", empty_model()$data$iter))),
             fluidRow(column(12, actionButton("start", "Run:"))))
  })






  # run hamstr -------------
  # in a separate process so that progress can be monitored by the shiny app
  # Approach taken from here:
  # https://www.jchau.org/2021/02/02/tracking-stan-sampling-progress-shiny/


  ## file to write progress
  tfile <- tempfile(fileext = ".txt")

  ## reactive values
  r <- reactiveValues(progress_mtime = -1)

  observeEvent(input$start, {
    df <- data()
    nc <- parallel::detectCores()
    if (nc >= 4)
      nchains <- 4
    else
      nchains <- 1

    ## start sampling in background process
    r$bg_process <- callr::r_bg(
      ## this is a long running computation
      func = function(df,
                      depth,
                      obs_age,
                      obs_err,
                      K_fine,
                      K_factor,
                      top_depth,
                      bottom_depth,
                      acc_mean_prior,
                      acc_shape,
                      mem_mean,
                      mem_strength,
                      model_bioturbation,
                      model_displacement,
                      n_ind,
                      L_prior_mean,
                      L_prior_shape,
                      D_prior_mean,
                      D_prior_shape,
                      stan_sampler_args#,
                      #hamstr_control
      ) {

        hamstr::hamstr(
          depth = df[[depth]],
          obs_age = df[[obs_age]],
          obs_err = df[[obs_err]],
          K_fine = K_fine,
          K_factor = K_factor,
          top_depth = top_depth,
          bottom_depth = bottom_depth,
          acc_mean_prior = acc_mean_prior,
          acc_shape = acc_shape,
          mem_mean = mem_mean,
          mem_strength = mem_strength,
          model_bioturbation = model_bioturbation,
          model_displacement = model_displacement,
          n_ind = n_ind,
          L_prior_mean = L_prior_mean,
          L_prior_shape = L_prior_shape,
          D_prior_mean = D_prior_mean,
          D_prior_shape = D_prior_shape,
          sample_posterior = TRUE,
          stan_sampler_args = stan_sampler_args#,
          #hamstr_control = hamstr_control
        )
      },
      args = list(
        df = df,
        depth = input$depth,
        obs_age = input$obs_age,
        obs_err = input$obs_err,
        K_fine = input$K_fine,
        K_factor = input$K_factor,
        top_depth = input$top_depth,
        bottom_depth = input$bottom_depth,
        acc_mean_prior = input$acc_mean_prior,
        acc_shape = input$acc_shape,
        mem_mean = input$mem_mean,
        mem_strength = input$mem_strength,
        model_bioturbation = as.numeric(input$model_bioturbation),
        n_ind = if (input$model_bioturbation) df[[input$n_ind]] else NULL,
        L_prior_mean = input$L_prior_mean,
        L_prior_shape = input$L_prior_shape,
        model_displacement = as.numeric(input$model_displacement),
        D_prior_mean = input$D_prior_mean,
        D_prior_shape = input$D_prior_shape,
        stan_sampler_args = list(iter = input$iter)#,
        #hamstr_control = list()
      ),
      stdout = tfile,
      supervise = TRUE
    )
    ## start polling bg process
    r$poll <- TRUE
  })

  ## observe status of bg process ----------
  observe({
    req(r$bg_process, r$poll)
    ## keep re-executing observer as
    ## long as bg process is running
    invalidateLater(millis = 1000, session)
    ## read current progress if file is modified
    mtime <- file.info(tfile)$mtime
    if (mtime > r$progress_mtime) {
      r$progress <- readLines(tfile)
      r$progress_mtime <- mtime
    }
    ## extract draws when bg process is finished
    if (!r$bg_process$is_alive()) {
      r$hamstr_mod <- r$bg_process$get_result()
      r$poll <- FALSE  ## stop polling bg process
    }
  })

  # print progress
  output$progress <- renderText({
    req(r$progress)
    paste(r$progress, collapse = "\n")
  })


  # create hamstr output --------

  output$table.pars <- renderTable({
    req(r$hamstr_mod)
    hamstr:::summary.hamstr_fit(r$hamstr_mod, type = "par")
  })

  output$plot.agemod <- renderPlot({
    req(r$hamstr_mod)
    plot(r$hamstr_mod, plot_diagnostics = FALSE)
  })

  output$plot.prior.acc <- renderPlot({
    req(r$hamstr_mod)
    hamstr:::plot.hamstr_fit(r$hamstr_mod, type = "acc_mean_prior_post")
  })

  output$plot.mem_prior_post <- renderPlot({
    req(r$hamstr_mod)
    hamstr:::plot.hamstr_fit(r$hamstr_mod, type = "mem_prior_post")
  })


  output$plot.L <- renderPlot({
    req(r$hamstr_mod, input$model_bioturbation)
    hamstr:::plot.hamstr_fit(r$hamstr_mod, type = "L")
  })

  output$plot.D <- renderPlot({
    req(r$hamstr_mod, input$model_displacement)
    hamstr:::plot.hamstr_fit(r$hamstr_mod, type = "D")
  })

  ## sediment acc_rates ------
  output$plot.acc_rates <- renderPlot({
    req(r$hamstr_mod)
    plot(r$hamstr_mod, type = "acc_rates", tau = input$tau, axis = input$acc_x)
  })


  # output for download ---------

  ## ui elements - download model -----

  output$ui.save <- renderUI({

    req(r$hamstr_mod)

    fluidRow(h3("Save hamstr model"),
             downloadButton("dl.hamstr_mod", "Save"))

  })

  output$dl.hamstr_mod <- downloadHandler(
    filename = function() {
      paste0("hamstr_mod.rdata")
    },
    content = function(file) {
      saveRDS(r$hamstr_mod, file)
    }
  )

  depth_data <- reactive({
    req(input$upload_depths)

    ext <- tools::file_ext(input$upload_depths$name)
    switch(
      ext,
      csv = vroom::vroom(input$upload_depths$datapath, delim = ","),
      tsv = vroom::vroom(input$upload_depths$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  output$depthfile <- renderTable({
    df <- depth_data()
    df[,input$new_depth_var]
  })

  # export ui ------
  output$ui.pick.seq.or.file <- renderUI({

    req(r$hamstr_mod)


    column(12,
           fluidRow(
             h3("Pick depths at which to export age estimates"),
             column(12,  radioButtons("seq_or_file", NULL,
                          choiceNames = c("Define a sequence of depths (defaults to modelled depths)",
                                          "Upload a file with a column of depths"),
                          choiceValues = c("seq", "depth.file")#,
                          #selected = character(0)
                          ))

           )
    )
  })

  output$ui.define.seq.file <- renderUI({

    req(r$hamstr_mod)


    column(12,
           fluidRow(
             conditionalPanel(condition = "input.seq_or_file == 'seq'",

                              fluidRow(

                              column(4, numericInput("top_depth_out", "top_depth:",
                                                     step = 0.1,
                                                     value = r$hamstr_mod$data$top_depth)),
                              column(4, numericInput("bottom_depth_out", "bottom_depth:",
                                                     step = 0.1,
                                                     value = r$hamstr_mod$data$bottom_depth)),
                              column(4, numericInput("by_out", "by:",
                                                     step = 0.1,
                                                     value = r$hamstr_mod$data$delta_c))
                              )
             ),
             conditionalPanel(condition = "input.seq_or_file == 'depth.file'",

                              fluidRow(
                                column(12,
                                       h4("Select file containing new depths"),
                                       fileInput("upload_depths", NULL, accept = c(".csv", ".tsv"))
                                )
                              )),
           )
    )
  })

  output$ui.exp.depth.col <- renderUI({
    req(depth_data())
     column(12,
            fluidRow(
              h4("Select column containing new depths"),
              varSelectInput("new_depth_var", "", depth_data(), selected = "depth_cm"),
              )
            )
    })

  new.dpths <- reactive({
    req(r$hamstr_mod)

    if (input$seq_or_file == "seq"){
      seq(input$top_depth_out, input$bottom_depth_out, by = input$by_out)
    } else if(input$seq_or_file == "depth.file"){

      ddf <- depth_data()
      as.numeric(ddf[[input$new_depth_var]])

      }
  })

output$new.depths <- DT::renderDT({
    data.frame(new_depths = new.dpths())
  })

output$ui.new.depths <- renderUI({

  req(r$hamstr_mod, input$seq_or_file)

  fluidRow(column(12,
                  h3("Exported depths"),
                  DT::DTOutput("new.depths")
         )
  )
})


output$ui.exp.new.depths <- renderUI({

    req(r$hamstr_mod, new.dpths())

    column(12,
           fluidRow(
             hr(),
             h4("Download a summary of the age model"),
             p(
               "The summary contains the depth-age model summarised across the individual realisations to give the median, mean, and quantiles of the modelled age at each depth."
             ),
             downloadButton("dl.am.s", "Download summary age model .tsv")
           ),
           hr(),
           fluidRow(
             h4(
               "Download the full age model with all realisations (iterations of the sampler)."
             ),
             downloadButton("dl.am.full", "Download full age model .tsv")
           )
           )

  })


  agemod.smry <- reactive({
    req(r$hamstr_mod)

    dpths <- seq(input$top_depth_out, input$bottom_depth_out, by = input$by_out)
    #dpths <- new.dpths()

    ages <- hamstr:::predict.hamstr_fit(r$hamstr_mod, depth = dpths)
    hamstr:::summary.hamstr_fit(ages)
  })

  agemod.full <- reactive({
    req(r$hamstr_mod)

    req(r$hamstr_mod)
    dpths <- seq(input$top_depth_out, input$bottom_depth_out, by = input$by_out)
    #dpths <- new.dpths()

    hamstr:::predict.hamstr_fit(r$hamstr_mod, depth = dpths)

    # hamstr:::predict.hamstr_fit(r$hamstr_mod)
  })

  output$dl.am.s <- downloadHandler(
    filename = function() {
      paste0("hamstr_agemod_summary.tsv")
    },
    content = function(file) {
      vroom::vroom_write(agemod.smry(), file)
    }
  )

  output$dl.am.full <- downloadHandler(
    filename = function() {
      paste0("hamstr_agemod_full.tsv")
    },
    content = function(file) {
      vroom::vroom_write(agemod.full(), file)
    }
  )

}
