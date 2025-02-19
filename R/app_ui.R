#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tabsetPanel(
        tabPanel(title = "Data Loading",
                 fluidRow(
                   column(3,
                          wellPanel(
                            h5(strong("Upload Data Files")),
                            p("Upload JSON files that are the outputs of the localization analysis in ThunderSTORM."),
                            br(),
                            h5("Naming Convention"),
                            p("Ensure that dataset names include relevant metadata and follow a consistent style."),
                            fileInput(
                              "jsonFiles", "Select JSON Files",
                              multiple = TRUE,
                              accept = c("application/json", ".json")
                            ),
                            p("Specify the metadata types in the main panel"),
                            textInput(
                              "sep", "_", label = "Dataset Name Separator",
                              placeholder = "Enter the separator used in dataset names"
                            ),

                            selectInput("checkGroup1",
                                        "Expr.conds with numeric values",
                                        choices = c("expr.cond1",
                                                    "expr.cond2",
                                                    "expr.cond3",
                                                    "expr.cond4",
                                                    "expr.cond5"), multiple = TRUE),
                            br(),
                            br(),
                            textInput(
                              "analysis.id", "Analysis Identifier",
                              placeholder = "Enter a unique ID for this analysis"
                            ),
                            actionButton(inputId = "load", label = "Load and Process Data"),
                            br(), br(),
                            h5(strong("Save and Log")),
                            br(),
                            downloadButton('downloadData1', 'Download Processed Output'),
                            br(),
                            br(),
                            screenshotButton(filename = "Data Loading Tab", label = "Save Visual Log")
                          )
                   ),


                   column(9,
                          h5(strong("Example Dataset Name:")),
                          verbatimTextOutput("folderpath"),
                          p("Dataset names should include the protein name and frame interval.
                            You can include up to five different experimental conditions
                            (expr.cond)."),
                          br(),
                          h5(strong("Copy and paste the dataset name with metadata here
                                    (exclude the non-metadata parts)")),
                          wellPanel(textInput("dataset.name.s",
                                              ""))),
                   column(9, h5(strong("Specify the type of variable each part of the dataset
                                        name represents"))),
                   column(1, selectInput("pos1", "Part1_",
                                         c("NA" = "remove1", "protein", "frame.interval",
                                           "expr.cond1",
                                           "expr.cond2",
                                           "expr.cond3",
                                           "expr.cond4",
                                           "expr.cond5"))),
                   column(1, selectInput("pos2", "_Part2_",
                                         c("NA" = "remove2", "protein", "frame.interval",
                                           "expr.cond1",
                                           "expr.cond2",
                                           "expr.cond3",
                                           "expr.cond4",
                                           "expr.cond5"))),
                   column(1, selectInput("pos3", "_Part3_",
                                         c("NA" = "remove3", "protein", "frame.interval",
                                           "expr.cond1",
                                           "expr.cond2",
                                           "expr.cond3",
                                           "expr.cond4",
                                           "expr.cond5"))),
                   column(1, selectInput("pos4", "_Part4_",
                                         c("NA" = "remove4", "protein", "frame.interval",
                                           "expr.cond1",
                                           "expr.cond2",
                                           "expr.cond3",
                                           "expr.cond4",
                                           "expr.cond5"))),
                   column(1, selectInput("pos5", "_Part5_",
                                         c( "NA" = "remove5", "protein", "frame.interval",
                                            "expr.cond1",
                                            "expr.cond2",
                                            "expr.cond3",
                                            "expr.cond4",
                                            "expr.cond5"))),
                   column(1, selectInput("pos6", "_Part6_",
                                         c("NA" = "remove6", "protein", "frame.interval",
                                           "expr.cond1",
                                           "expr.cond2",
                                           "expr.cond3",
                                           "expr.cond4",
                                           "expr.cond5"))),
                   column(1, selectInput("pos7", "_Part7",
                                         c("NA" = "remove7" , "protein", "frame.interval",
                                           "expr.cond1",
                                           "expr.cond2",
                                           "expr.cond3",
                                           "expr.cond4",
                                           "expr.cond5"))),
                   column(9,
                          h5(strong("Number of datasets:")),
                          verbatimTextOutput("summary1.1"),
                          h5(strong("Number of frames:")),
                          verbatimTextOutput("summary1.2"),
                          h5(strong("Number of observations:")),
                          verbatimTextOutput("summary1.3"),
                          h5(strong("Detail of datasets:")),
                          dataTableOutput("summary1.4"))
                 )),

        tabPanel(title = "Spatial Filtering",
                 fluidRow(
                   column(3,
                          wellPanel(
                            fileInput("file1", label = "Load Output from Data Loading Tab", accept = ".rds"),
                            actionButton(inputId = "start", label = "Start Processing"),
                            br(),
                            br(),
                            numericInput("num", label = "Select Dataset", value = 1,
                                         min = 1),
                            br(),
                            br(),
                            h5(strong("Data Processing Actions")),
                            p("Perform data point selection on the plot and select the type below.
                              Results are displayed to the right."),
                            br(),
                            br(),
                            actionButton(inputId = "select", label = "Substrate Trace"),
                            br(),
                            actionButton(inputId = "stuck", label = "Surface-bound Signal"),
                            br(),
                            actionButton(inputId = "split", label = "Split Substrate Trace"),
                            br(),
                            br(),
                            br(),
                            actionButton(inputId = "next.data.set", "Next dataset"),
                            br(),
                            br(),
                            br(),
                            br(),
                            h5(strong("Resetting Selections")),
                            p("To repeat selections for each dataset, ensure to reset previous selections."),
                            actionButton(inputId = "reset", label = "Reset Current Selection"),
                            br()),

                          wellPanel(
                            h5(strong("Intensity Filtering")),
                            actionButton(inputId = "intensity", label = "Show Intensity Distribution"),
                            br(), br(),
                            h5(strong("Set Intensity Range for Filtering")),
                            p("Exclude extreme intensity values."),
                            numericInput("min.intensity", "Minimum Intensity", value = 1),
                            numericInput("max.intensity", "Maximum Intensity", value = 10000)),

                          wellPanel(
                          h5(strong("Save and Log")),
                          br(),
                          downloadButton('downloadData2', 'Download Processed Output'),
                                    br(),
                                    br(),
                          screenshotButton(filename = "Spatial Filtering Tab", label = "Save Visual Log")
                                    )),
                   h5(strong("Selected data set name:")),
                   verbatimTextOutput("full.name"),
                   column(5, plotOutput("plot1",
                                        brush = "plot_brush")),
                   column(5, plotOutput("plot2")),
                   h5(strong("Progress:")),
                   column(10,
                          dataTableOutput("progress"),
                          br(),
                          br(),
                          br()),
                   column(6,
                          plotOutput("intensity.plot"),
                          br(),
                          br(),
                          br())
                 )),
        tabPanel(title = "Trajectory Detection",
                 fluidRow(
                   column(3,
                          wellPanel(fileInput("file2", label = "Load Output from Spatial Filtering Tab",
                                              accept = ".rds"),
                                    numericInput("max.dx",
                                                 label = "Max frame-to-frame displacement
                                                 along substrate (nm)",
                                                 value = 360),
                                    numericInput("max.dy",
                                                 label = "Max frame-to-frame displacement
                                                 across Substrate (nm)",
                                                 value = 240),
                                    numericInput("max.dx.2",
                                                 label = "Localization precision (nm)",
                                                 value = 25),

                                    br(),
                                    numericInput("min.frame.duration",
                                                 label = "Min trajectory duration (frames)",
                                                 value = 5),
                                    numericInput("trajs.to.inspect",
                                                 label = "Number of Trajectories for Inspection",
                                                 value = 5 ),
                                    h5("These trajectories wil be used for optimizing
                                       the noise exclusion model"),
                                    br(),

                                    actionButton(inputId = "detect",
                                                 label = "Detect trajectories"),
                                    br(),
                                    br(),

                                    h5("Adjust initial values if they are rejected and re-analyze"),

                                    actionButton(inputId = "reanalyse", label = "Re-analyze with new parameters")),
                          wellPanel(h5(strong("Visualization Control")),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    actionButton(inputId = "show", label = "Visualize Detected Trajectories:"),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    numericInput("num2", label = "Select Dataset for Visualization", value = 1,
                                                 min = 1)),
                          wellPanel(h5(strong("Save and Log")),
                                    br(),
                                    downloadButton('downloadData3', 'Download Processed Output'),
                                    br(),
                                    br(),
                                    screenshotButton(filename = "Trajectory Detection Tab", label = "Save Visual Log"))),
                   h5(strong("Trajectory Detection Result Overview")),
                   column(9, plotOutput("plot31")),
                   column(9, verbatimTextOutput("eval"),
                          br()),

                   h5(strong("Comparison of Trajectories for Inspection:")),
                   column(6, offset = 1,  plotOutput("plot32"),
                          br()),
                   column(6,  h5(strong("Visualized Dataset:"))),
                   br(),
                   column(9, verbatimTextOutput("full.name2")),
                   br(),
                   column(6, offset = 1, plotOutput("plot33")),
                   br()
                 )),
        tabPanel(title = "Visual Inspection",
                 fluidRow(
                   column(3,
                          wellPanel(
                            fileInput("file3", label = "Load Output from Trajectory Detection Tab", accept = ".rds"),
                            br(),
                            actionButton(inputId = "do.it",
                                         label = "Visualize Trajectories"),
                            br(),
                            h5("Inspection and Artifact Removal"),
                            p("Inspect trajectories visually and remove any detected artifacts"),
                            br(),
                            numericInput("num41",
                                         label = "Select Dataset",
                                         value = 1, min = 1),
                            numericInput("num42",
                                         label = "Select Trajectory",
                                         value = 1, min = 1),
                            actionButton(inputId = "remove4", label = "Remove Trajectory"),
                            br(),
                            br(),
                            h5(strong("Save and Log")),
                            br(),
                            downloadButton('downloadData4', 'Download Processed Output'),
                            br(),
                            br(),
                            screenshotButton(filename = "Visual Inspection Tab", label = "Save Visual Log"))),
                   h5(strong("Selected Dataset:")),
                   verbatimTextOutput("full.name41"),
                   h5(strong("Selected Trajectory:")),
                   verbatimTextOutput("full.name42"),
                   column(5, plotOutput("plot41")),
                   column(5, plotOutput("plot42")),
                 )),
        tabPanel(title = "Noise Exclusion",
                 fluidRow(
                   column(3,
                          wellPanel(
                            fileInput("file51", label = "Load Output from Data Loading Tab", accept = ".rds"),
                            fileInput("file52", label = "Load Output from Visual Inspection Tab", accept = ".rds"),
                            actionButton(inputId = "noise.filters", label = "Apply Noise Filters"),
                            checkboxGroupInput("checkGroup", h5(strong("Select Detected Data Types to Visualize")),
                                               choices = list("Noise-excluded data" =
                                                                "Noise-excluded data",
                                                              "Surface-bound emitters" =
                                                                "Surface-bound emitters",
                                                              "Non-linear movements" =
                                                                "Non-linear movements",
                                                              "High/low intensity noise" =
                                                                "High/low intensity noise",
                                                              "Short-lived noise" =
                                                                "Short-lived noise"),
                                               selected = "Noise-excluded data"),
                            sliderInput("stuck.gauge", "Surface-bound filter gauge",
                                        min = 0, max = 10,
                                        value = 7),
                            sliderInput("flank.gauge", "Non-linear filter gauge",
                                        min = 0, max = 10,
                                        value = 3),
                            sliderInput("intensity.gauge", "Intensity filter gauge",
                                        min = 0, max = 10,
                                        value = 3),
                            numericInput("num51", label = "Select Dataset", value = 1, min = 1)
                          ),
                          wellPanel(
                            h5(strong("Trajectory Visualization")),
                            actionButton(inputId = "do.it5", label = "Visualize Trajectories"),
                            br(),
                            br(),
                            numericInput("num52", label = "Select trajectory ID",
                                         value = 1, min = 1),
                            br(),
                            br()
                          ),
                          wellPanel(
                            h5(strong("Save and Log")),
                            br(),
                            downloadButton('downloadData5', 'Download Processed Output'),
                            br(),
                            br(),
                            screenshotButton(filename = "Noise Exclusion Tab", label = "Save Visual Log")
                          )),
                   column(6, offset = 1,
                          h5(strong("Noise Exclusion Results")),
                          plotOutput("plot51"),
                          plotOutput("plot52"),
                          br(),
                          br(),
                          br()),
                   column(6,plotOutput("plot53")),
                   column(3,plotOutput("plot54"))
                 )),
        tabPanel(title = "Analysis Output",
                 fluidRow(
                   column(3,
                          wellPanel(
                            fileInput("file6", label = "Load Output from Noise Exclusion Tab", accept = ".rds"),
                            p("Upload files sequentially and press 'Add Data' to include each one"),
                            actionButton(inputId = "add6", label = "Add Data"),
                            br(),
                            br(),
                            checkboxGroupInput("inCheckboxGroup", "Proteins Added", inline = TRUE),
                            br(),
                            selectInput("in.select", "Select Experimental Condition",
                                        c("expr.cond1", "expr.cond2"))
                          )),
                   column(9,
                          h5(strong("Overview of Analyzed Data")),
                          dataTableOutput("datasets"),
                          br(),
                          br()
                   )),
                 h5(strong("Visualization of Analysis Outputs")),
                 column(12,
                        wellPanel(
                          h5(strong("Scanning Speed")),
                          actionButton(inputId = "show60", label = "Display Results"),
                          br(),
                          br(),
                          checkboxGroupInput("inCheckboxGroup0",
                                             "Proteins", inline = TRUE),
                          br(),
                          selectInput("in.select0", "Select Exprimental Condition",
                                      c("expr.cond1", "expr.cond2")))),
                 column(4, plotOutput("plot60.1")),
                 column(8, plotOutput("plot60.2")),
                 column(1, offset = 3, downloadButton('downloadData60.1', 'Download plot.1.1 data')),
                 column(1, offset = 7, downloadButton('downloadData60.2', 'Download plot.1.2 data')),
                 column(8, offset = 4, plotOutput("plot60.3")),
                 column(1, offset = 11, downloadButton('downloadData60.3', 'Download plot.1.3 data')),

                 column(12,
                        br(),
                        br(),
                        wellPanel(
                          h5(strong("Binding Lifetime")),
                          actionButton(inputId = "show61", label = "Display Results"),
                          br(),
                          br(),
                          checkboxGroupInput("inCheckboxGroup1",
                                             "Proteins", inline = TRUE),
                          br(),
                          selectInput("in.select1", "Select Exprimental Condition",
                                      c("expr.cond1", "expr.cond2")))),
                 column(4, plotOutput("plot61.1")),
                 column(8,plotOutput("plot61.2")),
                 column(1, offset = 3, downloadButton('downloadData61.1', 'Download plot.2.1 data')),
                 column(1, offset = 7, downloadButton('downloadData61.2', 'Download plot.2.2 data')),

                 column(12,
                        br(),
                        br(),
                        wellPanel(
                          h5(strong("Scanning Coverage")),
                          actionButton(inputId = "show62", label = "Display Results"),
                          br(),
                          br(),
                          checkboxGroupInput("inCheckboxGroup2",
                                             "Proteins", inline = TRUE),
                          br(),
                          selectInput("in.select2", "Select Exprimental Condition",
                                      c("expr.cond1", "expr.cond2")))),
                 column(4, plotOutput("plot62.1")),
                 column(8, plotOutput("plot62.2")),
                 column(1, offset = 3, downloadButton('downloadData62.1', 'Download plot.3.1 data')),
                 column(1, offset = 7, downloadButton('downloadData62.2', 'Download plot.3.2 data')),

                 column(12,
                        br(),
                        br(),
                        wellPanel(
                          h5(strong("Accumulaitive Scanning Length")),
                          actionButton(inputId = "show63", label = "Display Results"),
                          br(),
                          br(),
                          checkboxGroupInput("inCheckboxGroup3",
                                             "Proteins", inline = TRUE),
                          br(),
                          selectInput("in.select3", "Select Exprimental Condition",
                                      c("expr.cond1", "expr.cond2")))),
                 column(4, plotOutput("plot63.1")),
                 column(8, plotOutput("plot63.2")),
                 column(1, offset = 3, downloadButton('downloadData63.1', 'Download plot.4.1 data')),
                 column(1, offset = 7, downloadButton('downloadData63.2', 'Download plot.4.1 data')),

                 column(12,
                        br(),
                        br(),
                        wellPanel(
                          h5(strong("Redundancy-Efficiency")),
                          actionButton(inputId = "show64", label = "Display Results"),
                          br(),
                          br(),
                          checkboxGroupInput("inCheckboxGroup4",
                                             "Proteins", inline = TRUE),
                          br(),
                          selectInput("in.select4", "Select Exprimental Condition",
                                      c("expr.cond1", "expr.cond2")))),
                 column(4, plotOutput("plot64.1")),
                 column(8, plotOutput("plot64.2")),
                 column(1, offset = 3, downloadButton('downloadData64.1', 'Download plot.5.1 data')),
                 column(1, offset = 7, downloadButton('downloadData64.2', 'Download plot.5.2 data')),
                 column(8, offset = 4, plotOutput("plot64.3")),
                 column(1, offset = 11, downloadButton('downloadData64.3', 'Download plot.5.3 data')),

                 column(12,
                        br(),
                        br(),
                        wellPanel(
                          h5(strong("Diffusion rate analysis")),
                          actionButton(inputId = "show65", label = "Display Results"),
                          br(),
                          br(),
                          checkboxGroupInput("inCheckboxGroup5",
                                             "Proteins", inline = TRUE),
                          selectInput("in.select5", "Select Exprimental Condition",
                                      c("expr.cond1", "expr.cond2")))),
                 column(4, plotOutput("plot65.1")),
                 column(8, plotOutput("plot65.2")),
                 column(1, offset = 3, downloadButton('downloadData65.1', 'Download plot.6.1 data' )),
                 column(1, offset = 7, downloadButton('downloadData65.2', 'Download plot.6.2 data')),
                 column(8, offset = 4,  plotOutput("plot65.3")),
                 column(1, offset = 11, downloadButton('downloadData65.3', 'Download plot.6.3 data'),
                        br(),
                        br()
                 ),
                 wellPanel(
                 column(1, screenshotButton(filename = "Analysis Output Tab", label = "Save Visual Log"))
                 )


        )
      ))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SMITracker"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
