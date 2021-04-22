library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

    # App title ----
    titlePanel("Uploading Files"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", "Choose a CSV DATA File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Input: Select a meta-data-file ----
            fileInput("file2", "Choose a CSV META-DATA File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Provide Title for your file
            textInput("title",
                      "Please provide a short title to identyfy file contents",
                      "Experiment X"),

            # Horizontal line ----
            tags$hr(),
            #Formatting
            tags$p('Select correct formatting'),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),

            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),

            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),

            # Horizontal line ----
            tags$hr(),

            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            actionButton("save","SAVE your data")
        ),

        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Preview",
                                 # Output: Data file ----
                                 tableOutput("contents"),
                                 tags$hr(),
                                 tags$p('The meta data for the file above'),
                                 tableOutput("meta")
                        ),
                        tabPanel("Instructions",
                                 p("Please provide the data in a .csv file, and
                                   folow tidy data practices. That means that:",
                                   tags$ol(
                                       tags$li("each relevant data from one set of experiments should be in one table"),
                                       tags$li("each observation (usually a sample), should be in one row"),
                                       tags$li("each variable (usually a measuring parameter from this sample) should be in a separate column")
                                   ),

                                   "Please also provide the correct sample name (as provided on the QR code) in the first column."),
                                 p("Please provide the METADATA in a seperate .csv file, where each row contains the the name of the variable and a description"),
                                 p())
            )
        )

    )
)

# Define server logic to read selected file ----
server <- function(input, output) {

    output$contents <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }

    })
# Here we input the metadata into the app.
    output$meta <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file2)

        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file2$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }

    })
    # save the data to server upon button click
    observeEvent(input$save, {

        file.copy(input$file1$datapath, paste0("saved_data/",
                                                Sys.time() %>% as.character() %>% gsub(pattern = "-| |:",replacement = "_"),
                                               "_",
                                               input$title %>% gsub(pattern = "-| |:",replacement = "_"),
                                               ".csv"))
        file.copy(input$file2$datapath, paste0("saved_data/",
                                                Sys.time() %>% as.character() %>% gsub(pattern = "-| |:",replacement = "_"),
                                               "_",
                                               input$title %>% gsub(pattern = "-| |:",replacement = "_"),
                                               "_meta.csv"))
    })

}

# Create Shiny app ----
shinyApp(ui, server)
