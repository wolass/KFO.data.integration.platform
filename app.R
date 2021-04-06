# This Shiny web application allows to integrate data into one single platform
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


pacman::p_load(shiny,shinyFiles)

# Define UI for data upload app ----
ui <- fluidPage(

    # App title ----
    titlePanel("Uploading Files"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Horizontal line ----
            tags$hr(),

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
            actionButton("upload", "Upload"),
            shinyFilesButton('files',
                             label='File select',
                             title='Please select a file',
                             multiple=FALSE)

        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Data file ----
            tableOutput("contents")

        )

    )
)

# Define server logic to read selected file ----
server <- function(input, output) {

        shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))

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
    #Save file upon button click
    observeEvent(input$upload, {
        save_data_flatfile <- function(data) {
            data <- t(data)
            file_name <- paste0(paste(get_time_human(), digest(data,
                                                               algo = "md5"), sep = "_"), ".csv")
            write.csv(x = data, file = file.path(results_dir, file_name),
                      row.names = FALSE, quote = TRUE)
        }
    })
}

# Create Shiny app ----
shinyApp(ui, server)


