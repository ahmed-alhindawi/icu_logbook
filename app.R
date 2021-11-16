library(tidyverse)
library(googlesheets4)
library(shiny)
library(lubridate)
library(patchwork)

ui <- fluidPage(
  
  titlePanel(div(column(width=12, h2("ICU Logbook", align="left"))),
             windowTitle="ICU Logbook"),
  div(column(width=12, 
             p("This website facilitates the analysis and generation of the ICU Logbooks"),
             p("To use, first upload the logbook as a CSV file, then select the date range you are interested in."))),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("target_file", 
                label="Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      dateRangeInput('date', label = 'Date range (dd/mm/yyyy):', start = Sys.Date() - 90, end = Sys.Date(), format="dd/mm/yyyy"),
      textInput('author', label = 'Author\'s Name', value = "", width = NULL, placeholder = "Author\'s Name"),
      downloadButton('report', "Generate Report"),
      actionButton('refreshData', 'Refresh Data')
    ),
    mainPanel(tabsetPanel(type = "tabs",
                          tabPanel("Cases", plotOutput("casePlots", height = "500px"), icon = icon("chart-bar")),
                          tabPanel("Diagnoses", dataTableOutput("diagnosisTable"), icon = icon("table")),
                          tabPanel("Procedures", plotOutput("procedurePlots", height = "500px"), icon = icon("chart-bar")),
                          tabPanel("Transfers", dataTableOutput("transferTable"), icon = icon("table")),
                          tabPanel("Teaching", dataTableOutput("teachingTable"), icon = icon("table"))
    ))
  )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    df <- eventReactive(input$target_file, {
      read_csv(input$target_file$datapath) %>% 
        rename(input_type = `Type...3`,
               admit_type = `Type...9`,
               transfer_type = `Type...14`) %>% 
        mutate(Date = case_when(
          is.na(Date) ~ as.Date(dmy_hms(Timestamp)),
          TRUE ~ dmy(Date))
        )}, ignoreNULL = FALSE)

    output$casePlots <- renderPlot({
      admit_type_plot <- df() %>% 
        filter(input_type == "Case") %>% 
        distinct(Identifier, .keep_all = TRUE) %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        ggplot(aes(x=admit_type)) +
        geom_histogram(stat="Count") +
        ggtitle("Admission Type Distribution") +
        theme_classic() +
        theme(axis.title = element_blank())
      
      age_plot <- df() %>% 
        filter(input_type == "Case") %>% 
        distinct(Identifier, .keep_all = TRUE) %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        ggplot(aes(x=Age)) +
        geom_density() +
        ggtitle("Age (years) Distribution") +
        theme_classic() +
        theme(axis.title = element_blank())
      
      gender_plot <- df() %>% 
        filter(input_type == "Case") %>% 
        distinct(Identifier, .keep_all = TRUE) %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        ggplot(aes(x=Gender)) +
        geom_histogram(stat="count") +
        ggtitle("Sex Distribution") +
        theme_classic() +
        theme(axis.title = element_blank())
      
      speciality_type_plot <- df() %>% 
        filter(input_type == "Case") %>% 
        distinct(Identifier, .keep_all = TRUE) %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        ggplot(aes(x=`Referring speciality`)) +
        geom_histogram(stat="count") +
        ggtitle("Speciality Distribution") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title = element_blank())
      
      speciality_type_plot / (age_plot + gender_plot + admit_type_plot)
    })
    
    output$diagnosisTable <- renderDataTable({
      df() %>% 
        filter(input_type == "Case") %>% 
        distinct(Identifier, .keep_all = TRUE) %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        group_by(Diagnosis) %>% 
        summarise(Count = n(), .groups="drop_last") %>% 
        arrange(desc(Count))
    })
    
    output$procedurePlots <- renderPlot({
      procedure_plot <- df() %>% 
        filter(input_type == "Procedure") %>%
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        ggplot(aes(x=`Procedure`)) +
        geom_histogram(stat="count") +
        labs(y="Count") +
        ggtitle("Procedure") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title = element_blank())
      
      supervision_plot <- df() %>% 
        filter(input_type == "Procedure") %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        ggplot(aes(x=`Supervision`)) +
        geom_histogram(stat="count") +
        labs(y="Count") +
        ggtitle("Supervision") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title = element_blank())
      
      procedure_plot + supervision_plot
    })
    
    output$transferTable <- renderDataTable({
      df() %>% 
        filter(input_type == "Transfer") %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        group_by(transfer_type, Reason) %>% 
        summarise(Count = n(), .groups="drop_last") %>% 
        arrange(desc(Count)) %>% 
        rename(`Transfer Type`= transfer_type)
    })
    
    output$teachingTable <- renderDataTable({
      df() %>% 
        filter(input_type == "Teaching") %>% 
        filter(between(`Date`, input$date[1], input$date[2])) %>% 
        select(Date, `Title of Teaching`) %>% 
        mutate(Date = format(Date, "%d/%m/%Y"))
    })
    
    output$report <- downloadHandler(
      filename = function(){
        paste0("report.zip")
      },
      content = function(file){
        temp_folder = tempfile()
        withProgress(message = 'Generating...', value = 0, {
            restricted_df <- df() %>% 
              filter(between(`Date`, input$date[1], input$date[2]))
            
            incProgress(0, detail = "graphs")
            rmarkdown::render("_logbook_report.Rmd", 
                              output_file="logbook.pdf", 
                              output_dir = paste0(temp_folder, "/reports"),
                              params = list(
                                dataframe = restricted_df,
                                author = input$author,
                                start_date=input$date[1],
                                end_date=input$date[2]
                                )
                              )
            incProgress(1, detail = "graphs")
          
          curr_wd = getwd()
          setwd(temp_folder)
          zip(zipfile = file, files = dir(".", full.names = TRUE))
          setwd(curr_wd)
          
        })
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
