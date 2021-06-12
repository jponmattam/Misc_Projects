library(tidyverse)
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(graphics)
library(gargle)

options(gargle_oauth_cache = "secrets3",
        email=TRUE)

#gs4_auth() ## one time authorization to access google docs

df1<-as_sheets_id("https://docs.google.com/spreadsheets/d/1juWVHkOR_kUT66wsUmgv_OZflDAPW3Aye4KfCHuF6PA/edit#gid=0")
df1.ss <- as.character(df1)
data.df <- read_sheet(df1.ss,   
                      sheet="Sheet1",
                      col_names = TRUE,
                      range = "A:I")

df2<-as_sheets_id("https://docs.google.com/spreadsheets/d/1juWVHkOR_kUT66wsUmgv_OZflDAPW3Aye4KfCHuF6PA/edit#gid=0")
df2.ss <- as.character(df2)
data.df.mtg <- read_sheet(df2.ss,
                          sheet = "Sheet2",
                          col_names = TRUE,
                          col_types = "c",
                          range="A:J")
colnames(data.df.mtg)[6] <- "Journal club"

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "CIPHER Hours"),
    dashboardSidebar(
        ## Input: Name and Date
        selectInput("name", "What's your name?", unique(data.df$Name)),
        selectInput("week", "Select Week:", unique(data.df$Week))
    ),
    dashboardBody(
        tabsetPanel(
            tabPanel("Hours",
                     fluidRow(
                         ## heading
                         column(12, h3("Please Enter Hours for Week:", textOutput("week.date"))),
                         #column(12, textOutput("yes")), ## CHECK that df is read
                         ## Input: Hours
                         column(4, numericInput("Monday", "Monday", value=0, min = 1, max=100)),
                         column(4, numericInput("Tuesday", "Tuesday", value=0, min = 1, max=100)),
                         column(4, numericInput("Wednesday", "Wednesday", value=0, min = 1, max=100)),
                         column(4, numericInput("Thursday", "Thursday", value=0, min = 1, max=100)),
                         column(4, numericInput("Friday", "Friday", value=0, min = 1, max=100)),
                         column(4, actionButton("submit.hours", "Submit", style="height:50px;width:100px"), align="center", style="margin-top:25px"),
                     ),
                     
                     ## Output: Hours Table
                     h3("Your Hours:"),
                     tableOutput("hours.table"),
                     ## Output: Total Hours
                     fluidRow(
                         column(12, h3("Total Hours:", textOutput("total")))
                     )
            ),
            tabPanel("Activities",
                      fluidRow(
                          #column(12, textOutput("no")), ## CHECK that df is read
                          ## heading
                         column(12, h3("Please Enter Activities Attended for Week:", textOutput("week.date1"))),
                          ## Input:
                          column(3, selectInput("atm", "All-Teams Meetings:",  choices=c("Yes", "No"), selected = "No")),
                          column(3, selectInput("pm", "Project Meetings:", choices=c("Yes", "No"), selected = "No")),
                          column(3, selectInput("jc", "Journal Club:", choices=c("Yes", "No"), selected = "No")),
                          column(3, selectInput("sem", "Seminars:", choices=c("Yes", "No"), selected = "No")),
                          column(3, selectInput("wksp", "Workshops", choices=c("Yes", "No"), selected = "No")),
                          column(3, selectInput("pc", "Practicum Check-Ins:", choices=c("Yes", "No"), selected = "No")),
                          column(3, selectInput("oth", "Other:", choices=c("Yes", "No"), selected = "No")),
                          column(3, actionButton("submit.act", "Submit", style="height:50px;width:100px"), align="center", style="margin-top:10px"),
                          ## Output: Hours Table
                          column(12, h3("Your Activities:")),
                      ),
                      tableOutput("activity.table")
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$yes <- renderText({
        if(class(as.data.frame(data.df))=="data.frame") {
            "Yes"
        } else {"No"}
    })
################################################################ get "week.date"
    df.name.wk <- reactive({
        data.df %>% filter(Week==input$week, Name==input$name)
    })
    output$week.date <- renderText({
        df <- df.name.wk()
        paste0(df[,"Date"])
    })
################################################################## TAB: HOURS; hours input -> google sheet and display 
    df.hours <- eventReactive(input$submit.hours, {
        vec.date <- data.df %>% filter(Week==input$week, Name==input$name) %>% select(Date) ## gets date

        hour.df <- data.frame(Name=input$name, Week=input$week, Date=vec.date, Monday=input$Monday, Tuesday=input$Tuesday,
                              Wednesday=input$Wednesday, Thursday=input$Thursday, Friday=input$Friday) %>%
            mutate(Total=rowSums(across(where(is.numeric))))

        df.name <- data.df %>% filter(Name==input$name)
        wk <- input$week
        df.name[df.name$Week==wk,] <- hour.df

        ## send to google sheet
        data.df.row <- data.df %>%
            mutate(id=as.numeric(rownames(.))+1) %>%
            filter(Name==input$name) 

        # ## get range values
        rowF <- first(data.df.row$id)
        rowL <- last(data.df.row$id)
        range.val <- paste0("A",rowF, ":", "I",rowL)

        range_write(df1.ss, data=df.name, range=range.val, col_names = FALSE)

        df.name
    })
###################### display hours table
    ## default
    observeEvent(input$name, {
        output$hours.table <- renderTable({
            data.df %>% filter(Name==input$name)
        })
    })
    
    ## updated
    observeEvent(input$submit.hours, {
        output$hours.table<- renderTable({
            df.hours()
        })
    })

########################## Display total hours
    ## default
    observeEvent(input$name, {
        output$total <- renderText({
            data.df %>%
                filter(Name==input$name) %>%
                select(Total) %>%
                summarise(sum =sum(Total, na.rm = TRUE)) %>%
                as.character()
        })
    })
    
    ## updated
    observeEvent(input$submit.hours,{
        output$total <- renderText({
            df.tot <- df.hours() %>%
                select(Total) %>%
                summarise(sum =sum(Total, na.rm = TRUE)) %>%
                as.character()
        })
    })
######################################### TAB: Attendance #data.df.mtg
######################### get "week.date" again
    df.name.wk1 <- reactive({
        data.df.mtg %>% filter(Week==input$week, Name==input$name)
    })
    output$week.date1 <- renderText({
        df <- df.name.wk1()
        paste0(df[,"Date"])
    })
    
    output$no <- renderText({
        if(class(as.data.frame(data.df.mtg))=="data.frame") {
            "Yes"
        } else {"No"}
    })

    df.activity <- eventReactive(input$submit.act, {
        vec.date <- data.df.mtg %>% filter(Week==input$week, Name==input$name) %>% select(Date) ## gets date
        df.act <- data.frame(Name=input$name, Week=input$week, Date=vec.date, `All-teams meetings`=input$atm, `Project meetings`=input$pm, `Journal club (Tuesday evenings)`=input$jc, `Seminars`=input$sem, `Workshops`=input$wksp, `Practicum check-ins`=input$pc, `Other: Orientation, etc.`=input$oth)
        
        df.name.mtg <- data.df.mtg %>% filter(Name==input$name)
        df.name.mtg[df.name.mtg$Week==input$week,] <- df.act
        
        
        ## send to google sheet
        data.df.row <- data.df.mtg %>%
            mutate(id=as.numeric(rownames(.))+1) %>%
            filter(Name==input$name)

        # ## get range values
        rowF <- first(data.df.row$id)
        rowL <- last(data.df.row$id)
        range.val <- paste0("A",rowF, ":", "J",rowL)

        range_write(df2.ss, data=df.name.mtg, sheet = "Sheet2", range=range.val, col_names = FALSE)
        
        df.name.mtg
    })
    
    ###################### display activities table
    ## default
    observeEvent(input$name, {
        output$activity.table <- renderTable({
            data.df.mtg %>% 
                filter(Name==input$name) %>%
                mutate(across(c("All-teams meetings", "Project meetings", "Journal club", "Seminars", "Workshops", "Practicum check-ins", "Other: Orientation, etc."), ~if_else(.=="Yes", "<b>Yes</b>", "No")))
            }, sanitize.text.function = function(x) x)
    })
    ## updated
    observeEvent(input$submit.act, {
        output$activity.table<- renderTable({
            df.activity() %>%
                mutate(across(c("All-teams meetings", "Project meetings", "Journal club", "Seminars", "Workshops", "Practicum check-ins", "Other: Orientation, etc."), ~if_else(.=="Yes", "<b>Yes</b>", "No")))
        }, sanitize.text.function = function(x) x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
