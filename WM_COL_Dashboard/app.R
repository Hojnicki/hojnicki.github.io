library(shiny)
library(shinydashboard)
library(xlsx)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(maps)
library(ggthemes)
library(mapproj)
library(scales)
library(shinythemes)
library(rsconnect)

#Data import 
df <- read.xlsx2('raw.xlsx', sheetIndex = 1, startRow = 1, password = NULL)

#prep columns
cols.num <- names(df[,-c(1:3)])
df[cols.num] <- sapply(df[cols.num], as.numeric)
df$Program <- as.factor(df$Program)
df$Year <- as.factor(df$Year)
df$Class.Start <- as.factor(df$Class.Start)

#set color palette 
cbPalette <- c("#183028", "#B9975B", "#789D4A", "#F0B323", "#D0D3D4")


#Set up enrollment data
x <- df %>% select(Program, Year, Class.Start, Total = Total.) %>% group_by(Program, Year, Class.Start)

#Set up state data
proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
states <- df %>% select(Program, Year, Class.Start, Virginia:US.Virgin.Islands)
states <- states %>% gather(State, Number, Virginia:US.Virgin.Islands)
states2 <- map_data('state')
states$State <- tolower(states$State)
states3 <- left_join(states2, states, by = c('region' = 'State'))
states4 <- states3 %>% select(Program, Year, Class.Start, State = region, Number)
states4$State <- as.factor(proper(states4$State))
states4 <- unique(states4)

#Set up demographic data
mf <- df %>% select(Program:Class.Start, Male:Choose.Not.to.Report, W.M.Alumni, Veteran...Active.Duty.Military, US.Minority, Holds.Advanced.Degree,Mean.Experience..years., Mean.Starting.Salary)
MFratio <- mf %>% select(Program:Class.Start,Male,Female)
MFratio$Male <- MFratio$Male * (-1)
MFratio <- MFratio %>% gather(Gender, Number, Male:Female)
MFratio$SessionYear <- paste(MFratio$Class.Start, ":", MFratio$Year)
MFratio$SessionYear <- as.factor(MFratio$SessionYear)
MFratio$SessionYear <- factor(MFratio$SessionYear, levels = c('Fall : 2015', 'Spring : 2016', 'Summer : 2016', 'Fall : 2016',
                                                                'Spring : 2017', 'Summer : 2017', 'Fall : 2017', 'Spring : 2018',
                                                                'Summer : 2018', 'Fall : 2018', 'Spring : 2019', 'Summer : 2019', 
                                                                'Fall : 2019', 'Spring : 2020', 'Summer : 2020'))


ethnic <- df %>% select(Program:Class.Start, African.American:Choose.Not.to.Report)
ethnic <- ethnic %>% gather(Ethnicity, Number, African.American:Choose.Not.to.Report)
ethnic$SessionYear <- paste(ethnic$Class.Start, ":", ethnic$Year)

assorted <- df %>% select(Program:Class.Start, Veteran...Active.Duty.Military, W.M.Alumni, Holds.Advanced.Degree, US.Minority, Legacy)
assorted <- assorted %>% gather(Special.Class, Number, Veteran...Active.Duty.Military:Legacy)
assorted$SessionYear <- paste(assorted$Class.Start, ":", assorted$Year)
assorted$SessionYear <- factor(assorted$SessionYear, levels = c('Fall : 2015', 'Spring : 2016', 'Summer : 2016', 'Fall : 2016',
                                                                'Spring : 2017', 'Summer : 2017', 'Fall : 2017', 'Spring : 2018',
                                                                'Summer : 2018', 'Fall : 2018', 'Spring : 2019', 'Summer : 2019', 
                                                                'Fall : 2019', 'Spring : 2020', 'Summer : 2020'))

age <- df %>% select(Program:Class.Start, Mean.Age:Age.Range.End)
age$SessionYear <- paste(age$Class.Start, ":", age$Year) 
age$SessionYear <- factor(age$SessionYear, levels = c('Fall : 2015', 'Spring : 2016', 'Summer : 2016', 'Fall : 2016',
                                                                'Spring : 2017', 'Summer : 2017', 'Fall : 2017', 'Spring : 2018',
                                                                'Summer : 2018', 'Fall : 2018', 'Spring : 2019', 'Summer : 2019', 
                                                                'Fall : 2019', 'Spring : 2020', 'Summer : 2020'))

# ui for the app
ui <- fluidPage(
    
    h1(id="big-heading", HTML("<b>Center for Online Learning Program Data</b>"),
    tags$style(HTML("#big-heading{color: #B9975B;}"))),
    #tags$head(
    #    tags$link(rel = 'stylesheet',type = 'text/css', href = 'custom.css')),
    #),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(position = 'right',
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            div(img(src = 'COL_stacked.v2.png', height = 180, width = 180), style = 'text-aling: center;'), 
            'We can put text or content here that will appear throughout the entire user experience',
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel("Enrollment", 
                                 selectInput(inputId = 'Program',
                                             label = 'Select a Program',
                                             choices = unique(df$Program)),
                                 plotOutput(outputId = "enrollment"), DTOutput('enrollmentTable'), br(),
                                 div(img(src = 'COL .png', height = 139), style = 'text-aling: center;')),
                        tabPanel("Home State Data",
                                 fluidRow(
                                     column(4,selectInput(inputId = 'Program2',
                                                                    label = 'Select a Program',
                                                                    choices = unique(df$Program))),
                                     column(4,selectInput(inputId = 'Year',
                                                  label = 'Select a Year',
                                                  choices = unique(df$Year),
                                                  selected = '2020')), 
                                     column(4,selectInput(inputId = 'Start',
                                                 label = 'Select Session Start',
                                                 choices = unique(df$Class.Start),
                                                 multiple = F))),
                                 br(),
                                 plotOutput(outputId = 'state'), DTOutput('stateTable')),
                        tabPanel('Student Diversity',
                                 fluidRow(
                                     column(4,selectInput(inputId = 'Program3',
                                                          label = 'Select a Program',
                                                          choices = unique(df$Program))),
                                     column(4,selectInput(inputId = 'SessionYear2',
                                                          label = 'Select a Session Start and Year',
                                                          choices = unique(ethnic$SessionYear),
                                                          selected = 'Summer : 2020'))), 
                                 br(),
                                 fluidRow(
                                     column(6,plotOutput(outputId = 'ethnic')),
                                     column(6,plotOutput(outputId = 'mfRatio'))
                                     ),
                                 br(),
                                 fluidRow(
                                     column(12, plotOutput(outputId = 'assort'))
                                     ),
                                 br(),
                                 br(),
                                 fluidRow(
                                     align = 'center',
                                     'Alternatively we could break all of these assorted lines into multiple charts. We can also do things like add a trendline with a 95% confidence interval band. Below is an example.'
                                 ),
                                 br(),
                                 br(),
                                 fluidRow(
                                     column(12, plotOutput(outputId = 'alumni')),
                                 ),
                                 br(),
                                 fluidRow(
                                     column(12, plotOutput(outputId = 'veterans'))
                                 ),
                                 br(),
                                 fluidRow(
                                     column(12, plotOutput(outputId = 'minority'))
                                 ),
                                 br(),
                                 fluidRow(
                                     column(12, plotOutput(outputId = 'degree'))
                                 ),
                                 br(),
                                 fluidRow(
                                     column(12, plotOutput(outputId = 'legacy'))
                                 )
                            )
                        #tabPanel('# of Alumni'),
                        #tabPanel('Faculty and Section Leaders'),
                        #tabPanel('COL Facts'))
            
        )
    )
))

server <- function(input, output) {
    
###############################################################################################################
#enrollment graphs#
    output$enrollment <- renderPlot({
        #x <- df %>% select(Program, Year, Class.Start, Total = Total.) %>% group_by(Program, Year, Class.Start
        x %>% filter(Program == input$Program) %>% 
            ggplot(aes(Year, Total, group = Class.Start, color = Class.Start))+ 
            geom_line(size = 2) + 
            scale_colour_manual(values = cbPalette,name = 'Session Start') +
            facet_grid(cols = vars(Program)) +
            theme_minimal() + ylab('Total Number of Admitted Students') +
            theme(strip.background = element_blank(), strip.text.x = element_blank()) +
            ggtitle('Total Admitted Students by Program and Session Start')
    })
    
    output$enrollmentTable <- renderDataTable(x, filter = 'top', server = FALSE, extensions = 'Buttons',
                                              options = list( pageLength = 25,
                                                              dom = 'Bfrtip',
                                                              buttons = c('copy', 'excel','pdf','print')))
################################################################################################################
#home state data graphs#
    output$state <- renderPlot({
        states3 %>% filter(Program == input$Program2, Number > 0, Year == input$Year) %>%
            ggplot(aes(long, lat, group = group, fill = Program)) + geom_polygon(color = 'gray90', size = 0.05) + 
            coord_map('albers', lat0=39, lat1 = 45) + scale_fill_manual(values = cbPalette) + theme_map() + 
            labs(title = 'Which states our students come from') + theme(plot.title = element_text(size = 24, colour = '#B9975B'))
        
    })
    
    output$stateTable <- renderDataTable(states4, filter = 'top', server = FALSE, extensions = 'Buttons',
                                         options = list( pageLength = 25,
                                                         dom = 'Bfrtip',
                                                         buttons = c('copy', 'excel','pdf','print')))
#################################################################################################################
#Student Diversity Graphs

    output$mfRatio <- renderPlot({
        # X Axis Breaks and Labels 
        brks <- seq(-100, 100, 10)
        lbls = paste0(as.character(c(seq(100, 0, -10), seq(10, 100, 10))))
        MFratio  %>% filter(Program == input$Program3) %>% 
            ggplot(aes(x = SessionYear, y = Number, fill = Gender)) + geom_bar(stat = 'identity', width = .6) +
            theme_minimal() + 
            scale_y_continuous(breaks = brks, labels = lbls) +
            coord_flip() + 
            scale_fill_manual(values = c('#183028', '#B9975B')) + ggtitle('Gender Breakdown') + ylab('Number of Students') +
            theme(plot.title = element_text(size = 16, colour = '#183028')) + xlab('Session Start')
    })
    
    output$ethnic <- renderPlot({
        ethnic %>% filter(SessionYear == input$SessionYear2 & Program == input$Program3 & Number > 0) %>%
            ggplot(aes(Ethnicity, Number, fill = '#183028')) + geom_col(position = 'dodge') + coord_flip() +
            scale_fill_manual(values = c('#B9975B','green')) +
            theme_minimal() + theme(legend.position = "none") +
            ggtitle('Student body demographics')+
            theme(plot.title = element_text(size = 16, colour = '#183028', hjust = 1))
    })
    
    output$assort <- renderPlot({
        assorted %>% filter(Program == input$Program3 & Number > 0) %>%
            ggplot(aes(SessionYear, Number, group = Special.Class, color = Special.Class))+ 
                geom_line(size = 2) + 
                scale_colour_manual(values = cbPalette,name = 'Class Session Start') +
                #facet_grid(cols = vars(Program)) + 
                theme(strip.background = element_blank(), strip.text.x = element_blank()) + theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90)) + ggtitle('Assorted Demographics')+
            theme(plot.title = element_text(size = 16, colour = '#183028')) + ylab('Number of Students') + xlab('Session Start')
    })
    
    output$alumni <- renderPlot({
        assorted %>% filter(Program == input$Program3 & Number > 0, Special.Class == 'W.M.Alumni') %>%
            ggplot(aes(SessionYear, Number, group = Special.Class, color = Special.Class))+ 
            geom_line(size = 2) + 
            scale_colour_manual(values = cbPalette,name = 'Class Session Start') +
            theme(strip.background = element_blank(), strip.text.x = element_blank()) + theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90)) + ggtitle('William & Mary Alumni')+
            theme(plot.title = element_text(size = 16, colour = '#183028'), legend.position = 'none') +
            ylab('Number of Students') + xlab('Session Start') +
            geom_smooth(method = lm)
    })
    
    output$veterans <- renderPlot({
        assorted %>% filter(Program == input$Program3 & Number > 0, Special.Class == 'Veteran...Active.Duty.Military') %>%
            ggplot(aes(SessionYear, Number, group = Special.Class, color = Special.Class))+ 
            geom_line(size = 2) + 
            scale_colour_manual(values = cbPalette,name = 'Class Session Start') +
            theme(strip.background = element_blank(), strip.text.x = element_blank()) + theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90)) + ggtitle('Veterans & Active Duty Military')+
            theme(plot.title = element_text(size = 16, colour = '#183028'), legend.position = 'none') +
            ylab('Number of Students') + xlab('Session Start') +
            geom_smooth(method = lm)
    })
    
    output$minority <- renderPlot({
        assorted %>% filter(Program == input$Program3 & Number > 0, Special.Class == 'US.Minority') %>%
            ggplot(aes(SessionYear, Number, group = Special.Class, color = Special.Class))+ 
            geom_line(size = 2) + 
            scale_colour_manual(values = cbPalette,name = 'Class Session Start') +
            theme(strip.background = element_blank(), strip.text.x = element_blank()) + theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90)) + ggtitle('US Minority')+
            theme(plot.title = element_text(size = 16, colour = '#183028'), legend.position = 'none') +
            ylab('Number of Students') + xlab('Session Start') +
            geom_smooth(method = lm)
    })
    
    output$degree <- renderPlot({
        assorted %>% filter(Program == input$Program3 & Number > 0, Special.Class == 'Holds.Advanced.Degree') %>%
            ggplot(aes(SessionYear, Number, group = Special.Class, color = Special.Class))+ 
            geom_line(size = 2) + 
            scale_colour_manual(values = cbPalette,name = 'Class Session Start') +
            theme(strip.background = element_blank(), strip.text.x = element_blank()) + theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90)) + ggtitle('Student holds an Advanced Degree')+
            theme(plot.title = element_text(size = 16, colour = '#183028'), legend.position = 'none') +
            ylab('Number of Students') + xlab('Session Start') +
            geom_smooth(method = lm)
    })
    
    output$legacy <- renderPlot({
        assorted %>% filter(Program == input$Program3 & Number > 0, Special.Class == 'Legacy') %>%
            ggplot(aes(SessionYear, Number, group = Special.Class, color = Special.Class))+ 
            geom_line(size = 2) + 
            scale_colour_manual(values = cbPalette,name = 'Class Session Start') +
            theme(strip.background = element_blank(), strip.text.x = element_blank()) + theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90)) + ggtitle('Student is a Legacy')+
            theme(plot.title = element_text(size = 16, colour = '#183028'), legend.position = 'none') +
            ylab('Number of Students') + xlab('Session Start') +
            geom_smooth(method = lm)
    })

    
}

shinyApp(ui = ui, server = server)
