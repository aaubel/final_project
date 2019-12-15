# Final Project

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

probation <- read.csv("JCPSS_2012-2018.csv") 
mostrecent <- probation %>% filter(REPORT_YEAR==2018) %>% select(REPORT_YEAR, GENDER_DESC, RACE_DESC, AGE_GROUP, ACTION_TYPE_DESC, REFERRAL_TYPE, REFERRAL_SOURCE_DESC, OFFENSE_LEVEL, GROUP_OFFENSE_LEVEL, DISPOSITION_DESC) #Subset for 2018 and relevant variables

mostrecent$ACTION_TYPE_DESC = factor(mostrecent$ACTION_TYPE_DESC, levels(mostrecent$ACTION_TYPE_DESC)[c(2,1)]) #Change order of levels for "Action taken" variable. Makes more sense to have Referral first and Court second, rather than alphabetical. 


ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    titlePanel("Juvenile Probation in California: A Data Dashboard"),
    tabsetPanel(
        tabPanel("Introduction",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Welcome to the California Juvenile Probation Data Dashboard."),
                         p("This dashboard summarizes information on youth referred to probation agencies throughout California. All data are from the", a("California Department of Justice's Juvenile Court and Probation Statistical System (JCPSS).", href="https://openjustice.doj.ca.gov/data")),
                         br(),
                         p("To learn more, watch the video to the right or access project files", a("here.", href="https://github.com/aaubel/final_project"))
                     ),
                     mainPanel(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/vEv3nmsk_J8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                 )),
        
        tabPanel("Time Series",
                 sidebarLayout(
                     sidebarPanel(
                         p("This graphs shows the number of referrals to juvenile probation over time. The black line represents the total number of referrals each year. Use the drop-down menu to see time trends for different groups of individuals and offenses."),
                         br(),
                         selectInput(inputId = "var1", label = "Select variable", choices = c("Gender" = "GENDER_DESC", "Age" = "AGE_GROUP", "Race" = "RACE_DESC", "Offense level" = "OFFENSE_LEVEL"))
                                     ),
                         
                     mainPanel(
                         plotOutput("timeSeries")
                     )
                 )),
        
                 
        tabPanel("Type of Crimes",
                sidebarLayout(
                    sidebarPanel(
                        p("This tile plot shows the prevalence of different types of offenses over time. Use the drop-down menu to see the various types of offenses for each offense level."),
                        br(),
                        selectInput(inputId = "var2", label = "Select variable", choices = as.list(levels(probation$OFFENSE_LEVEL)))
                    ),
                    
                    mainPanel(
                        plotOutput("tilePlot")
                    )
                )),
                 
        
        tabPanel("Demographic Info",
                 sidebarLayout(
                     sidebarPanel(
                     p("These charts describe the demographic characteristics of youth that were referred to probation in 2018. The number at the top of each bar is the percentage of youth in that group. Use the drop-down menu to choose which characteristic you'd like to see."),
                     br(),
                     selectInput(inputId = "var3", label = "Select variable", choices = c("Gender" = "GENDER_DESC", "Age" = "AGE_GROUP", "Race" = "RACE_DESC"))
                 ),
                 
                 mainPanel(
                     plotOutput("barChart"),
                 )
             )),
        
        
        tabPanel("Process Details",
                 sidebarLayout(
                     sidebarPanel(
                     p("These tables reflect youth that were referred to probation in 2018 and the various stages in the juvenile justice process. Select a variable from the drop-down menu for more info."),
                     br(),
                     selectInput(inputId = "var4", label = "Select variable", choices = c("Referral Source" = "REFERRAL_SOURCE_DESC", "Referral Type" = "REFERRAL_TYPE", "Offense Level" = "OFFENSE_LEVEL", "Action Taken" = "ACTION_TYPE_DESC"))
                     ),
                     
                    mainPanel(
                    tableOutput("firstTable"),
                    br(),
                    h4(textOutput("caption1"))
                    )
               )),
        
        tabPanel("Outcomes",
                 sidebarLayout(
                     sidebarPanel(
                     p("This page shows the outcomes (or 'Dispositions') of cases that were handled by probation and those that were sent to juvenile court. Use the drop-down menu to see the outcomes for each process and to learn which variables predict the more serious outcomes."),
                     br(),
                     selectInput(inputId = "var5", label = "Select variable", choices = as.list(levels(mostrecent$ACTION_TYPE_DESC)))
                ),
                
                    mainPanel(
                    tableOutput("secondTable"),
                    br(),
                    h4(textOutput("caption2"))
                ))
    )))


server <- function(input, output) {
    output$timeSeries <- renderPlot({
        if(input$var1=="GENDER_DESC"){time <- probation %>% 
            group_by(REPORT_YEAR, GENDER_DESC) %>% 
            summarize(count=n()) %>% 
            mutate(var = GENDER_DESC)}
        if(input$var1=="AGE_GROUP"){time <- probation %>%
            group_by(REPORT_YEAR, AGE_GROUP) %>%
            summarize(count=n()) %>%
            mutate(var = AGE_GROUP)}
        if(input$var1=="RACE_DESC"){time <- probation %>%
            group_by(REPORT_YEAR, RACE_DESC) %>%
            summarize(count=n()) %>%
            mutate(var = RACE_DESC)}
        if(input$var1=="OFFENSE_LEVEL"){time <- probation %>%
            group_by(REPORT_YEAR, OFFENSE_LEVEL) %>%
            summarize(count=n()) %>%
            mutate(var = OFFENSE_LEVEL)}
        
        total <- probation %>% group_by(REPORT_YEAR) %>% summarize(count=n())        
        
        ggplot() +
            geom_line(data = total, aes(x=REPORT_YEAR, y=count/1000)) +
            geom_line(data = time, aes(x=REPORT_YEAR, y=count/1000, color=as.factor(var))) +
            geom_text(aes(x=2018, y=75), label = "Total") +
            scale_x_continuous(breaks=c(2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
            theme(legend.title = element_blank()) +
            xlab("Year") +
            ylab("Number of cases (in thousands)") +
            ggtitle("Number of cases over time, 2012-2018")
    })
    
    output$tilePlot <- renderPlot({
        tile_type <- probation %>% filter(OFFENSE_LEVEL==input$var2) %>% group_by(REPORT_YEAR, GROUP_OFFENSE_LEVEL) %>% summarize(count=n())
        
        tile_type %>% ggplot() + 
            geom_tile(aes(x=REPORT_YEAR, y=GROUP_OFFENSE_LEVEL, fill=count)) + 
            scale_x_continuous(breaks=c(2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
            scale_fill_viridis_c("Number of cases", trans="sqrt") +
            ggtitle("Number of cases over time by type of offense, 2012-2018") +
            xlab("Year") +
            ylab("")
    })
    
    output$barChart <- renderPlot({
        if(input$var3=="GENDER_DESC"){mostrecent <- mostrecent %>%
            mutate(var = GENDER_DESC)}
        
        if(input$var3=="AGE_GROUP"){mostrecent <- mostrecent %>%
            mutate(var = AGE_GROUP)}
        
        if(input$var3=="RACE_DESC"){mostrecent <- mostrecent %>%
            mutate(var = RACE_DESC)}
        
        value <- mostrecent %>% group_by(var) %>% summarize(count=n(), percent=count/nrow(mostrecent)*100)
        
        ggplot() + 
            geom_bar(data = mostrecent, aes(x=var, fill=var)) +
            geom_text(data = value, aes(x=var, y=count, label=round(percent, digits=1)), nudge_y = 1000) +
            scale_fill_brewer(palette = "BuGn") +
            theme(legend.position = "none") +
            ggtitle("Number and percent of cases by demographic group, 2018") +
            xlab("") +
            ylab("Number of cases")
    })
    
    
    output$firstTable <- renderTable({
       if(input$var4=="REFERRAL_SOURCE_DESC"){mostrecent_new <- mostrecent %>% mutate(Variable=REFERRAL_SOURCE_DESC)}
        if(input$var4=="REFERRAL_TYPE"){mostrecent_new <- mostrecent %>% mutate(Variable=REFERRAL_TYPE)}
        if(input$var4=="OFFENSE_LEVEL"){mostrecent_new <- mostrecent %>% mutate(Variable=OFFENSE_LEVEL)}
        if(input$var4=="ACTION_TYPE_DESC"){mostrecent_new <- mostrecent %>% mutate(Variable=ACTION_TYPE_DESC)}
        
         mostrecent_new %>% group_by(Variable) %>% summarize(Count=n(), Percent=Count/nrow(mostrecent)*100)
    })
    
    output$caption1 <- renderText({
        if(input$var4=="REFERRAL_SOURCE_DESC"){text <- "The majority of cases (88.9%) were referred to probation by law enforcement agencies."}
        if(input$var4=="REFERRAL_TYPE"){text <- "Roughly two-thirds (69%) of referrals were new, or youth who are typically first-time offenders. One-third (31%) were subsequent referrals, which refers to youth who were already being supervised by the probation department and generally results from a new arrest or probation violation."}
        if(input$var4=="OFFENSE_LEVEL"){text <- "Half of all cases were for misdemeanor-level offenses, followed by felonies and status offenses. Status offenses, like skipping school and running away, are behaviors that are unlawful only when they are committed by minors."}
        if(input$var4=="ACTION_TYPE_DESC"){text <- "45% of cases were handled informally by the probation department ('Referral'). The other 55% were transferred to juvenile court for formal processing ('Court')."}
        text
    })
    
    output$secondTable <- renderTable({
        mostrecent2 <- mostrecent %>% filter(ACTION_TYPE_DESC==input$var5)
        
        mostrecent2 %>% group_by(DISPOSITION_DESC) %>% summarize(Count=n(), Percent=Count/nrow(mostrecent2)*100)
})

    
    output$caption2 <- renderText({
        if(input$var5=="Referral"){text2 <- "Among the cases handled informally by the probation department, the majority (73%) were closed at intake. The odds of being sent to juvenile court were significantly higher among males, non-Whites, older youth, and cases that were referred by law enforcement, subsequent referrals, and felony-level offenses."}
        if(input$var5=="Court"){text2 <- "Among the cases sent to juvenile court, 60% resulted in wardship probation, meaning the Court has jurisdiction over the minor as if the Court was the minor’s parent. The odds of wardship were significantly higher among males, non-Whites, 16- and 17-year olds, and cases that were referred by law enforcement, subsequent referrals, and status offenses."}
        text2
    })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
