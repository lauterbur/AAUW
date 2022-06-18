# physics analysis

library(shiny)
library(googlesheets4)
library(tidyverse)
library(wordcloud)
library(tm)

# setwd("/home/lauterbur/Desktop/AAUW_2022/pathways_module/analysis")
# options(gargle_oauth_cache = ".secrets")
# gs4_auth()
# list.files(".secrets/")
# gs4_deauth()
gs4_auth(cache = ".secrets", email = "elise.lauterbur@gmail.com")

dataURL <- "https://docs.google.com/spreadsheets/d/108k8Utaiq7sP98TBflZloB8TdFyvNlIiWX4ntVeQ_rg/edit?usp=sharing"
# Define UI for application 
ui <- fixedPage(
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Skills",
                
                    # Application title
                        titlePanel("Your skills"),
                    
                        fixedRow(
                          plotOutput("skillCloud")
                          )
                        ),

                tabPanel("Help",
                       #  titlePanel("What you would like help with"),
                         
                       column(6,
                         fixedRow(
                           h2(textOutput("studentHelp"))
                         ),
                         fixedRow(
                           plotOutput("studentHelpCloud")
                         )
              ),
                
              #  tabPanel("Ways to Help",
                      #   titlePanel("How caregivers can help"),
                         
                      column(6,
                         fixedRow(
                           h2(textOutput("caregiverHelp"))
                         ),
                         fixedRow(
                           plotOutput("caregiverHelpCloud")
                         )
                ),
                ),
                
                tabPanel("Confidence",
                       #  titlePanel("Things you are confident about"),
                      column(6,   
                         fixedRow(
                           h2(textOutput("studentConfidence"))
                         ),
                         fixedRow(
                           plotOutput("studentConfidenceCloud")
                         )
                ),
                
            #    tabPanel("Caregiver confidence",
            #             titlePanel("What your caregivers have confidence in about you"),
                      column(6,
                         fixedRow(
                           h2(textOutput("caregiverConfidence"))
                         ),
                         fixedRow(
                           plotOutput("caregiverConfidenceCloud")
                         )
                )
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    options(scipen = 999) 

  data <- read_sheet(dataURL)
    colnames(data) <-c("time","studentSkills1","example1","studentSkills2","example2","studentHelp","why1","caregiverHelp","studentConfidence","why2","caregiverConfidence","why3")
    data_long <- data %>%
      mutate(id=row_number()) %>%
      pivot_longer(names_to="studentSkillsLabel", cols=c("studentSkills1","studentSkills2"),values_to="studentSkills") %>%
      select(-c("studentSkillsLabel",contains("example"),contains("why"),"time")) %>%
      mutate(studentSkills=str_replace_all(studentSkills, "!", ""),
             studentHelp=str_replace_all(studentHelp, "!", ""),
             caregiverHelp=str_replace_all(caregiverHelp, "!", ""),
             studentConfidence=str_replace_all(studentConfidence, "!", ""),
             caregiverConfidence=str_replace_all(caregiverConfidence, "!", "")) %>%
      rbind(c('college applications',"money","SAT score","staying on track",10000,"math"))
    
    output$skillCloud <- renderPlot({ # 
      wordcloud(data_long$studentSkills, colors=brewer.pal(8, "Spectral"))
    })
    
    output$studentHelp <- renderText({
      "What you would like help with"
    })
    
    output$caregiverHelp <- renderText({
      "How your caregivers can help"
    })
    
    output$studentHelpCloud <- renderPlot({ # 
      wordcloud(data_long$studentHelp, colors=rev(brewer.pal(8, "Set1")))
    })
    
    output$caregiverHelpCloud <- renderPlot({ # 
      wordcloud(data_long$caregiverHelp, colors=rev(brewer.pal(8, "Set2")))
    })
    
    output$studentConfidence <- renderText({
      "Things you are confident about"
    })
    
    output$caregiverConfidence <- renderText({
      "What your caregivers have confidence in you about"
    })
    
    output$studentConfidenceCloud <- renderPlot({ # 
      wordcloud(data_long$studentConfidence, colors=rev(brewer.pal(8, "Accent")))
    })
    
    output$caregiverConfidenceCloud <- renderPlot({ # 
      wordcloud(data_long$caregiverConfidence, colors=rev(brewer.pal(8, "Dark2")))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
