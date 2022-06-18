#

library(shiny)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(scales)
library(forcats)

dataURL <- "https://docs.google.com/spreadsheets/d/1aQQEHSNAnLhGVN4ywWG7ojlYORfyhJV_x_tmp5o8O6k/edit?usp=sharing"

# setwd("/home/lauterbur/Desktop/AAUW_2022/bio_module/bio_data_testing/")
# options(gargle_oauth_cache = ".secrets")
# gs4_auth()
# list.files(".secrets/")
# gs4_deauth()
gs4_auth(cache = ".secrets", email = "elise.lauterbur@gmail.com")

# Define UI for application
ui <- fixedPage(title="Data analysis for genetic and environmental diabetes risk",
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  
  tabsetPanel(type = "tabs", header="Data analysis for genetic and environmental diabetes risk",
              tabPanel("Effect of food",
                       
                       # Application title
                       titlePanel("Effect of food on developing diabetes"),
                       
                       fixedRow(
                         plotOutput("exp1")
                       ),
              ),
              
              tabPanel("Effect of genes",
                       titlePanel("Effect of genetic risk on developing diabetes"),
                       fixedRow(
                         column(6,
                                plotOutput("exp2")
                                ),
                         column(6,
                                plotOutput("exp3")
                                )
                       )
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(scipen = 999) 
  
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  data <- read_sheet(dataURL)
  data_long <- data %>%
    select(c(2,3,4,8,9,10,11,12,13)) %>%
    dplyr::mutate(id=1:n()) %>%
    rename_at(vars(everything()), 
              ~c("allrats_1", "sugar_risk_1", "nosugar_risk_1",
             "allrats_2", "sugar_norisk_2", "sugar_risk_2",
             "allrats_3", "nosugar_norisk_3", "nosugar_risk_3",
             "id")) %>%
    pivot_longer(cols=-c(id, starts_with("all")),
                 names_sep="_",
                 names_to=c("sugar","risk","experiment"),
                 values_to="diabetic_rats") %>%
    group_by(id)
  
  # make plots
  output$exp1 <- renderPlot({
    data_summary(data_long %>% filter(experiment==1),varname="diabetic_rats",groupnames="sugar") %>% 
      ggplot() + aes(x=sugar, y=diabetic_rats, fill=sugar) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(ymin=diabetic_rats-sd, ymax=diabetic_rats+sd),
                    width=.2) +
      labs(#title="Effect of sugary food on developing diabetes",
           fill="Food") +
      xlab("Food") +
      ylab("Number of diabetic rats") +
      scale_fill_hue(labels=c("No sugar","Sugar")) +
      scale_x_discrete(labels=c("nosugar"="No sugar","sugar"="Sugar")) +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=18,face="bold"),
            title=element_text(size=20,face="bold"),
            legend.text=element_text(size=12))
  })
  
  output$exp2 <- renderPlot({
    data_summary(data_long %>% filter(experiment==2),varname="diabetic_rats",groupnames="risk") %>% 
      ggplot() + aes(x=risk, y=diabetic_rats, fill=risk) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(ymin=diabetic_rats-sd, ymax=diabetic_rats+sd),
                    width=.2) +
      labs(title="With sugary food",
           fill="Genetic risk") +
      xlab("Risk") +
      ylab("Number of diabetic rats") +
      scale_fill_hue(labels=c("No genetic risk", "Genetic risk")) +
      scale_x_discrete(labels=c("norisk"="No genetic risk","risk"="Genetic risk")) +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=18,face="bold"),
            title=element_text(size=20,face="bold"),
            legend.text=element_text(size=12))
  })
  
  output$exp3 <- renderPlot({
    data_summary(data_long %>% filter(experiment==3),varname="diabetic_rats",groupnames="risk") %>% 
      ggplot() + aes(x=risk, y=diabetic_rats, fill=risk) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(ymin=diabetic_rats-sd, ymax=diabetic_rats+sd),
                    width=.2) +
      labs(title="Without sugary food",
           fill="Genetic risk") +
      xlab("Risk") +
      ylab("Number of diabetic rats") +
      scale_fill_hue(labels=c("No genetic risk", "Genetic risk")) +
      scale_x_discrete(labels=c("norisk"="No genetic risk","risk"="Genetic risk")) +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=18,face="bold"),
            title=element_text(size=20,face="bold"),
            legend.text=element_text(size=12))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)