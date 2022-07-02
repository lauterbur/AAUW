# physics analysis

library(shiny)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(scales)
library(forcats)
library(ggpubr)

# setwd("/home/lauterbur/Desktop/AAUW_2022/physics_module/analysis")
# options(gargle_oauth_cache = ".secrets")
# gs4_auth()
# list.files(".secrets/")
# gs4_deauth()
gs4_auth(cache = ".secrets", email = "elise.lauterbur@gmail.com")

dataURL <- "https://docs.google.com/spreadsheets/d/12IoT_ZDA0P7ypDAoObk69nwHwbQQHOqRtP1VSkjKxGY/edit?usp=sharing"
# Define UI for application 
ui <- fixedPage(
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Frequency vs string length",
                
                    # Application title
                        titlePanel("Data analysis for the effect of string length on frequency"),
                    
                        fixedRow(
                          column(6,
                                 plotOutput("freqPlot")
                          ),
                          column(6,
                                 plotOutput("lengthPlot")
                          )
                        ),
                        # fixedRow(
                        #   plotOutput("freqlengthPlot")
                        # )
                    ),
                
                tabPanel("Musical Installation",
                         radioButtons("mostPop", "Most popular frequency choices",
                                      c("1" = 1,
                                        "2" = 2,
                                        "3" = 3)
                         ),
                         fixedRow(
                             plotOutput("tubeChoices")
                         )
                    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    options(scipen = 999) 
  # 1. plot freq vs ratio
  # 2. picture of relative lengths of most common pitches chosen, and play (or rank combinations by number of times chosen and be able to pick?)
  
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
  
    data <- read_sheet(dataURL,
                       col_types="c")
    data <- data %>%
      mutate(id=row_number())
    colnames(data) <-c("time","notice1","interval1","longfreq","shortfreq","notice2","lowlength","interval2","highlength","freqchoices","lengthchoices","freqpair",id")
    data_long<-data %>%
      pivot_longer(names_to="freq", cols=c("longfreq","shortfreq"),values_to="freqvalue") %>%
      pivot_longer(names_to="length", cols=c("lowlength","highlength"),values_to="lengthvalue") %>%
      mutate(interval1=as.numeric(gsub("\\(","",gsub("\\)","",str_extract_all(interval1, "\\([^()]+\\)")[[1]]))),
             interval2=as.numeric(gsub("\\(","",gsub("\\)","",str_extract_all(interval2, "\\([^()]+\\)")[[1]]))))
    
    output$freqPlot <- renderPlot({ # 
      ggplot(data_long, aes(interval1, freqvalue)) +
        geom_point() +
        theme_bw() +
        xlab("Length ratio") +
        ylab("Frequency") +
        theme(axis.text=element_text(size=15),
              axis.title=element_text(size=20))
    })
    
    output$lengthPlot <- renderPlot({ # 
      ggplot(data_long, aes(interval1, lengthvalue)) +
        geom_point() +
        theme_bw() +
        xlab("Length ratio") +
        ylab("Pipe length (ft)") +
        theme(axis.text=element_text(size=15),
              axis.title=element_text(size=20))
    })
    
    output$freqlengthPlot <- renderPlot({ # 
      ggplot(data_long, aes(lengthvalue, freqvalue)) +
        geom_point() +
        theme_bw() +
        xlab("Pipe length (ft)") +
        ylab("Frequency") +
        theme(axis.text=element_text(size=15),
              axis.title=element_text(size=20))
    })
    
    data_sep <- data %>%
      select(freqchoices,lengthchoices,id) %>%
      pivot_longer(cols=c("freqchoices","lengthchoices"), names_to="type", values_to="choices") %>%
      separate(choices, c("A","B","C","D","E")) %>%
      pivot_longer(cols=c("A","B","C","D","E"), names_to="pipe", values_to="value") %>%
      mutate(value=as.numeric(value))
    
    data_sep_short <- data_sep %>% 
      group_by(id, type) %>% 
      summarise(choices=paste(sort(unique(value)),collapse=", ")) %>%
      group_by(type, choices) %>%
      count(choices) %>%
      rename(common=n) %>%
      separate(choices, c("A","B","C","D","E")) %>%
      pivot_longer(cols=c("A","B","C","D","E"), names_to="pipe", values_to="value") %>%
      group_by(type) %>%
      mutate(value=as.numeric(value))
      
    maxcommon<-reactive({
      unique(sort(data_sep_short$common,decreasing = TRUE))[as.numeric(input$mostPop)]
      })

    output$tubeChoices <- renderPlot({ 
      tubeLengths<-data_sep_short %>%
        filter(type=="lengthchoices",
               common==maxcommon()) %>%
        ggplot(aes(pipe, value)) +
        geom_bar(stat="identity", aes(fill=value)) +
        geom_text(aes(label = value), size=10, nudge_y=-20) +
        scale_fill_viridis_c(begin = .25) +
        theme_bw() +
        xlab("Pipe") +
        ylab("Length (ft)") +
        theme(#axis.title.x = element_blank(),
              #axis.ticks.x = element_blank(),
              #axis.text.x = element_blank(),
              axis.text=element_text(size=15),
              axis.title=element_text(size=20))
      tubeLengths
      # 
      # tubeFreqs<-data_sep_short %>%
      #   filter(type=="freqchoices",
      #          common==common()) %>%
      #   ggplot(aes(pipe, 1, fill=value)) +
      #   geom_tile() +
      #   theme_minimal() +
      #   scale_fill_viridis_c(begin = .25) +
      #   xlab("Pipe") +
      #   ylab("Frequency (Hz)") +
      #   labs(fill="Frequency (Hz)") +
      #   theme(legend.position="bottom",
      #         legend.title=element_text(size=20),
      #         legend.text=element_text(size=15),
      #         axis.ticks.y=element_blank(),
      #         axis.title.y=element_blank(),
      #         axis.text.y=element_blank(),
      #         axis.text.x=element_text(size=15),
      #         axis.title=element_text(size=20)) +
      #   geom_text(aes(label = value), size=5)
      # ggarrange(tubeLengths, tubeFreqs,
      #           nrow=2,
      #           heights=c(4,2),
      #           align="v"
      #           )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
