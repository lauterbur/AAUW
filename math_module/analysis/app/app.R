# math analysis

library(shiny)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(scales)
library(forcats)

# setwd("/home/lauterbur/Desktop/AAUW_2022/math_module/analysis")
# options(gargle_oauth_cache = ".secrets")
# gs4_auth()
# list.files(".secrets/")
# gs4_deauth()
# gs4_auth(cache = ".secrets", email = "elise.lauterbur@gmail.com")

dataURL <- "https://docs.google.com/spreadsheets/d/1Jxy-6f-RbHFttzZ2ZJsPUYe6Nj3rIWz3bF-tb5AzHLY/edit?usp=sharing"
# Define UI for application 
ui <- fixedPage(
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Costs, Houses, People",
                
                    # Application title
                        titlePanel("Data analysis for house building project"),
                    
                        fixedRow(
                            column(6,
                                   plotOutput("peoplePlot")
                            ),
                            column(6,
                                   plotOutput("housePlot")
                            )
                        ),
                        fixedRow(
                            radioButtons("houseGroup", "Group by house type?",
                                         c("No" = "no",
                                           "Yes" = "yes")
                                        ),
                            plotOutput("costPeople")
                        )
                    ),
                
                tabPanel("Breakdown by enhancements",
                         fixedRow(
                             plotOutput("amenityEffect")
                         ),
                         fixedRow(
                             column(6,
                                    plotOutput("avgAmenity")
                                    ),
                             column(6,
                                    plotOutput("amenityCompare")
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
      select(c(2:16)) %>%
      rename_at(vars(everything()), 
                ~c("totalHouses",
                   "totalTiny",
                   "tinyWC",
                   "tinyGarden",
                   "tinyEnergy",
                   "totalRow",
                   "rowWC",
                   "rowGarden",
                   "rowEnergy",
                   "totalFree",
                   "freeWC",
                   "freeGarden",
                   "freeEnergy",
                   "totalCost",
                   "totalPeople")) %>%
        rowwise() %>%
        dplyr::mutate(totalHouses=sum(totalTiny,totalRow,totalFree),
                      totalPeople=sum(totalTiny*1,totalRow*3,totalFree*5)) %>%
        ungroup() %>%
        dplyr::mutate(id=1:n()) %>%
        # pivot_longer(cols=c(starts_with("tiny"),starts_with("row"),starts_with("free")),
        #              names_to=c(".value","amenity"),
        #              names_pattern="(^[a-z]+)(.*)")
      
        pivot_longer(cols=c(starts_with("tiny"),starts_with("row"),starts_with("free")),
                     names_to="amenity",
                     values_to="number_amenity") %>%
        pivot_longer(cols=c("totalTiny","totalRow","totalFree"),
                     names_to="type",
                     values_to="number_house") %>%
        mutate(type=tolower(str_remove(type,"total"))) %>%
        rowwise() %>% 
        filter(grepl(type,amenity)) %>% 
        mutate(amenity=tolower(str_remove(amenity,"tiny|free|row"))) %>%
        group_by(id)
    
    # make plots
    output$peoplePlot <- renderPlot({ # DOUBLE CHECK THAT THIS OUTPUT MAKES SENSE BECAUSE OF LONG FORMAT
        data_long %>%
          distinct(totalHouses,totalCost,totalPeople,type,number_house) %>%
          mutate(peoplePerHouse=ifelse(type=="tiny",1*number_house,
                                    ifelse(type=="row",3*number_house,
                                           ifelse(type=="free",5*number_house,
                                                  NA)))) %>%
          group_by(type) %>%
          ggplot(aes(totalPeople, weights=peoplePerHouse)) +
            geom_histogram(binwidth=2,aes(fill=type)) +
            scale_x_continuous(breaks=pretty_breaks()) +
            xlab("Total number of people housed") +
            theme_bw() +
            labs(title="Number of people housed by house type")
    })
    
    output$housePlot <- renderPlot({ # DOUBLE CHECK THAT THIS OUTPUT MAKES SENSE BECAUSE OF LONG FORMAT
      data_long %>%
        distinct(totalHouses,totalCost,totalPeople,type,number_house) %>%
        group_by(type) %>%
        ggplot(aes(totalPeople, weights=number_house)) +
            geom_histogram(binwidth=2,aes(fill=type)) +
            scale_x_continuous(breaks=pretty_breaks()) +
            xlab("Total number of houses built") +
            theme_bw() +
            labs(title="Number of houses built by house type")
    })
    
    output$costPeople <- renderPlot({
        switch(input$houseGroup,
               "no"= ggplot(data_long, aes(x=totalCost, y=totalPeople)) +
                        geom_point() +
                        theme_bw() +
                        xlab("Total cost ($)") +
                        ylab("Total number of people housed") +
                        labs(title="Effect of total cost on number of people housed"),
                "yes"= data_long %>%
                        select(-c(totalHouses)) %>%
                        pivot_wider(names_from=c(amenity),
                                    values_from=number_amenity) %>%
                        mutate(count_amenity=rowSums(.[c("wc","garden","energy")]!=0)) %>%
                        ggplot() +
                        aes(x=totalCost, y=totalPeople) +
                        geom_point(aes(color=factor(count_amenity))) +
                        theme_bw() +
                        xlab("Total cost ($)") +
                        ylab("Total number of people housed") +
                        labs(title="Effect of total cost on number of people housed, by enhancement",
                             color="Number of enhancements\nprovided across all houses")
              )
    })
    
    output$avgAmenity <- renderPlot({
      data_summary(data_long,varname="number_house",groupnames=c("amenity","type")) %>% 
        ggplot() + aes(x=fct_rev(amenity), y=number_house, fill=fct_rev(type)) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymin=number_house-sd, ymax=number_house+sd),
                      width=.2,
                      position=position_dodge(0.9)) +
        theme_bw() +
        labs(title="Average number of houses built with each enhancement",
             fill="House type") +
        xlab("Enhancement") +
        ylab("Number of houses") +
        scale_fill_hue(labels=c("Tiny", "Row", "Free-standing")) +
        theme_bw()
    })

    output$amenityEffect <- renderPlot({
        data_long %>%
            select(-c(totalHouses, totalCost)) %>%
            pivot_wider(names_from=c(amenity),
                        values_from=number_amenity) %>%
            #pivot_wider(names_from=type,
             #           values_from=number_house) %>%
            group_by(id, totalPeople, type) %>%
            mutate(count_amenity=rowSums(.[c("wc","garden","energy")]!=0)) %>%
            mutate(count_amenity=as.factor(count_amenity)) %>%
            ungroup() %>%
            group_by(count_amenity) %>%
            dplyr::summarise(mean=mean(totalPeople),sd=sd(totalPeople)) %>%
            ggplot(aes(x=count_amenity, y=mean)) +
            geom_bar(stat="identity",aes(fill=count_amenity)) +
            geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                          width=.2) +
            theme_bw() +
            xlab("Number of enhancements used in houses") +
            ylab("Number of people housed") +
            labs(title="Effect of number of enhancements on number of people housed")
        # total people helped with and without amenities
    })
    output$amenityCompare <- renderPlot({
      data_long %>%
        group_by(type,amenity) %>%
        dplyr::summarise(sum=sum(number_amenity)) %>%
        ggplot() + aes(x=fct_rev(type),y=sum,fill=fct_rev(amenity)) +
        geom_bar(stat="identity") +
        theme_bw() +
        xlab("Type of house") +
        ylab("Number of houses") +
        labs(title="Total numbers of each type of house with each enhancement",
             fill="amenity") +
        scale_fill_hue(labels=c("wheelchair","garden","energy efficient"))
        # ggplot(data_long, aes(x=fct_rev(type), y=number_amenity)) +
        #     geom_boxplot(aes(fill=fct_rev(amenity))) +
        #     theme_bw() +
        #     ylab("Number of houses with amenity") +
        #     xlab("House type") +
        #     labs(title="Number of each type of house with each amenity",
        #          fill="amenity") +
        #     scale_fill_hue(labels=c("wheelchair","garden","energy efficient"))
        # most amenities by house type
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
