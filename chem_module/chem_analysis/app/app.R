#

library(shiny)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(scales)
library(forcats)


dataURL <- "https://docs.google.com/spreadsheets/d/1sbtcKETME3hliDCLKiAgcCr8qyPL6Cqib1gDI6R6KHM/edit?usp=sharing"

# setwd("/home/lauterbur/Desktop/AAUW_2022/chem_module/chem_data_analysis/")
# options(gargle_oauth_cache = ".secrets")
# gs4_auth()
# list.files(".secrets/")
# gs4_deauth()
gs4_auth(cache = ".secrets", email = "elise.lauterbur@gmail.com")

# Define UI for application
ui <- fixedPage(title="Data analysis for battery chemistry",
                tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                
                tabsetPanel(type = "tabs", header="Data analysis for for battery chemistry",
                            tabPanel("Effect of anode and cathode",
                                     
                                     # Application title
                                     titlePanel("Effect of anode and cathode element choice on voltage and electrode reaction"),
                                     br(),
                                     
                                     fixedRow(
                                         column(6,
                                             plotOutput("totalV")
                                         ),
                                         column(6,
                                                plotOutput("totalS"))
                                     ),
                                     
                                     fixedRow(
                                         plotOutput("reaction_v_V")
                                     )
                            )
                            
                            # tabPanel("Effect of genes",
                            #          titlePanel("Effect of genetic risk on developing diabetes"),
                            #          fixedRow(
                            #              column(6,
                            #                     plotOutput("exp2")
                            #              ),
                            #              column(6,
                            #                     plotOutput("exp3")
                            #              )
                            #          )
                            # )
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
        select(c(5,6,7,8,9,10,11,12,13,14)) %>%
        dplyr::mutate(id=1:n()) %>%
        rename_at(vars(everything()), 
                  ~c("anode", 
                     "Pt_voltage", "Ag_voltage","Cu_voltage",
                     "Pt_anodereact", "Ag_anodereact", "Cu_anodereact",
                     "Pt_cathodereact", "Ag_cathodereact", "Cu_cathodereact",
                     "id")) %>%
        dplyr::mutate(Pt_react=max(Pt_anodereact/46,Pt_cathodereact/25),
                      Ag_react=max(Ag_anodereact/46,Ag_cathodereact/25),
                      Cu_react=max(Cu_anodereact/46,Cu_cathodereact/25)) %>%
        pivot_longer(cols=-c(id, anode),
                     names_sep="_",
                     names_to=c("cathode","quality"),
                     values_to="value") %>%
        filter(quality=="voltage" | quality=="react") %>%
        group_by(id)
    
    # make plots
    output$totalV <- renderPlot({
        data_summary(data_long,varname="value",groupnames=c("anode","cathode","quality")) %>% 
            filter(quality=="voltage") %>%
            ggplot() + aes(x=anode, y=cathode, fill=value) +
            geom_tile() +
            geom_text(aes(label = round(value, 1))) +
            scale_fill_gradient(low = "white", high = "orange") +
            theme_bw() +
            labs(title="Voltage at each anode and\ncathode combination",
                 fill="Voltage") +
            scale_y_discrete(labels=c("Platinum (Pb)","Copper (Cu)", "Silver (Ag)")) +
            xlab("Anode") +
            ylab("Cathode") +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=18,face="bold"),
                  title=element_text(size=20,face="bold"),
                  legend.text=element_text(size=12))
    })
    
    output$totalS <- renderPlot({
        data_summary(data_long,varname="value",groupnames=c("anode","cathode","quality")) %>% 
            filter(quality=="react") %>%
            ggplot() + aes(x=anode, y=cathode, fill=value*100) +
            geom_tile() +
            geom_text(aes(label = paste(round(value*100, 1),"%",sep=""))) +
            scale_fill_gradient(low = "white", high = "cyan4") +
            theme_bw() +
            labs(title="Electrode reaction at each\nanode and cathode combination",
                 fill="Percent Reacted") +
            scale_y_discrete(labels=c("Platinum (Pb)","Copper (Cu)", "Silver (Ag)")) +
            xlab("Anode") +
            ylab("Cathode") +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=18,face="bold"),
                  title=element_text(size=20,face="bold"),
                  legend.text=element_text(size=12))
    })
    
    output$reaction_v_V <- renderPlot({
        data_summary(data_long,varname="value",groupnames=c("anode","cathode","quality")) %>% 
            pivot_wider(id_cols=c(anode,cathode),
                        names_from=quality,
                        values_from=c(value,sd)) %>%
            ggplot() + aes(x=round(value_react*100,1), y=value_voltage) +
            geom_point() +
            geom_errorbar(aes(ymin=value_voltage-sd_voltage, ymax=value_voltage+sd_voltage),
                          width=.1) +
            geom_errorbar(aes(xmin=round(value_react-sd_react,2), xmax=round(value_react+sd_react,2)),
                          width=.1) +
            theme_bw() +
            labs(title="Relationship between voltage and reaction") +
            xlab("Percent battery used") +
            ylab("Voltage") +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=18,face="bold"),
                  title=element_text(size=20,face="bold"),
                  legend.text=element_text(size=12))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)