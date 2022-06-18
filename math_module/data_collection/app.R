# math data collection

library(shiny)
library(DT)

mathNames <- list("mult", "div")
mathCodes <- list("&#215;", "&#247;") 
math <- setNames(mathNames, mathCodes)
budget <- 500000
budget2 <- 1500000

realHouseCost <- 24000
realPeopleHouse <- 4

realHouses <- floor(budget/realHouseCost)
realPeople <- floor(realHouses * realPeopleHouse)
  
wc_cost <- 10
g_cost <- 5
e_cost <- 8
th_size <- 64
rh_size <- 256
fh_size <- 1000
tiny_cost <- 5500
row_cost <- 25000
free_cost <- 75000
tiny_people <- 1
row_people <- 3
free_people <- 5

min_people <- floor((budget2*row_people)/((wc_cost+g_cost+e_cost)*rh_size+row_cost))

# Define UI for application that draws a histogram
ui <- fixedPage(
      tabsetPanel(type = "tabs",
                
                # Step 1 ------------------------------------------------------------------
                tabPanel("Step 1",
                         # Tab title
                         titlePanel("Renovating Houses for People in Your Community"),
                         fixedRow(
                           htmlOutput("title1"),
                           br(),
                           br(),
                           br()
                         ),
                         fixedRow(
                             column(1,
                                    br(),
                                    div(style= "display: inline;",
                                        textInput(inputId="budgetVal",
                                                  label="Budget: ", value=0,
                                                  placeholder=NULL)),
                                    br(),
                                    textOutput("budget_unit_status")
                             ),
                             
                             column(2,
                                    br(),
                                    div(style= "display: inline;",
                                        selectInput("budgetUnits","Units:",
                                                    c("None"="none",
                                                      "Dollars"="dollars",
                                                      "Houses"="houses",
                                                      "People"="people"),
                                                    width="120px"))
                             ),
                             
                             column(1,
                                    br(),
                                    div(style= "display: inline;",
                                        selectizeInput(inputId="conv1",label="Operator:",
                                                       choices=NULL,
                                                       width="60px"
                                        )
                                    )
                             ),
                             
                             column(1,
                                    div(style= "display: inline;",
                                        textInput(inputId="housecostVal",
                                                  label="House cost: ", value=0,
                                                  placeholder=NULL)),
                                    br(),
                                    textOutput("housecost_unit_status")
                             ),
                             
                             column(2,
                                    br(),
                                    div(style= "display: inline;",
                                        selectInput("housecostUnits","Units:",
                                                    c("None"="none",
                                                      "Dollars/House"="dollars/house",
                                                      "Houses/Dollar"="houses/dollar",
                                                      "Houses/Person"="houses/people",
                                                      "People/House"="people/house"),
                                                    width="140px"))
                             ),
                             
                             column(1,
                                    br(),
                                    div(style= "display: inline;",
                                        selectizeInput(inputId="conv2",label="Operator:",
                                                       choices=NULL,
                                                       width="60px"
                                        )
                                    )
                             ),
                             
                             column(2,
                                    div(style= "display: inline;",
                                        textInput(inputId="peoplehouseVal",
                                                  label="People who can live in each house:", value=0,
                                                  placeholder=NULL)),
                                    br(),
                                    textOutput("peoplehouse_unit_status")
                             ),
                             
                             column(2,
                                    br(),
                                    div(style= "display: inline;",
                                        selectInput("peoplehouseUnits","Units:",
                                                    c("None"="none",
                                                      "Dollars/House"="dollars/house",
                                                      "Houses/Dollar"="houses/dollar",
                                                      "Houses/Person"="houses/people",
                                                      "People/House"="people/house"),
                                                    width="140px"))
                             ),
                         ),
                         
                         fixedRow(
                             br(),
                             br(),
                             htmlOutput("follow"),
                             br(),
                             div(style="font-size: 2em",
                               uiOutput("formula1"),
                               uiOutput("formula2")
                             )
                         ),
                         
                         fixedRow(
                           column(10, align="center",
                                  br(),
                                  br(),
                                  dataTableOutput("table",
                                                  width=4)
                           )
                         ),

                         fixedRow(
                             br(),
                             htmlOutput("helped")
                         )
                ),
                
                
                # Step 2 ------------------------------------------------------------------
                tabPanel("Step 2",
                          # Tab title
                         titlePanel("Building Houses for People in Your Community"),
                         fixedRow(
                           htmlOutput("title2"),
                           br(),
                           br(),
                           br()
                         ),
                         sidebarLayout(
                             sidebarPanel(width = 12,
                                          fixedRow(
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         textInput(inputId="tinyVal",
                                                                   label="Number of tiny houses:", value=0,
                                                                   placeholder=NULL,
                                                                   width="120px"))
                                              ),
                                              
                                              column(3, 
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("tiny_house_size"),
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="wheelchairTinyVal",
                                                         #           label="Number wheelchair accessible:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider1"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("wheelchair_cost1")
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="gardenTinyVal",
                                                         #           label="Number with garden:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider2"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("garden_cost1")
                                              ),
                                              
                                              column(1,
                                                     div(style= "display: inline;",
                                                     #     textInput(inputId="energyTinyVal",
                                                     #               label="Number optimized for energy efficiency:", value=0,
                                                     #               placeholder=NULL,
                                                     #               width="120px"))
                                                     uiOutput("slider3"))
                                                ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("energy_cost1")
                                              )
                                          ),
                                          
                                          fixedRow(
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         textInput(inputId="rowVal",
                                                                   label="Number of row houses:", value=0,
                                                                   placeholder=NULL,
                                                                   width="120px"))
                                              ),
                                              
                                              column(3,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("row_house_size")
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="wheelchairRowVal",
                                                         #           label="Number wheelchair accessible:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider4"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("wheelchair_cost2")
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                     #     textInput(inputId="gardenRowVal",
                                                     #               label="Number with garden:", value=0,
                                                     #               placeholder=NULL,
                                                     #               width="120px"))
                                                     uiOutput("slider5"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("garden_cost2")
                                              ),
                                              
                                              column(1,
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="energyRowVal",
                                                         #           label="Number optimized for energy efficiency:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider6"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("energy_cost2")
                                              )
                                              
                                          ),
                                          
                                          fixedRow(
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         textInput(inputId="freeVal",
                                                                   label="Number of free standing houses:", value=0,
                                                                   placeholder=NULL,
                                                                   width="120px"))
                                              ),
                                              
                                              column(3,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("free_house_size")
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="wheelchairFreeVal",
                                                         #           label="Number wheelchair accessible:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider7"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("wheelchair_cost3")
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="gardenFreeVal",
                                                         #           label="Number with garden:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider8"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("garden_cost3")
                                              ),
                                              
                                              column(1,
                                                     div(style= "display: inline;",
                                                         # textInput(inputId="energyFreeVal",
                                                         #           label="Number optimized for energy efficiency:", value=0,
                                                         #           placeholder=NULL,
                                                         #           width="120px"))
                                                         uiOutput("slider9"))
                                              ),
                                              
                                              column(1,
                                                     br(),
                                                     br(),
                                                     br(),
                                                     uiOutput("energy_cost3")
                                              )
                                              
                                          )
                             ),
                             mainPanel(
                                 dataTableOutput("table2",
                                                 width = 2),
                                 br(),
                                 htmlOutput("helped2"),
                                 br(),
                                 br()
                             )
                         )
                )
    )
)


# Define server logic ---------------------------------------------------
server <- function(input, output, session) {
    options(scipen = 999) 
    
    output$title1 <- renderText({paste("<h4><center><b>Your organization has been offered a $",formatC(as.numeric(budget), big.mark=",", format="f", digits=0)," grant to renovate houses for people from people in need in your community.<br><br>
                                       Each house costs $",formatC(as.numeric(realHouseCost), big.mark=",", format="f", digits=0),
                                       " to renovate and will give ",realPeopleHouse," people a home of their own.<br><br>
                                       Use unit conversion to find out:<br><br>
                                       What is the maximum number of people can you help?</b></center></h4>", sep="")}) 
    output$budget_unit_status <- renderText({ifelse(input$budgetUnits == "dollars",
                                                    "Budget units are correct", "Try different budget units")})
    output$housecost_unit_status <- renderText({ifelse(input$housecostUnits == "houses/dollar" & input$conv1 == "mult" | input$housecostUnits == "dollars/house" & input$conv1 == "div",
                                                       "House cost units are correct with operator", 
                                                       "Try different house cost units and/or operator")})
    output$peoplehouse_unit_status <- renderText({ifelse(input$peoplehouseUnits == "people/house" & input$conv2 == "mult" | input$peoplehouseUnits == "houses/people" & input$conv2 == "div",
                                                         "People per house units are correct with operator", 
                                                         "Try different people per house units and/or operator")})
    
    updateSelectizeInput(session,
                         "conv1",
                         choices = math,
                         options = list(render = I("
      {
        item:   function(item, escape) { return '<div>' + item.label + '</div>'; },
        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
      }
    "))
    )
    updateSelectizeInput(session,
                         "conv2",
                         choices = math,
                         options = list(render = I("
      {
        item:   function(item, escape) { return '<div>' + item.label + '</div>'; },
        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
      }
    "))
    )
    
    houses <- reactive({
        ifelse(input$budgetUnits == "dollars" & input$housecostUnits == "houses/dollar" & input$conv1 == "mult",
               floor(as.numeric(eval(parse(text=input$budgetVal))) * as.numeric(eval(parse(text=input$housecostVal)))),
               ifelse(input$budgetUnits == "dollars" & input$housecostUnits == "dollars/house" & input$conv1 == "div",
                      floor(as.numeric(eval(parse(text=input$budgetVal))) / as.numeric(eval(parse(text=input$housecostVal)))),
                      "check conversion")
        )
    })
    
    people <- reactive({
        ifelse(input$budgetUnits == "dollars" & input$housecostUnits == "houses/dollar" & input$conv1 == "mult" & input$peoplehouseUnits == "people/house" & input$conv2 == "mult",
               floor(floor(as.numeric(eval(parse(text=input$budgetVal))) * as.numeric(eval(parse(text=input$housecostVal)))) * as.numeric(eval(parse(text=input$peoplehouseVal)))),
               ifelse(input$budgetUnits == "dollars" & input$housecostUnits == "dollars/house" & input$conv1 == "div" & input$peoplehouseUnits == "people/house" & input$conv2 == "mult",
                      floor(floor(as.numeric(eval(parse(text=input$budgetVal))) / as.numeric(eval(parse(text=input$housecostVal)))) * as.numeric(eval(parse(text=input$peoplehouseVal)))),
                      ifelse(input$budgetUnits == "dollars" & input$housecostUnits == "houses/dollar" & input$conv1 == "mult" & input$peoplehouseUnits == "houses/people" & input$conv2 == "div",
                             floor(floor(as.numeric(eval(parse(text=input$budgetVal))) * as.numeric(eval(parse(text=input$housecostVal)))) / as.numeric(eval(parse(text=input$peoplehouseVal)))),
                             ifelse(input$budgetUnits == "dollars" & input$housecostUnits == "houses/dollar" & input$conv1 == "div" & input$peoplehouseUnits == "houses/people" & input$conv2 == "div",
                                    floor(floor(as.numeric(eval(parse(text=input$budgetVal))) / as.numeric(eval(parse(text=input$housecostVal)))) / as.numeric(eval(parse(text=input$peoplehouseVal)))),
                                    "check conversion")
                      )
               )
        )
        
    })
    
    output$table <- DT::renderDataTable({
        
        # display top 2 rows at a time
        options(DT.options = list(searching=FALSE,
                                  lengthChange=FALSE,
                                  dom = "t"))
        
        data <- data.frame(houses = houses(),
                           people = people())
        
        # set conditions and return the beautiful table
        return(datatable(data, rownames = FALSE) %>% 
                   formatStyle('houses', backgroundColor = styleEqual(c(realHouses),"limegreen",default="grey")) %>%
                   formatStyle('people', backgroundColor = styleEqual(c(realPeople),"limegreen",default="grey")))
        }) 
    
    output$helped <- renderText({
        ifelse(people() == realPeople & houses() == realHouses & input$budgetVal == budget, 
               paste("<h4><b><center>You have helped <h4 style='color:Green'>", people(), "</h4> people by renovating ", houses(), " houses with your $", 
                     formatC(as.numeric(input$budgetVal), big.mark=",", format="f", digits=0), " budget.</center></b>\n<b><center>Great job!</center></b></h4>", sep=""),
               ""
        )
    })
    
    output$formula1 <- renderUI({
        tophouse <- strsplit(input$housecostUnits,"/")[[1]][1]
        bottomhouse <- ifelse(is.na(strsplit(input$housecostUnits,"/")[[1]][2]), "none", strsplit(input$housecostUnits,"/")[[1]][2])
        
        toppeople <- strsplit(input$peoplehouseUnits,"/")[[1]][1]
        bottompeople <- ifelse(is.na(strsplit(input$peoplehouseUnits,"/")[[1]][2]), "none", strsplit(input$peoplehouseUnits,"/")[[1]][2])
        
        conv1 <- ifelse(input$conv1 == "mult", "\\times", "\\div")
        conv2 <- ifelse(input$conv2 == "mult", "\\times", "\\div")
        
        withMathJax(paste0("$$", input$budgetUnits, conv1, "\\frac{", tophouse, "}{", bottomhouse, "}", conv2, "\\frac{", toppeople, "}{", bottompeople, "}$$"))
    })
    
    output$formula2 <- renderUI({
      tophouse <- strsplit(input$housecostUnits,"/")[[1]][1]
      bottomhouse <- ifelse(is.na(strsplit(input$housecostUnits,"/")[[1]][2]), "none", strsplit(input$housecostUnits,"/")[[1]][2])
      
      toppeople <- strsplit(input$peoplehouseUnits,"/")[[1]][1]
      bottompeople <- ifelse(is.na(strsplit(input$peoplehouseUnits,"/")[[1]][2]), "none", strsplit(input$peoplehouseUnits,"/")[[1]][2])
      
      conv1 <- ifelse(input$conv1 == "mult", "\\times", "\\div")
      conv2 <- ifelse(input$conv2 == "mult", "\\times", "\\div")
     
      if(input$conv1 == "div")
             withMathJax(helpText("remember that division is like taking a reciprocal:"),
               paste0("$$", input$budgetUnits, conv1, "\\frac{", tophouse, "}{", bottomhouse, "} = ", input$budgetUnits, "\\times \\frac{1}{ \\frac{", tophouse, "}{", bottomhouse, "}} = ", input$budgetUnits, "\\times \\frac{", bottomhouse, "}{", tophouse, "}$$"))#,
      })
    
    output$follow <-renderText({"<center><h5><b>Follow the units:</b></h5></center>"})
    
    # step 2
    
    output$title2 <- renderText({paste("<h4><center><b>Your organization did so well renovating houses that you have been offered a $",
                                       formatC(as.numeric(budget2), big.mark=",", format="f", digits=0),
                                       " grant to build new houses for people in need in your community.<br><br>
                                       This time, you can choose among three different types of houses ('tiny': minimalist but cozy; 
                                       'row': larger but share walls with other houses;
                                       and 'free-standing': largest and most private).<br><br>
                                       You may also choose to include three types of enhancements:<br> * make the house wheelchair accessible, including ramps, wide doorways, and low countertops;<br>
                                       * give the house a small garden, which provides a place to grow food and flowers and may attract birds and butterflies;<br>
                                       * energy efficiency, which will lower the long-term electricity costs.<br>
                                       You may add one, two, or all three to any house.<br><br>
                                       The base prices of the houses and the number of people who can live in each vary, and the costs of the enhancements depend on the size of the house.<br>
                                       These are shown below.<br><br>
                                       Use your unit conversion skills to calculate how much each set of houses you build will cost.<br><br>
                                       Enter your proposed values and see the results in the spreadsheet below.<br><br></h4>
                                       <h3><center>Your goal is to help the most people in the best way possible.<br>
                                       How you determine what is best is up to you.</b></center></h3>", sep="")})
    
    output$slider1 <- renderUI({
        sliderInput(inputId = "wheelchairTinyVal", "Number wheelchair accessible:",
                    min = 0, max = as.numeric(input$tinyVal), value = 0,
                    step = 1)
    })
    
    output$slider2 <- renderUI({
        sliderInput(inputId = "gardenTinyVal", "Number with garden:",
                    min = 0, max = as.numeric(input$tinyVal), value = 0,
                    step = 1)
    })
    
    output$slider3 <- renderUI({
        sliderInput(inputId = "energyTinyVal", "Number optimized for energy efficiency:",
                    min = 0, max = as.numeric(input$tinyVal), value = 0,
                    step = 1)
    })
    output$slider4 <- renderUI({
        sliderInput(inputId = "wheelchairRowVal", "Number wheelchair accessible:",
                    min = 0, max = as.numeric(input$rowVal), value = 0,
                    step = 1)
    })
    
    output$slider5 <- renderUI({
        sliderInput(inputId = "gardenRowVal", "Number with garden:",
                    min = 0, max = as.numeric(input$rowVal), value = 0,
                    step = 1)
    })
    
    output$slider6 <- renderUI({
        sliderInput(inputId = "energyRowVal", "Number optimized for energy efficiency:",
                    min = 0, max = as.numeric(input$rowVal), value = 0,
                    step = 1)
    })
    
    output$slider7 <- renderUI({
        sliderInput(inputId = "wheelchairFreeVal", "Number wheelchair accessible:",
                    min = 0, max = as.numeric(input$freeVal), value = 0,
                    step = 1)
    })
    
    output$slider8 <- renderUI({
        sliderInput(inputId = "gardenFreeVal", "Number with garden:",
                    min = 0, max = as.numeric(input$freeVal), value = 0,
                    step = 1)
    })
    
    output$slider9 <- renderUI({
        sliderInput(inputId = "energyFreeVal", "Number optimized for energy efficiency:",
                    min = 0, max = as.numeric(input$freeVal), value = 0,
                    step = 1)
    })
    
    output$table2 <- DT::renderDataTable({
        
        # display top 2 rows at a time
        options(DT.options = list(searching=FALSE,
                                  lengthChange=FALSE,
                                  dom = "t"))
        
        data <- data.frame(houses = c(input$tinyVal, input$rowVal, input$freeVal, 
                                      as.numeric(input$tinyVal) + as.numeric(input$rowVal) + as.numeric(input$freeVal)),
                           house_cost = c(as.numeric(input$tinyVal) * tiny_cost, 
                                          as.numeric(input$rowVal) * row_cost, 
                                          as.numeric(input$freeVal) * free_cost,
                                          as.numeric(input$tinyVal) * tiny_cost + as.numeric(input$rowVal) * row_cost + as.numeric(input$freeVal) * free_cost),
                           wheelchair = c(input$wheelchairTinyVal, input$wheelchairRowVal, input$wheelchairFreeVal, as.numeric(input$wheelchairTinyVal) + as.numeric(input$wheelchairRowVal) + as.numeric(input$wheelchairFreeVal)),
                           wheelchair_cost = c(as.numeric(input$wheelchairTinyVal) * th_size * wc_cost, 
                                               as.numeric(input$wheelchairRowVal) * rh_size * wc_cost, 
                                               as.numeric(input$wheelchairFreeVal) * fh_size * wc_cost,
                                                as.numeric(input$wheelchairTinyVal) * th_size * wc_cost + 
                                                 as.numeric(input$wheelchairRowVal) * rh_size * wc_cost + 
                                                 as.numeric(input$wheelchairFreeVal) * fh_size * wc_cost),
                           garden = c(input$gardenTinyVal, input$gardenRowVal, input$gardenFreeVal, as.numeric(input$gardenFreeVal) + as.numeric(input$gardenRowVal) + as.numeric(input$gardenFreeVal)),
                           garden_cost = c(as.numeric(input$gardenTinyVal) * th_size * g_cost, 
                                           as.numeric(input$gardenRowVal) * rh_size * g_cost, 
                                           as.numeric(input$gardenFreeVal) * fh_size * g_cost,
                                            as.numeric(input$gardenTinyVal) * th_size * g_cost + 
                                             as.numeric(input$gardenRowVal) * rh_size * g_cost + 
                                             as.numeric(input$gardenFreeVal) * fh_size * g_cost),
                           energy = c(input$energyTinyVal, input$energyRowVal, input$energyFreeVal, as.numeric(input$energyTinyVal) + as.numeric(input$energyRowVal) + as.numeric(input$energyFreeVal)),
                           energy_cost = c(as.numeric(input$energyTinyVal) * th_size * e_cost, 
                                           as.numeric(input$energyRowVal) * rh_size * e_cost, 
                                           as.numeric(input$energyFreeVal) * fh_size * e_cost,
                                           as.numeric(input$energyTinyVal) * th_size * e_cost + 
                                             as.numeric(input$energyRowVal) * rh_size * e_cost + 
                                             as.numeric(input$energyFreeVal) * fh_size * e_cost),
                          total_cost = c(as.numeric(input$tinyVal) * tiny_cost + as.numeric(input$wheelchairTinyVal) * th_size * wc_cost + as.numeric(input$gardenTinyVal) * th_size * g_cost + as.numeric(input$energyTinyVal) * th_size * e_cost,
                                           as.numeric(input$rowVal) * row_cost + as.numeric(input$wheelchairRowVal) * rh_size * wc_cost + as.numeric(input$gardenRowVal) * rh_size * g_cost + as.numeric(input$energyRowVal) * rh_size * e_cost,
                                           as.numeric(input$freeVal) * free_cost + as.numeric(input$wheelchairFreeVal) * fh_size * wc_cost + as.numeric(input$gardenFreeVal) * fh_size * g_cost + as.numeric(input$energyFreeVal) * fh_size * e_cost,
                                           as.numeric(input$tinyVal) * tiny_cost + as.numeric(input$wheelchairTinyVal) * th_size * wc_cost + as.numeric(input$gardenTinyVal) * th_size * g_cost + as.numeric(input$energyTinyVal) * th_size * e_cost +
                                                      as.numeric(input$rowVal) * row_cost + as.numeric(input$wheelchairRowVal) * rh_size *  wc_cost + as.numeric(input$gardenRowVal) * rh_size * g_cost + as.numeric(input$energyRowVal) * rh_size * e_cost +
                                                      as.numeric(input$freeVal) * free_cost + as.numeric(input$wheelchairFreeVal) * fh_size * wc_cost + as.numeric(input$gardenFreeVal) * fh_size * g_cost + as.numeric(input$energyFreeVal) * fh_size * e_cost),
                           total_people = c(as.numeric(input$tinyVal) * tiny_people, as.numeric(input$rowVal) * row_people, as.numeric(input$freeVal) * free_people,
                                            as.numeric(input$tinyVal) * tiny_people + as.numeric(input$rowVal) * row_people + as.numeric(input$freeVal) * free_people)
        )
        
        # set conditions and return the beautiful table
        return(datatable(data, 
                         colnames = c("# houses", "cost of houses","# wheelchair accessible", "cost of wheelchair accessibility", 
                                      "# with garden", "cost of garden", "# energy efficient", "cost of energy efficiency",
                                      "total cost","total people"),
                         rownames = c("tiny houses", "row houses", "free-standing houses", "total")) %>% 
                   formatStyle(c('total_cost', 'total_people'), backgroundColor = "lightgrey") %>%
                   formatStyle(c(1:11), backgroundColor = styleRow(4, "lightgrey")) %>%
                   formatStyle(c('total_cost', 'total_people'), backgroundColor = styleRow(4, "lightblue")) %>%
                   formatCurrency(c(2,4,6,8,9), currency = "$", interval = 3, mark = ",")

        )
    })
    
    output$tiny_house_size <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(tiny_cost), big.mark=",", format="d", digits=0), "}{house}, \\frac{", tiny_people, "  person}{house}, \\frac{", th_size, "ft^2}{house}$$"))})
    output$row_house_size <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(row_cost), big.mark=",", format="f", digits=0), "}{house}, \\frac{", row_people, "  people}{house}, \\frac{", rh_size, "ft^2}{house}$$"))})
    output$free_house_size <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(free_cost), big.mark=",", format="f", digits=0), "}{house}, \\frac{", free_people, "  people}{house}, \\frac{", fh_size, "ft^2}{house}$$"))})
    output$wheelchair_cost1 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(wc_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$garden_cost1 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(g_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$energy_cost1 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(e_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$wheelchair_cost2 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(wc_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$garden_cost2 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(g_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$energy_cost2 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(e_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$wheelchair_cost3 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(wc_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$garden_cost3 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(g_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    output$energy_cost3 <- renderUI({withMathJax(paste0("$$", "\\times \\frac{$", formatC(as.numeric(e_cost), big.mark=",", format="f", digits=0), "}{ft^2}$$"))})
    
    totalcost <- reactive({
        as.numeric(input$tinyVal) * tiny_cost + as.numeric(input$wheelchairTinyVal) * wc_cost + as.numeric(input$gardenTinyVal) * g_cost + as.numeric(input$energyTinyVal) * e_cost +
            as.numeric(input$rowVal) * row_cost + as.numeric(input$wheelchairRowVal) * wc_cost + as.numeric(input$gardenRowVal) * g_cost + as.numeric(input$energyRowVal) * e_cost +
            as.numeric(input$freeVal) * free_cost + as.numeric(input$wheelchairFreeVal) * wc_cost + as.numeric(input$gardenFreeVal) * g_cost + as.numeric(input$energyFreeVal) * e_cost
    })
    
    totalpeople <- reactive({
        as.numeric(input$tinyVal) * tiny_people + as.numeric(input$rowVal) * row_people + as.numeric(input$freeVal) * free_people
    })
    
    totalhouses <- reactive({
        as.numeric(input$tinyVal) + as.numeric(input$rowVal) + as.numeric(input$freeVal)
    })
    
    output$helped2 <- renderText({
        ifelse(totalcost() <= budget2 & totalpeople() >= min_people, 
               paste("<h3><b><center>You have helped <h3 style='color:Green'>", totalpeople(), "</h3> people by building ", totalhouses(), " houses for $", 
                     totalcost(), "!</center></b>\n<b><center>Great job!</center></b>", sep=""),
               ifelse(totalcost() > budget2,
                      paste("<h4><center>You are over your $", formatC(as.numeric(budget2), big.mark=",", format="f", digits=0), 
                            " budget at $", formatC(as.numeric(totalcost()), big.mark=",", format="f", digits=0),
                            ". Please revise your choices.</center></h4>", sep=""),
                      "")
                )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
