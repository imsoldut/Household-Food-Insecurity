#Household Food Insecurity Dashboard
#Data 6200 Final Project
# Author : Charity Obeng Yeboaa

#Reference:  Shiny documentation: https://rstudio.github.io/shinydashboard/structure.html
#Loading Libraries
# shiny libraries
library(shiny)
library(shinydashboard)
#Reading dataset
library(readxl)
#libraries for data manipulation
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggpubr)
library(GGally)
#libraries for creating model
library(reshape)
library(maps)
library(plyr)
library(epiDisplay)
library(MASS)
library(caret)
library(e1071)
library(randomForest)
library(naivebayes)
library(psych)
library(Amelia)


#food <- read.csv("C:\\Users\\USER\\Desktop\\Food_Insecurity.csv")
#View(food)

### visualising dataset #######
#str(food)
#colSums(is.na(food))
#summary(food)


#### Preprocessing #####
#df = subset(food, select = -c(1:5)) # remove col with ids
#View(df)
#food <- df
#food_ins <- df
#View(food_ins)
#food_ins <- subset(food_ins, select = -c(11)) # remove col with id's
#View(food_ins)

#food_ins <- subset(food_ins, select = -c(11))
#food_ins <- subset(food_ins, select = -c(21))
#food_ins <- subset(food_ins, select = -c(19))



#### handling missing values ####
#food_ins[food_ins == ''] <- NA  # replace all empty cols with NAs
#View(food_ins)
#colSums(is.na(food_ins)) ## all 3 cols with nas are as a result of not applicable.


#### removing redundant cols ####
#food_ins <- subset(food_ins, select = -c(19:20)) # remove the migrants cols
#View(food_ins)
#colSums(is.na(food_ins))


#### cleaning of names and renaming values####
## marital status ##    
#food_ins[food_ins$marital_stat == "Informal/living together", "marital_stat"] <- "informal"
#food_ins[food_ins$marital_stat == "Married (Customary/Traditional)", "marital_stat"] <- "Married"
#food_ins[food_ins$marital_stat == "Married (Islamic)", "marital_stat"] <- "Married"
#food_ins[food_ins$marital_stat == "Married (Civil/Ordinance)", "marital_stat"] <- "Married"
#food_ins[food_ins$marital_stat == "Married (Other type)", "marital_stat"] <- "Married"

#unique(food_ins$marital_stat)

### renaming dwelling types ###
#food_ins[food_ins$dwell_type == "Separate house (Detached)", "dwell_type"] <- "Detached"
#food_ins[food_ins$dwell_type == "Compound house (rooms)", "dwell_type"] <- "CH"
#food_ins[food_ins$dwell_type == "Huts/Buildings (same compound)", "dwell_type"] <- "Huts"
#food_ins[food_ins$dwell_type == "Wooden structure", "dwell_type"] <- "wood_str"
#food_ins[food_ins$dwell_type == "Flat/Apartment", "dwell_type"] <- "flat"
#food_ins[food_ins$dwell_type == "Semi-detached house", "dwell_type"] <- "Semi-detached"
#food_ins[food_ins$dwell_type == "Uncompleted building", "dwell_type"] <- "uncompleted"
#food_ins[food_ins$dwell_type == "Living quarters attached to office/shop", "dwell_type"] <- "quarters"
#food_ins[food_ins$dwell_type == "Kiosk/poly kiosk", "dwell_type"] <- "Kiosk"
#food_ins[food_ins$dwell_type == "Metal Container", "dwell_type"] <- "met_container"
#food_ins[food_ins$dwell_type == "Other (Specify)", "dwell_type"] <- "other"


### renaming for religion ##
#unique(food_ins$religion)
#food_ins[food_ins$religion == "Protestant (Anglican, Lutheran, Presbyterian, Methodist etc)", "religion"] <- "Protestant"
#food_ins[food_ins$religion == "Pentecostal/Charismatic", "religion"] <- "Charismatic"

### renaming for etnicity
#food_ins[food_ins$ethnicity == "All Other Tribes (Hausa, Baribari, Zabrama)", "ethnicity"] <- "other"


## Replace NA in Edu_level with never attended since NA means never attended 
#colSums(is.na(food_ins))
#food_ins[is.na(food_ins)] <- "na" 


#### changing int to numeric ######
#food_ins <- food_ins %>% modify_at(c(11,14), as.numeric)
#str(food_ins)



##### Loading Dataset #############################################

#ddata2 <- read_excel(path="C:\\Users\\USER\\Desktop\\FoodInsecurity_data.xlsx")
ddata2 <- read_excel(path="FoodInsecurity_data.xlsx")
#View(ddata2)       

# filtered table with characteristics of severely insecure population
tableau6 <- filter(ddata2, foodinscat == "Severely Insecured")

#Use 80% of dataset as training set and remaining 20% as testing set
sample.1 <- sample(c(TRUE, FALSE), nrow(ddata2), replace=TRUE, prob=c(0.8,0.2))
train_3 <- ddata2[sample.1, ]
test_3 <- ddata2[!sample.1, ] 

model_naive3 <- naive_bayes(foodinscat ~ Age + sex + hh_size + marital_stat + religion + occupancy_stat + ethnicity + edu_level, data = train_3, usekernel = T) 


ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title = "Food Insecurity"),
  dashboardSidebar(
    sidebarMenu(
      id="menu",
      menuItem("About",tabName = "fff", icon=icon("info")),
      menuItem("Data Visualiztion",tabName="analysis", icon = icon("chart-line")),
      menuItem("Further Analysis",tabName = "plots", icon=icon("dove"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "fff",
              h1("Dashboard Overview"),
              p("Food security, which assesses the availability of enough, wholesome food, is a key indication of
socioeconomic progress. Lack of food security is frequently a sign of poverty and inequality, 
 and in such situations, it is less likely that food will be accessible, sufficient, and available to already disadvantaged groups. 
 
This dashboard provides an interface where a user can navigate and take quick actions to the various sections of the application for a detailed view. The dashboard and its entire 
  design may be useful to policy makers and intervention implementers. 
  The aim is to inform a user about the prevalence of household food insecurity among various groups of the population.
  The data visualization choice is in a manner that will inform a user on 
  disadvantaged populations, that is, groups that are less likely to have access to adequate and nutritious food.  
"),
              h2("Food Insecurity Indicators "),
              p("The survey incorporated the Food Insecurity Experience Scale (FIES), a measure of food insecurity based on experience that assessed the degree of food insecurity based on respondents' answers to inquiries regarding barriers to accessing enough food. Based on the FIES questions, two FIES-based indicators are mainly used for national monitoring purposes.
               The first indicator is an estimate of the sum of the fairly food insecure, moderately food insecure and the severely food insecure segments of the population, and the second indicator is the segment of the population. that are severely food insecure. The two indicators are denoted as follows:  "),
              
              tags$ul(
                tags$li('The proportion of the population experiencing fair, moderate or severe food insecurity.'),
                tags$li('The proportion of the population experiencing severe food insecurity. ')
              ),
              h2("Key Findings"),
              p("The data is a secondary data sourced from the Annual Household Income and Expenditure Survey, 2022, by the Ghana Statistical Service. Some key findings displayed by the dashboard are listed below:"),
              
              #tags$ul(
                #tags$li('The proportion of the population experiencing fair, moderate or severe food insecurity.'),
                #tags$li('The proportion of the population experiencing severe food insecurity. ')
              #),
              #p("There are 10761 households that took part of the interview. Among these, 4655(43.3%) are severely insecure, 3117(29.0%) are moderately insecure and 2989(27.8%) are fairly insecure.
#Household heads beyond age 60 are more vulnerable to food insecurity that any other age category. 43% of this group are severely insecure. This is followed by heads.
#•	Grouping food insecurity by marital status, the widowed group are more severely food insecure than any other marital status, followed by the divorced group.
#"),
              tags$ul(
                tags$li('There are 10761 households that took part of the interview. Among these, 4655(43.3%) are severely insecure, 3117(29.0%) are moderately insecure and 2989(27.8%) are fairly insecure'),
                tags$li('Household heads beyond age 60 are more vulnerable to food insecurity that any other age category. 43% of this group are severely insecure. This is followed by headsn between the ages of 15 and 35.'),
                tags$li('Grouping food insecurity by marital status, the widowed group are more severely food insecure than any other marital status, followed by the divorced group.')
              ),
              p("Charity, Obeng Yeboaa"),
              p("December 10, 2022")
              
      ),
#Tab 2: Exploratory #######
      tabItem(tabName = "analysis",
              fluidRow(
                infoBox("Fairly Insecured", value = 0.278, subtitle = NULL,
                        icon = shiny::icon("bar-chart"), color = "aqua",
                        href = NULL, fill = FALSE),
                
                
                infoBox("Moderately insecure",value = 0.29, subtitle = NULL,
                        icon = shiny::icon("bar-chart"), color = "aqua",
                        href = NULL, fill = FALSE),
                
                infoBox("Severely insecure",value = 0.43, subtitle = NULL ,
                        icon = shiny::icon("bar-chart"), color = "aqua",
                        href = NULL, fill = FALSE)),
              
              fluidRow(
                box(width=12,
                    selectInput("var_1","View Food Insecurity By:",
                                c('Age'='age',
                                  'Ethnicity'='eth',
                                  'Marital Status'='mar',
                                  'Sex'='sex',
                                  'Religion'='religion',
                                  'Occupancy Status'='occupancy',
                                  'Educational Level'='education',
                                  'Household Size'='hh size',
                                  'Dwelling Type'='dwelling',
                                  'Food Insecurity'=='prevalence')),
                    plotlyOutput("foodinscat")
                )
                
              ),
              fluidRow(
                box(
                  plotlyOutput("first_plot")),
                #box(plotOutput("foodinscat")),
                box(plotlyOutput("third_plot")),
                box(plotlyOutput("fourth_plot")),
                box(plotlyOutput("nineth_plot")),
                box(plotlyOutput("tenth_plot")),
                box(plotlyOutput("elenth_plot")),
                box(width= 12, plotlyOutput("twelth_plot")),
                box(width = 12, DT::dataTableOutput("mytable"))
                
              )
      ),
  #Tab 3: Naive Bayes Classification ######    
      tabItem(tabName = "plots",
              
              #Age + sex + hh_size + marital_stat + religion + occupancy_stat + ethnicity + edu_level
              sliderInput("age", label = "Age:",
                          min = 15, max = 120,
                          value = 40),
              sliderInput("hhsize", label = "HH_Size:",
                          min = 1, max = 28,
                          value = 5),
              fluidRow( 
                box(width = 6,
                    selectInput("edu", label = "Educational Level:",
                                choices = ddata2$edu_level%>%unique(),
                                selected = "na")),
                box(width=6,
                    selectInput("mar", label = "Marital Status:",
                                choices = ddata2$marital_stat%>%unique(),
                                selected = "informal"))
                
              ),
              
              fluidRow(
                box(width = 6,
                    selectInput("religion", label = "Religion:",
                                choices = ddata2$religion%>%unique(),
                                selected = "No religion")),
                box(width = 6,
                    selectInput("occ", label = "Occupation Status:",
                                choices = ddata2$occupancy_stat%>%unique(),
                                selected = "Owning"))
              ),
              fluidRow(
                box(width = 6,
                    selectInput("eth", label = "Ethnicity:",
                                choices = ddata2$ethnicity%>%unique(),
                                selected = "Akan")),
                box(width = 6,
                    selectInput("sex", label = "Sex:",
                                choices = ddata2$sex%>%unique(),
                                selected = "Female"))
              ),
              
              
              
              actionButton("submitbutton", "submit", class="btn btn-primary")
              #box(plotOutput("fifth_plot")),
              #box(plotOutput("sixth_plot")),
              #box(plotOutput("seventh_plot")),
              #box(plotOutput("eighth_plot"))
              
              ,
              verbatimTextOutput("predictoutput"),
              fluidRow(
                tags$label(h2('select inputs for prediction')),
                verbatimTextOutput('contents')
                #tableOutput('tabledata')
              )
      )      
    )
    
    
  )
  
)


##################Server Function###################

server <- function(input,output){
  
  modelpred <- eventReactive(input$submitbutton,{
    
    #Age + sex + hh_size + marital_stat + religion + occupancy_stat + ethnicity + edu_level
    age <- input$age
    sex <- input$sex
    hh_size <- input$hhsize
    mar <- input$mar
    religion <- input$religion
    occ <- input$occ
    eth <- input$eth
    edu <- input$edu
    
    
    data_mod <- tibble(Age=age,sex=sex,hh_size=hh_size,marital_stat=mar,religion=religion,occupancy_stat=occ,ethnicity=eth,edu_level=edu)
    pred <- predict(model_naive3,data_mod, type="prob")
    pred
  })
  
  output$predictoutput <- renderText({
    input$submitbutton
    isolate(modelpred())
  })
  
  
  output$foodinscat <- renderPlotly({
    if (input$var_1 == 'age'){
      ggplot(ddata2, aes(x=Agecat, fill = foodinscat)) +
        geom_bar(position = "fill") +
        coord_flip() +
        scale_fill_brewer() + 
        labs(y= "Percent", x = "Age") +
        labs(title="Food Insecurity by Age")
    }
    else if (input$var_1 == 'eth'){
      ggplot(ddata2, aes(x=ethnicity, fill = foodinscat)) +
        geom_bar(position = "fill") +
        coord_flip() +
        scale_fill_brewer() + 
        labs(y= "Percent", x = "Ethnicity") +
        labs(title="Food Insecurity by Ethnicity")
    }
    else if (input$var_1 == 'mar'){
      ggplot(ddata2, aes(x=marital_stat, fill = foodinscat)) +
        geom_bar(position = "fill") +
        coord_flip() +
        scale_fill_brewer() + 
        labs(y= "Percent", x = "Marital Status") +
        labs(title="Food Insecurity by Marital Status")
    }
    else if (input$var_1 == 'sex'){
      ggplot(ddata2, aes(x = foodinscat, fill = sex)) + 
        geom_bar(width = 0.5) +
        scale_fill_brewer() + 
        labs(y= "Percent", x = "Sex") +
        labs(title="Food Insecurity by Sex")
    }
    else if (input$var_1 == 'religion') {
      ggplot(ddata2, aes(x=religion, fill=foodinscat)) + 
        geom_bar(position = "fill") + 
        scale_fill_brewer() +
        coord_flip() + 
        labs(y= "Percent", x = "Religion") +
        labs(title="Food Insecurity by Religion of Household Head")
    }
    else if (input$var_1 == 'occupancy') {
      ggplot(ddata2, aes(x=occupancy_stat, fill=foodinscat)) + 
        geom_bar(position = "fill") + 
        scale_fill_brewer() +
        coord_flip() + 
        labs(y= "Percent", x = "Occupancy Status")+
        labs(title="Food Insecurity by Occupancy Status Head")
    }
    else if (input$var_1 == 'education') {
      ggplot(ddata2, aes(x=edu_level, fill=foodinscat)) +  
        geom_bar(position = "fill") +
        coord_flip() +
        scale_fill_brewer() +
        labs(y= "Percent", x = "Education") +
        labs(title="Food Insecurity by Educational Level")
    }
    else if (input$var_1 == 'hh size') {
      ggplot(ddata2, aes(x=hh_sizecat, fill=foodinscat)) + 
        geom_bar(position = "fill") + 
        scale_fill_brewer() +
        coord_flip() +
        labs(y= "Percent", x = "Household SIze") +
        labs(title="Food Insecurity by HH Size of")
    }
    else if (input$var_1 == 'dwelling') {
      ggplot(ddata2, aes(x=dwell_type, fill=foodinscat)) +  
        geom_bar(position = "fill") +
        coord_flip() +
        scale_fill_brewer() +
        labs(title="Food Insecurity by Dwelling Type")
    }
    # else if (input$var_1 == 'prevalence') {
    #ggplot(ddata2, aes(x=foodinscat)) + 
    #geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width = 0.5) +
    #coord_flip() + 
    #labs(title="Prvalence of Food Insecurity")
    #}
    
  })
  
  output$first_plot <- renderPlotly({
    ggplot(ddata2, aes(x=ddata2$foodinscat)) +
      geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width=0.5) +
      coord_flip() +
      labs(title="Prvalence of Food Insecurity") 
    
  })
  
  output$third_plot <- renderPlotly({
    ggplot(ddata2, aes(x=worried)) +
      geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width=0.5) +
      labs(title="HH size of heads that were worried") 
    
  })
  
  output$fourth_plot <- renderPlotly({
    ggplot(ddata2, aes(x=healthy_food)) +
      geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width = 0.5) +
      labs(x = "Health Food") +
      labs(title="HH that could not afford healthy food") 
    
  })
  
  output$nineth_plot <- renderPlotly({
    ggplot(ddata2, aes(x=few_kinds)) +
      geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width = 0.5) +
      labs(title="HH that ate only few kinds of food") 
    
  })
  
  output$tenth_plot <- renderPlotly({
    ggplot(ddata2, aes(x=without_eating)) +
      geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width = 0.5) +
      labs(title="HH that went a whole day without eating") 
    
  })
  
  output$elenth_plot <- renderPlotly({
    ggplot(ddata2, aes(x=Age, col=foodinscat))+
      geom_freqpoly(binwidth = 1)+
      ggtitle("Age Distribution by Food Insecurity")
    #theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$twelth_plot <- renderPlotly({
    ggplot(ddata2, aes(hh_size, col=foodinscat))+
      geom_freqpoly(binwidth = 1)+
      ggtitle("HH Food Insecurity By Household Size")
    
  })
  # Data set filtered by severely insecure population
  output$mytable = DT::renderDataTable({
    tableau6
  })
  
  
  output$contents <- renderPrint({
    if(input$submitbutton>0) {
      isolate("calculation complet.")
    } else {
      return("server is ready for calc.")
    }
  })
  
  #prediction results
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
}


#output$first_plot <- renderPlot({
# ggplot(ddata2, aes(x= foodinscat)) + 
#  geom_bar(fill=rgb(0.1,0.4,0.5,0.7), width = 0.5) +
# coord_flip() + 
#labs(title="Prvalence of Food Insecurity")

#})

#output$third_plot <- renderPlot({
# ggplot(ddata2, aes(x=edu_level, fill=foodinscat)) +  
#  geom_bar() +
# coord_flip() +
#scale_fill_brewer() +
#labs(title="Food Insecurity by Educational Level")

#})

#output$fourth_plot <- renderPlot({
# ggplot(ddata2, aes(x=marital_stat, fill=foodinscat)) + 
#  geom_bar() + 
# coord_flip() + 
#scale_fill_brewer() +
#labs(title="Food Insecurity by Marital Status Level")

#})

#output$fifth_plot <- renderPlot({
# ggplot(ddata2, aes(x = foodinscat, fill = sex)) + 
#  geom_bar() +
# scale_fill_brewer() + 
#labs(title="Food Insecurity by Sex")
#})

#output$sixth_plot <- renderPlot({
# ggplot(ddata2, aes(x=Agecat, fill=foodinscat)) + 
#  geom_bar() + 
# scale_fill_brewer() +
#labs(title="Food Insecurity by Age of Household Head")
#})

#output$seventh_plot <- renderPlot({
# ggplot(ddata2, aes(x=religion, fill=foodinscat)) + 
#  geom_bar() + 
# scale_fill_brewer() +
#coord_flip() + 
#labs(title="Food Insecurity by Religion of Household Head")

#})

#output$eighth_plot <- renderPlot({
# ggplot(ddata2, aes(x=hh_sizecat, fill=foodinscat)) + 
#  geom_bar() + 
# scale_fill_brewer() +
#coord_flip() + 
#labs(title="Food Insecurity by HH Size of")

#})

#}

shinyApp(ui = ui,server=server)

