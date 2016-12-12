

## Script for Shiny dashboard to help understand data
# install.packages(c("shiny","shinydashboard","DT","ggplot2","dplyr","tidyr","ggthemes","scales"))

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)

reportsetup <- explanation %>%
  mutate(predict = as.factor(predict),
          state = as.factor(state),
          usr = as.factor(usr),
          density = as.ordered(density),
          sex = as.factor(sex),
          # age = as.ordered(age),
          age.r = as.ordered(round(age/10,0)),
          educ = as.factor(educ),
          hisp = as.factor(hisp),
          race = as.factor(racecmb),
          marital = as.factor(marital),
          parent = as.factor(parent),
          citizen = as.factor(citizen),
          relig = as.factor(relig),
          attend = as.ordered(attend),
          income = as.ordered(income),
          reg = as.factor(reg),
          party = as.factor(party),
          ideo = as.factor(ideo)
  )

ui <- dashboardPage(
  dashboardHeader(title = "Pew Polarization 2014"),
  dashboardSidebar(width = 120,
                   sidebarMenu(
                     menuItem(
                       "Histograms",
                       tabName = "histograms",
                       icon = icon("bar-chart"),
                       selected = TRUE
                     ),
                     menuItem("States", tabName = "map", icon = icon("globe"))
                   )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "histograms",
      # Boxes need to be put in a row (or column)
      fluidRow(box(
        width = 12, plotOutput("usr_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("density_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("sex_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("age_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("educ_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("hisp_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("race_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("marital_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("parent_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("citizen_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("relig_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("attend_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("income_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("reg_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("party_plot", height = 250)
      )),
      fluidRow(box(
        width = 12, plotOutput("ideo_plot", height = 250)
      ))
    ),
    tabItem(tabName = "map",
            h2("Factor Concentration by State"),
            fluidRow(box(
              width = 12, dataTableOutput("state_table")
            )))
  ))
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  ## Histograms
  output$usr_plot <- renderPlot({
    usrdist <- reportsetup %>%
      group_by(predict, usr) %>%
      summarise(n = sum(weight) # weight variable) 
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = usr)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      # scale_fill_manual(values = c("blue", "red")) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Urban/Suburban/Rural',
        title = 'Location Type by Factor'
      )
    gg_prop1 %+%
      usrdist
  })

    output$density_plot <- renderPlot({
    densitydist <- reportsetup %>%
      group_by(predict, density) %>%
      summarise(n = sum(weight) # weight variable) 
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = density)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      # scale_fill_manual(values = c("blue", "red")) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Density',
        title = 'Population Density (ZIP code) by Factor'
      )
    gg_prop1 %+%
      densitydist
  })
  
    output$sex_plot <- renderPlot({
    sexdist <- reportsetup %>%
      group_by(predict, sex) %>%
      summarise(n = sum(weight) # weight variable) 
                ) %>%
                mutate(prop = prop.table(n))
                
                gg_prop1 <- ggplot(data = data.frame(),
                                   aes(x = predict, y = prop, fill = sex)) +
                  geom_bar(stat = 'identity', position = 'dodge') +
                  geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                            position = position_dodge(width = 0.9),
                            vjust = -0.25) +
                  scale_y_continuous(labels = percent) +
                  scale_fill_manual(values = c("blue", "red")) +
                  labs(
                    x = 'Factor',
                    y = NULL,
                    fill = 'Sex',
                    title = 'Gender by Factor'
                  )
                gg_prop1 %+%
                  sexdist
  })
    
    output$age_plot <- renderPlot({
      agedist <- reportsetup %>%
        group_by(predict, age.r) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = age.r)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        # scale_fill_manual(values = c("blue", "red")) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Age/10 (rounded)',
          title = 'Age by Factor: 2=16-25; 3=26-35; ... 10=96-105'
        )
      gg_prop1 %+%
        agedist
    })

    output$educ_plot <- renderPlot({
      educdist <- reportsetup %>%
        group_by(predict, educ) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = educ)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        # scale_fill_manual(values = c("blue", "red")) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Education',
          title = 'Education by Factor'
        )
      gg_prop1 %+%
        educdist
    })

    statedist <- reportsetup[which(is.na(reportsetup$predict)==FALSE), ] %>%
      group_by(state, predict) %>%
      summarise(
        n=sum(weight) # weight variable
      ) %>%
      mutate(prop = prop.table(n))
    
    statedisttable <- spread(statedist[c("state","predict","prop")], key = predict, value = prop)
    
    output$state_table <- renderDataTable({
      datatable(statedisttable) %>%
        formatPercentage(names(statedisttable[c(2:4)]), 1) %>%
        formatStyle(names(statedisttable[c(2:4)]),
                    background = styleColorBar(range(statedisttable[c(2:4)], na.rm = TRUE), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    })
}

shinyApp(ui, server)
