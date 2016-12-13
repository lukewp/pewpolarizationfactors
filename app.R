

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

    output$hisp_plot <- renderPlot({
      hispdist <- reportsetup %>%
        group_by(predict, hisp) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = hisp)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Hispanic/Non-Hispanic',
          title = 'Hispanic/Non-Hispanic by Factor'
        )
      gg_prop1 %+%
        hispdist
    })
    
    output$race_plot <- renderPlot({
      racedist <- reportsetup %>%
        group_by(predict, race) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = race)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Race',
          title = 'Race by Factor'
        )
      gg_prop1 %+%
        racedist
    })

    output$marital_plot <- renderPlot({
      maritaldist <- reportsetup %>%
        group_by(predict, marital) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = marital)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Marital Status',
          title = 'Marital Status by Factor'
        )
      gg_prop1 %+%
        maritaldist
    })
    
    output$parent_plot <- renderPlot({
      parentdist <- reportsetup %>%
        group_by(predict, parent) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = parent)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Parent Status',
          title = 'Parent Status by Factor'
        )
      gg_prop1 %+%
        parentdist
    })
    
    output$citizen_plot <- renderPlot({
      citizendist <- reportsetup %>%
        group_by(predict, citizen) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = citizen)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Citizenship Status',
          title = 'Citizenship Status by Factor'
        )
      gg_prop1 %+%
        citizendist
    })
    
    output$relig_plot <- renderPlot({
      religdist <- reportsetup %>%
        group_by(predict, relig) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = relig)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Religion',
          title = 'Religion by Factor'
        )
      gg_prop1 %+%
        religdist
    })
    
    output$attend_plot <- renderPlot({
      attenddist <- reportsetup %>%
        group_by(predict, attend) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = attend)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Church Attendance',
          title = 'Church Attendance by Factor'
        )
      gg_prop1 %+%
        attenddist
    })
    
    output$income_plot <- renderPlot({
      incomedist <- reportsetup %>%
        group_by(predict, income) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = income)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Income',
          title = 'Income by Factor'
        )
      gg_prop1 %+%
        incomedist
    })
    
    output$reg_plot <- renderPlot({
      regdist <- reportsetup %>%
        group_by(predict, reg) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = reg)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Voter Registration',
          title = 'Voter Registration by Factor'
        )
      gg_prop1 %+%
        regdist
    })
    
    output$party_plot <- renderPlot({
      partydist <- reportsetup %>%
        group_by(predict, party) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = party)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Party',
          title = 'Party by Factor'
        )
      gg_prop1 %+%
        partydist
    })
    
    output$ideo_plot <- renderPlot({
      ideodist <- reportsetup %>%
        group_by(predict, ideo) %>%
        summarise(n = sum(weight) # weight variable) 
        ) %>%
        mutate(prop = prop.table(n))
      
      gg_prop1 <- ggplot(data = data.frame(),
                         aes(x = predict, y = prop, fill = ideo)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        scale_y_continuous(labels = percent) +
        labs(
          x = 'Factor',
          y = NULL,
          fill = 'Ideology',
          title = 'Ideology by Factor'
        )
      gg_prop1 %+%
        ideodist
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
