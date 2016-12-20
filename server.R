#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)

## explanation object created by shinySetup.R
reportsetup <- explanation %>%
  mutate(predict = as.factor(predict),
         state = as.factor(state),
         usr = as.factor(usr),
         density = as.ordered(density),
         sex = as.factor(sex),
         # age = as.ordered(age),
         age.r = as.ordered(round(age/10,0)), # Splitting into buckets by 10
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
         ideo = as.factor(ideo),
         q26f1.r = as.ordered(round(q26f1/10,0)), # Splitting into buckets by 10
         q26f2.r = as.ordered(round(q26f2/10,0)) # Splitting into buckets by 10
  )


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Histograms
  withProgress(message = 'All demographics plots ...', value = 0, {

    output$usr_plot <- renderPlot({
      withProgress(message = 'Location Type plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }  
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          usrdist
      })
    })
    incProgress(1/16)
    
    output$density_plot <- renderPlot({
      withProgress(message = 'density plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }  
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          densitydist
      })
    })
    incProgress(1/16)
    
    output$sex_plot <- renderPlot({
      withProgress(message = 'Sex plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }  
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          sexdist
      })
    })
    incProgress(1/16)
    
    output$age_plot <- renderPlot({
      withProgress(message = 'Age plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          agedist
      })
    })
    incProgress(1/16)
    
    output$educ_plot <- renderPlot({
      withProgress(message = 'Education plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          educdist
      })
    })
    incProgress(1/16)
    
    output$hisp_plot <- renderPlot({
      withProgress(message = 'Hispanic plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          hispdist
      })
    })
    incProgress(1/16)
    
    output$race_plot <- renderPlot({
      withProgress(message = 'Race plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          racedist
      })
    })
    incProgress(1/16)
    
    output$marital_plot <- renderPlot({
      withProgress(message = 'Marital Status plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          maritaldist
      })
    })
    incProgress(1/16)
    
    output$parent_plot <- renderPlot({
      withProgress(message = 'Parent status plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        parentdist <- reportsetup[which(is.na(reportsetup$parent)==FALSE), ] %>%
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          parentdist
      })
    })
    incProgress(1/16)
    
    output$citizen_plot <- renderPlot({
      withProgress(message = 'Citizenship plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        citizendist <- reportsetup[which(is.na(reportsetup$citizen)==FALSE), ] %>%
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          citizendist
      })
    })
    incProgress(1/16)
    
    output$relig_plot <- renderPlot({
      withProgress(message = 'Religion plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          religdist
      })
    })
    incProgress(1/16)
    
    output$attend_plot <- renderPlot({
      withProgress(message = 'Church Attendance plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          attenddist
      })
    })
    incProgress(1/16)
    
    output$income_plot <- renderPlot({
      withProgress(message = 'Income plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          incomedist
      })
    })
    incProgress(1/16)
    
    output$reg_plot <- renderPlot({
      withProgress(message = 'Voter Registration plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          regdist
      })
    })
    incProgress(1/16)
    
    output$party_plot <- renderPlot({
      withProgress(message = 'Political Party plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          partydist
      })
    })
    incProgress(1/16)
    
    output$ideo_plot <- renderPlot({
      withProgress(message = 'Political Ideology plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
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
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          ideodist
      })
    })
    incProgress(1/16)
  })
  
  statedist <- reportsetup[which(is.na(reportsetup$predict)==FALSE), ] %>%
    group_by(state, predict) %>%
    summarise(
      n=sum(weight) # weight variable
    ) %>%
    mutate(prop = prop.table(n))
  
  statedisttable <- spread(statedist[c("state","predict","prop")], key = predict, value = prop)
  
  output$state_table <- renderDataTable({
    datatable(statedisttable, options = list(pageLength = 100)) %>%
      formatPercentage(names(statedisttable[c(2:4)]), 1) %>%
      formatStyle(names(statedisttable[c(2:4)]),
                  background = styleColorBar(range(statedisttable[c(2:4)], na.rm = TRUE), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  ## Tab survey1:
  withProgress(message = 'All survey1 plots ...', value = 0, {
    
    output$qa1_plot <- renderPlot({
      withProgress(message = 'Q.A1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa1dist <- reportsetup[which(is.na(reportsetup$qa1)==FALSE), ] %>%
          group_by(predict, qa1) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A1',
            title = 'Q.A1 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa1dist
      })
    })
    incProgress(1/51)
    
    output$qc1_plot <- renderPlot({
      withProgress(message = 'Q.C1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qc1dist <- reportsetup[which(is.na(reportsetup$qc1)==FALSE), ] %>%
          group_by(predict, qc1) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C1',
            title = 'Q.C1 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc1dist
      })
    })
    incProgress(1/51)
    
    output$qc1a_plot <- renderPlot({
      withProgress(message = 'Q.C1a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qc1adist <- reportsetup[which(is.na(reportsetup$qc1a)==FALSE), ] %>%
          group_by(predict, qc1a) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc1a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C1a',
            title = 'Q.C1a by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc1adist
      })
    })
    incProgress(1/51)
    
    output$qb2_plot <- renderPlot({
      withProgress(message = 'Q.B2 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb2dist <- reportsetup[which(is.na(reportsetup$qb2)==FALSE), ] %>%
          group_by(predict, qb2) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb2)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B2',
            title = 'Q.B2 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb2dist
      })
    })
    incProgress(1/51)
    
    output$qb3_plot <- renderPlot({
      withProgress(message = 'Q.B3 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb3dist <- reportsetup[which(is.na(reportsetup$qb3)==FALSE), ] %>%
          group_by(predict, qb3) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb3)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B3',
            title = 'Q.B3 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb3dist
      })
    })
    incProgress(1/51)
    
    output$qb4_plot <- renderPlot({
      withProgress(message = 'Q.B4 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb4dist <- reportsetup[which(is.na(reportsetup$qb4)==FALSE), ] %>%
          group_by(predict, qb4) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb4)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B4',
            title = 'Q.B4 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb4dist
      })
    })
    incProgress(1/51)
    
    output$qb5_plot <- renderPlot({
      withProgress(message = 'Q.B5 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb5dist <- reportsetup[which(is.na(reportsetup$qb5)==FALSE), ] %>%
          group_by(predict, qb5) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb5)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B5',
            title = 'Q.B5 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb5dist
      })
    })
    incProgress(1/51)
    
    output$qa6_plot <- renderPlot({
      withProgress(message = 'Q.A6 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa6dist <- reportsetup[which(is.na(reportsetup$qa6)==FALSE), ] %>%
          group_by(predict, qa6) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa6)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A6',
            title = 'Q.A6 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa6dist
      })
    })
    incProgress(1/51)
    
    output$qa8_plot <- renderPlot({
      withProgress(message = 'Q.A8 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa8dist <- reportsetup[which(is.na(reportsetup$qa8)==FALSE), ] %>%
          group_by(predict, qa8) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa8)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A8',
            title = 'Q.A8 by Factor'
          ) + 
          theme(legend.position="bottom", legend.direction = "vertical")
        gg_prop1 %+%
          qa8dist
      })
    })
    incProgress(1/51)
    
    output$qa9a_plot <- renderPlot({
      withProgress(message = 'Q.A9a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9adist <- reportsetup[which(is.na(reportsetup$qa9a)==FALSE), ] %>%
          group_by(predict, qa9a) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9a',
            title = 'Q.A9a by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9adist
      })
    })
    incProgress(1/51)
    
    output$qa9b_plot <- renderPlot({
      withProgress(message = 'Q.A9b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9bdist <- reportsetup[which(is.na(reportsetup$qa9b)==FALSE), ] %>%
          group_by(predict, qa9b) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9b',
            title = 'Q.A9b by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9bdist
      })
    })
    incProgress(1/51)
    
    output$qa9c_plot <- renderPlot({
      withProgress(message = 'Q.A9c plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9cdist <- reportsetup[which(is.na(reportsetup$qa9c)==FALSE), ] %>%
          group_by(predict, qa9c) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9c)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9c',
            title = 'Q.A9c by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9cdist
      })
    })
    incProgress(1/51)
    
    output$qa9d_plot <- renderPlot({
      withProgress(message = 'Q.A9d plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9ddist <- reportsetup[which(is.na(reportsetup$qa9d)==FALSE), ] %>%
          group_by(predict, qa9d) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9d)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9d',
            title = 'Q.A9d by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9ddist
      })
    })
    incProgress(1/51)
    
    output$qa9e_plot <- renderPlot({
      withProgress(message = 'Q.A9e plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9edist <- reportsetup[which(is.na(reportsetup$qa9e)==FALSE), ] %>%
          group_by(predict, qa9e) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9e)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9e',
            title = 'Q.A9e by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9edist
      })
    })
    incProgress(1/51)
    
    output$qa9f_plot <- renderPlot({
      withProgress(message = 'Q.A9f plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9fdist <- reportsetup[which(is.na(reportsetup$qa9f)==FALSE), ] %>%
          group_by(predict, qa9f) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9f)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9f',
            title = 'Q.A9f by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9fdist
      })
    })
    incProgress(1/51)
    
    output$qa9g_plot <- renderPlot({
      withProgress(message = 'Q.A9g plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qa9gdist <- reportsetup[which(is.na(reportsetup$qa9g)==FALSE), ] %>%
          group_by(predict, qa9g) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qa9g)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.A9g',
            title = 'Q.A9g by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qa9gdist
      })
    })
    incProgress(1/51)
    
    output$q11a_plot <- renderPlot({
      withProgress(message = 'Q.11a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11adist <- reportsetup %>%
          group_by(predict, q11a) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11a',
            title = 'Q.11a by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11adist
      })
    })
    incProgress(1/51)
    
    output$q11b_plot <- renderPlot({
      withProgress(message = 'Q.11b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11bdist <- reportsetup %>%
          group_by(predict, q11b) %>%
          summarise(n = sum(weight) # weight variable) 
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11b',
            title = 'Q.11b: The Democratic Party by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11bdist
      })
    })
    incProgress(1/51)
    
    output$q11c_b_plot <- renderPlot({
      withProgress(message = 'Q.11c.B plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11c_bdist <- reportsetup[which(is.na(reportsetup$q11c_b)==FALSE), ] %>%
          group_by(predict, q11c_b) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11c_b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11c.B',
            title = 'Q.11c.B by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11c_bdist
      })
    })
    incProgress(1/51)
    
    output$q11e_b_plot <- renderPlot({
      withProgress(message = 'Q.11e.B plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11e_bdist <- reportsetup[which(is.na(reportsetup$q11e_b)==FALSE), ] %>%
          group_by(predict, q11e_b) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11e_b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11e.B',
            title = 'Q.11e.B by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11e_bdist
      })
    })
    incProgress(1/51)
    
    output$q11h_b_plot <- renderPlot({
      withProgress(message = 'Q.11h.B plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11h_bdist <- reportsetup[which(is.na(reportsetup$q11h_b)==FALSE), ] %>%
          group_by(predict, q11h_b) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11h_b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11h.B',
            title = 'Q.11h.B by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11h_bdist
      })
    })
    incProgress(1/51)
    
    output$q11i_b_plot <- renderPlot({
      withProgress(message = 'Q.11i.B plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11i_bdist <- reportsetup[which(is.na(reportsetup$q11i_b)==FALSE), ] %>%
          group_by(predict, q11i_b) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11i_b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11i.B',
            title = 'Q.11i.B by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11i_bdist
      })
    })
    incProgress(1/51)
    
    output$q11j_b_plot <- renderPlot({
      withProgress(message = 'Q.11j.B plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11j_bdist <- reportsetup[which(is.na(reportsetup$q11j_b)==FALSE), ] %>%
          group_by(predict, q11j_b) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11j_b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11j.B',
            title = 'Q.11j.B by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11j_bdist
      })
    })
    incProgress(1/51)
    
    output$q11at_plot <- renderPlot({
      withProgress(message = 'Q.11at plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11atdist <- reportsetup[which(is.na(reportsetup$q11at)==FALSE), ] %>%
          group_by(predict, q11at) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11at)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11at',
            title = 'Q.11at by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11atdist
      })
    })
    incProgress(1/51)
    
    output$q11bt_plot <- renderPlot({
      withProgress(message = 'Q.11bt plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q11btdist <- reportsetup[which(is.na(reportsetup$q11bt)==FALSE), ] %>%
          group_by(predict, q11bt) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q11bt)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.11bt',
            title = 'Q.11bt by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q11btdist
      })
    })
    incProgress(1/51)
    
    output$qb12_plot <- renderPlot({
      withProgress(message = 'Q.B12 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb12dist <- reportsetup[which(is.na(reportsetup$qb12)==FALSE), ] %>%
          group_by(predict, qb12) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb12)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B12',
            title = 'Q.B12 by Factor'
          ) + 
          theme(legend.position="bottom", legend.direction = "vertical")
        gg_prop1 %+%
          qb12dist
      })
    })
    incProgress(1/51)
    
    output$q25a_plot <- renderPlot({
      withProgress(message = 'Q.25a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25adist <- reportsetup %>%
          group_by(predict, q25a) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25a',
            title = 'Q.25a by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25adist
      })
    })
    incProgress(1/51)
    
    output$q25b_plot <- renderPlot({
      withProgress(message = 'Q.25b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25bdist <- reportsetup %>%
          group_by(predict, q25b) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25b',
            title = 'Q.25b by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25bdist
      })
    })
    incProgress(1/51)
    
    output$q25c_plot <- renderPlot({
      withProgress(message = 'Q.25c plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25cdist <- reportsetup %>%
          group_by(predict, q25c) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25c)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25c',
            title = 'Q.25c by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25cdist
      })
    })
    incProgress(1/51)
    
    output$q25d_plot <- renderPlot({
      withProgress(message = 'Q.25d plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25ddist <- reportsetup %>%
          group_by(predict, q25d) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25d)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25d',
            title = 'Q.25d by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25ddist
      })
    })
    incProgress(1/51)
    
    output$q25f_plot <- renderPlot({
      withProgress(message = 'Q.25f plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25fdist <- reportsetup %>%
          group_by(predict, q25f) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25f)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25f',
            title = 'Q.25f by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25fdist
      })
    })
    incProgress(1/51)
    
    output$q25g_plot <- renderPlot({
      withProgress(message = 'Q.25g plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25gdist <- reportsetup %>%
          group_by(predict, q25g) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25g)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25g',
            title = 'Q.25g by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25gdist
      })
    })
    incProgress(1/51)
    
    output$q25h_plot <- renderPlot({
      withProgress(message = 'Q.25h plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25hdist <- reportsetup %>%
          group_by(predict, q25h) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25h)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25h',
            title = 'Q.25h by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25hdist
      })
    })
    incProgress(1/51)
    
    output$q25i_plot <- renderPlot({
      withProgress(message = 'Q.25i plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25idist <- reportsetup %>%
          group_by(predict, q25i) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25i)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25i',
            title = 'Q.25i by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25idist
      })
    })
    incProgress(1/51)
    
    output$q25j_plot <- renderPlot({
      withProgress(message = 'Q.25j plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25jdist <- reportsetup %>%
          group_by(predict, q25j) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25j)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25j',
            title = 'Q.25j by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25jdist
      })
    })
    incProgress(1/51)
    
    output$q25k_plot <- renderPlot({
      withProgress(message = 'Q.25k plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25kdist <- reportsetup %>%
          group_by(predict, q25k) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25k)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25k',
            title = 'Q.25k by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25kdist
      })
    })
    incProgress(1/51)
    
    output$q25l_plot <- renderPlot({
      withProgress(message = 'Q.25l plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25ldist <- reportsetup %>%
          group_by(predict, q25l) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25l)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25l',
            title = 'Q.25l by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25ldist
      })
    })
    incProgress(1/51)
    
    output$q25m_plot <- renderPlot({
      withProgress(message = 'Q.25m plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25mdist <- reportsetup %>%
          group_by(predict, q25m) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25m)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25m',
            title = 'Q.25m by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25mdist
      })
    })
    incProgress(1/51)
    
    output$q25n_plot <- renderPlot({
      withProgress(message = 'Q.25n plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25ndist <- reportsetup %>%
          group_by(predict, q25n) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25n)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25n',
            title = 'Q.25n by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25ndist
      })
    })
    incProgress(1/51)
    
    output$q25o_plot <- renderPlot({
      withProgress(message = 'Q.25o plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25odist <- reportsetup[which(is.na(reportsetup$q25o)==FALSE), ] %>%
          group_by(predict, q25o) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25o)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25o',
            title = 'Q.25o by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25odist
      })
    })
    incProgress(1/51)
    
    output$q25p_plot <- renderPlot({
      withProgress(message = 'Q.25p plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q25pdist <- reportsetup[which(is.na(reportsetup$q25p)==FALSE), ] %>%
          group_by(predict, q25p) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q25p)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.25p',
            title = 'Q.25p by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q25pdist
      })
    })
    incProgress(1/51)
    
    output$qb26_plot <- renderPlot({
      withProgress(message = 'Q.B26 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb26dist <- reportsetup[which(is.na(reportsetup$qb26)==FALSE), ] %>%
          group_by(predict, qb26) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb26)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B26',
            title = 'Q.B26 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb26dist
      })
    })
    incProgress(1/51)
    
    output$qc26_plot <- renderPlot({
      withProgress(message = 'Q.C26 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qc26dist <- reportsetup[which(is.na(reportsetup$qc26)==FALSE), ] %>%
          group_by(predict, qc26) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc26)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C26',
            title = 'Q.C26 by Factor'
          ) +
          theme(legend.position="bottom", legend.direction = "vertical")
        gg_prop1 %+%
          qc26dist
      })
    })
    incProgress(1/51)
    
    output$oftvote_plot <- renderPlot({
      withProgress(message = 'Vote Frequency plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        oftvotedist <- reportsetup %>%
          group_by(predict, oftvote) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = oftvote)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'OFTVOTE',
            title = 'OFTVOTE by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          oftvotedist
      })
    })
    incProgress(1/51)
    
    output$q26f1_plot <- renderPlot({
      withProgress(message = 'Q.25.f1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q26f1dist <- reportsetup[which(is.na(reportsetup$q26f1.r)==FALSE), ] %>%
          group_by(predict, q26f1.r) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q26f1.r)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.26.f1',
            title = 'Q.26.f1 (divided by 10, rounded) by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q26f1dist
      })
    })
    incProgress(1/51)
    
    output$q26f2_plot <- renderPlot({
      withProgress(message = 'Q.26.f2 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        q26f2dist <- reportsetup[which(is.na(reportsetup$q26f2.r)==FALSE), ] %>%
          group_by(predict, q26f2.r) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q26f2.r)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.26.f2',
            title = 'Q.26.f2 (divided by 10, rounded) by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q26f2dist
      })
    })
    incProgress(1/51)
    
    output$qb27_plot <- renderPlot({
      withProgress(message = 'Q.B27 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb27dist <- reportsetup[which(is.na(reportsetup$qb27)==FALSE), ] %>%
          group_by(predict, qb27) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb27)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B27',
            title = 'Q.B27 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb27dist
      })
    })
    incProgress(1/51)
    
    output$qb27a_plot <- renderPlot({
      withProgress(message = 'Q.B27a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        qb27adist <- reportsetup[which(is.na(reportsetup$qb27a)==FALSE), ] %>%
          group_by(predict, qb27a) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb27a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B27a',
            title = 'Q.B27a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb27adist
      })
    })
    incProgress(1/51)
    
    output$int1_plot <- renderPlot({
      withProgress(message = 'INT1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        int1dist <- reportsetup %>%
          group_by(predict, int1) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = int1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'INT1',
            title = 'INT1 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          int1dist
      })
    })
    incProgress(1/51)
    
    output$int2_plot <- renderPlot({
      withProgress(message = 'INT2 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        int2dist <- reportsetup[which(is.na(reportsetup$int2)==FALSE), ] %>%
          group_by(predict, int2) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = int2)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'INT2',
            title = 'INT2 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          int2dist
      })
    })
    incProgress(1/51)
    
    output$int3m_plot <- renderPlot({
      withProgress(message = 'INT3M plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        int3mdist <- reportsetup[which(is.na(reportsetup$int3m)==FALSE), ] %>%
          group_by(predict, int3m) %>%
          summarise(n = sum(weight) # weight variable)
          ) %>%
          mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = int3m)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'INT3M',
            title = 'INT3M by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          int3mdist
      })
    })
    incProgress(1/51)
    
  })
  ## Next Tab:
  output$qc28a_plot <- renderPlot({
    qc28adist <- reportsetup[which(is.na(reportsetup$qc28a)==FALSE), ] %>%
      group_by(predict, qc28a) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28a)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28a',
        title = 'Q.C28a by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28adist
  })
  
  output$qc28b_plot <- renderPlot({
    qc28bdist <- reportsetup[which(is.na(reportsetup$qc28b)==FALSE), ] %>%
      group_by(predict, qc28b) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28b)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28b',
        title = 'Q.C28b by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28bdist
  })
  
  output$qc28c_plot <- renderPlot({
    qc28cdist <- reportsetup[which(is.na(reportsetup$qc28c)==FALSE), ] %>%
      group_by(predict, qc28c) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28c)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28c',
        title = 'Q.C28c by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28cdist
  })
  
  output$qc28d_plot <- renderPlot({
    qc28ddist <- reportsetup[which(is.na(reportsetup$qc28d)==FALSE), ] %>%
      group_by(predict, qc28d) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28d)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28d',
        title = 'Q.C28d by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28ddist
  })
  
  output$qc28e_plot <- renderPlot({
    qc28edist <- reportsetup[which(is.na(reportsetup$qc28e)==FALSE), ] %>%
      group_by(predict, qc28e) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28e)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28e',
        title = 'Q.C28e by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28edist
  })
  
  output$qc28f_plot <- renderPlot({
    qc28fdist <- reportsetup[which(is.na(reportsetup$qc28f)==FALSE), ] %>%
      group_by(predict, qc28f) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28f)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28f',
        title = 'Q.C28f by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28fdist
  })
  
  output$qc28g_plot <- renderPlot({
    qc28gdist <- reportsetup[which(is.na(reportsetup$qc28g)==FALSE), ] %>%
      group_by(predict, qc28g) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28g)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28g',
        title = 'Q.C28g by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28gdist
  })
  
  output$qc28h_plot <- renderPlot({
    qc28hdist <- reportsetup[which(is.na(reportsetup$qc28h)==FALSE), ] %>%
      group_by(predict, qc28h) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc28h)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C28h',
        title = 'Q.C28h by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc28hdist
  })
  
  output$qa29a_plot <- renderPlot({
    qa29adist <- reportsetup[which(is.na(reportsetup$qa29a)==FALSE), ] %>%
      group_by(predict, qa29a) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qa29a)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.A29a',
        title = 'Q.A29a by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qa29adist
  })
  
  output$qa29b_plot <- renderPlot({
    qa29bdist <- reportsetup[which(is.na(reportsetup$qa29b)==FALSE), ] %>%
      group_by(predict, qa29b) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qa29b)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.A29b',
        title = 'Q.A29b by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qa29bdist
  })
  
  output$q40_plot <- renderPlot({
    q40dist <- reportsetup %>%
      group_by(predict, q40) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q40)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.40',
        title = 'Q.40 by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q40dist
  })
  
  output$qb40a_plot <- renderPlot({
    qb40adist <- reportsetup[which(is.na(reportsetup$qb40a)==FALSE), ] %>%
      group_by(predict, qb40a) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qb40a)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.B40.a',
        title = 'Q.B40.a by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qb40adist
  })
  
  output$qb40b_plot <- renderPlot({
    qb40bdist <- reportsetup[which(is.na(reportsetup$qb40b)==FALSE), ] %>%
      group_by(predict, qb40b) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qb40b)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.B40.b',
        title = 'Q.B40.b by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qb40bdist
  })
  
  output$q41_plot <- renderPlot({
    q41dist <- reportsetup %>%
      group_by(predict, q41) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q41)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.41',
        title = 'Q.41 by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q41dist
  })
  
  output$q42_plot <- renderPlot({
    q42dist <- reportsetup %>%
      group_by(predict, q42) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q42)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.42',
        title = 'Q.42 by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q42dist
  })
  
  output$q43_plot <- renderPlot({
    q43dist <- reportsetup[which(is.na(reportsetup$q43)==FALSE), ] %>%
      group_by(predict, q43) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q43)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.43',
        title = 'Q.43 by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q43dist
  })
  
  output$qc48a_plot <- renderPlot({
    qc48adist <- reportsetup[which(is.na(reportsetup$qc48a)==FALSE), ] %>%
      group_by(predict, qc48a) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc48a)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C48.a',
        title = 'Q.C48.a by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc48adist
  })
  
  output$qc48b_plot <- renderPlot({
    qc48bdist <- reportsetup[which(is.na(reportsetup$qc48b)==FALSE), ] %>%
      group_by(predict, qc48b) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc48b)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C48.b',
        title = 'Q.C48.b by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc48bdist
  })
  
  output$qc48c_plot <- renderPlot({
    qc48cdist <- reportsetup[which(is.na(reportsetup$qc48c)==FALSE), ] %>%
      group_by(predict, qc48c) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc48c)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C48.c',
        title = 'Q.C48.c by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc48cdist
  })
  
  output$qc49a_plot <- renderPlot({
    qc49adist <- reportsetup[which(is.na(reportsetup$qc49a)==FALSE), ] %>%
      group_by(predict, qc49a) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc49a)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C49.a',
        title = 'Q.C49.a by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc49adist
  })
  
  output$qc49b_plot <- renderPlot({
    qc49bdist <- reportsetup[which(is.na(reportsetup$qc49b)==FALSE), ] %>%
      group_by(predict, qc49b) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc49b)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C49.b',
        title = 'Q.C49.b by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc49bdist
  })
  
  output$qc49c_plot <- renderPlot({
    qc49cdist <- reportsetup[which(is.na(reportsetup$qc49c)==FALSE), ] %>%
      group_by(predict, qc49c) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = qc49c)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.C49.c',
        title = 'Q.C49.c by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      qc49cdist
  })
  
  output$q50q_plot <- renderPlot({
    q50qdist <- reportsetup[which(is.na(reportsetup$q50q)==FALSE), ] %>%
      group_by(predict, q50q) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50q)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.q',
        title = 'Q.50.q by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50qdist
  })
  
  output$q50r_plot <- renderPlot({
    q50rdist <- reportsetup[which(is.na(reportsetup$q50r)==FALSE), ] %>%
      group_by(predict, q50r) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50r)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.r',
        title = 'Q.50.r by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50rdist
  })
  
  output$q50s_plot <- renderPlot({
    q50sdist <- reportsetup[which(is.na(reportsetup$q50s)==FALSE), ] %>%
      group_by(predict, q50s) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50s)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.s',
        title = 'Q.50.s by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50sdist
  })
  
  output$q50t_plot <- renderPlot({
    q50tdist <- reportsetup[which(is.na(reportsetup$q50t)==FALSE), ] %>%
      group_by(predict, q50t) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50t)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.t',
        title = 'Q.50.t by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50tdist
  })
  
  output$q50u_plot <- renderPlot({
    q50udist <- reportsetup[which(is.na(reportsetup$q50u)==FALSE), ] %>%
      group_by(predict, q50u) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50u)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.u',
        title = 'Q.50.u by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50udist
  })
  
  output$q50v_plot <- renderPlot({
    q50vdist <- reportsetup[which(is.na(reportsetup$q50v)==FALSE), ] %>%
      group_by(predict, q50v) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50v)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.v',
        title = 'Q.50.v by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50vdist
  })
  
  output$q50w_plot <- renderPlot({
    q50wdist <- reportsetup[which(is.na(reportsetup$q50w)==FALSE), ] %>%
      group_by(predict, q50w) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50w)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.w',
        title = 'Q.50.w by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50wdist
  })
  
  output$q50y_plot <- renderPlot({
    q50ydist <- reportsetup[which(is.na(reportsetup$q50y)==FALSE), ] %>%
      group_by(predict, q50y) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50y)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.y',
        title = 'Q.50.y by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50ydist
  })
  
  output$q50z_plot <- renderPlot({
    q50zdist <- reportsetup[which(is.na(reportsetup$q50z)==FALSE), ] %>%
      group_by(predict, q50z) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50z)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.z',
        title = 'Q.50.z by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50zdist
  })
  
  output$q50aa_plot <- renderPlot({
    q50aadist <- reportsetup[which(is.na(reportsetup$q50aa)==FALSE), ] %>%
      group_by(predict, q50aa) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50aa)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.aa',
        title = 'Q.50.aa by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50aadist
  })
  
  output$q50bb_plot <- renderPlot({
    q50bbdist <- reportsetup[which(is.na(reportsetup$q50bb)==FALSE), ] %>%
      group_by(predict, q50bb) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50bb)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.bb',
        title = 'Q.50.bb by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50bbdist
  })
  
  output$q50dd_plot <- renderPlot({
    q50dddist <- reportsetup[which(is.na(reportsetup$q50dd)==FALSE), ] %>%
      group_by(predict, q50dd) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50dd)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.dd',
        title = 'Q.50.dd by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50dddist
  })
  
  output$q50ee_plot <- renderPlot({
    q50eedist <- reportsetup[which(is.na(reportsetup$q50ee)==FALSE), ] %>%
      group_by(predict, q50ee) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50ee)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.ee',
        title = 'Q.50.ee by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50eedist
  })
  
  output$q50ff_plot <- renderPlot({
    q50ffdist <- reportsetup[which(is.na(reportsetup$q50ff)==FALSE), ] %>%
      group_by(predict, q50ff) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50ff)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.ff',
        title = 'Q.50.ff by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50ffdist
  })
  
  output$q50gg_plot <- renderPlot({
    q50ggdist <- reportsetup[which(is.na(reportsetup$q50gg)==FALSE), ] %>%
      group_by(predict, q50gg) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50gg)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.gg',
        title = 'Q.50.gg by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50ggdist
  })
  
  output$q50hh_plot <- renderPlot({
    q50hhdist <- reportsetup[which(is.na(reportsetup$q50hh)==FALSE), ] %>%
      group_by(predict, q50hh) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q50hh)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.50.hh',
        title = 'Q.50.hh by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q50hhdist
  })
  
  output$q51ii_plot <- renderPlot({
    q51iidist <- reportsetup[which(is.na(reportsetup$q51ii)==FALSE), ] %>%
      group_by(predict, q51ii) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51ii)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.ii',
        title = 'Q.51.ii by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51iidist
  })
  
  output$q51jj_plot <- renderPlot({
    q51jjdist <- reportsetup[which(is.na(reportsetup$q51jj)==FALSE), ] %>%
      group_by(predict, q51jj) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51jj)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.jj',
        title = 'Q.51.jj by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51jjdist
  })
  
  output$q51kk_plot <- renderPlot({
    q51kkdist <- reportsetup[which(is.na(reportsetup$q51kk)==FALSE), ] %>%
      group_by(predict, q51kk) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51kk)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.kk',
        title = 'Q.51.kk by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51kkdist
  })
  
  output$q51ll_plot <- renderPlot({
    q51lldist <- reportsetup[which(is.na(reportsetup$q51ll)==FALSE), ] %>%
      group_by(predict, q51ll) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51ll)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.ll',
        title = 'Q.51.ll by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51lldist
  })
  
  output$q51mm_plot <- renderPlot({
    q51mmdist <- reportsetup[which(is.na(reportsetup$q51mm)==FALSE), ] %>%
      group_by(predict, q51mm) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51mm)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.mm',
        title = 'Q.51.mm by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51mmdist
  })
  
  output$q51oo_plot <- renderPlot({
    q51oodist <- reportsetup[which(is.na(reportsetup$q51oo)==FALSE), ] %>%
      group_by(predict, q51oo) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51oo)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.oo',
        title = 'Q.51.oo by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51oodist
  })
  
  output$q51pp_plot <- renderPlot({
    q51ppdist <- reportsetup[which(is.na(reportsetup$q51pp)==FALSE), ] %>%
      group_by(predict, q51pp) %>%
      summarise(n = sum(weight) # weight variable)
      ) %>%
      mutate(prop = prop.table(n))
    
    gg_prop1 <- ggplot(data = data.frame(),
                       aes(x = predict, y = prop, fill = q51pp)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                position = position_dodge(width = 0.9),
                vjust = -0.25) +
      scale_y_continuous(labels = percent) +
      labs(
        x = 'Factor',
        y = NULL,
        fill = 'Q.51.pp',
        title = 'Q.51.pp by Factor'
      ) +
      theme(legend.position="bottom")
    gg_prop1 %+%
      q51ppdist
  })
  
  # # Hide the loading message when the rest of the server function has executed
  # hide(id = "loading-content", anim = TRUE, animType = "fade")    
  # show("app-content")
  
  output$coefs_table <- renderDataTable({
    datatable(model.rank.coefs, options = list(pageLength = 100)) %>%
      formatPercentage('prob', 1) %>%
      formatStyle('prob',
                  background = styleColorBar(range(model.rank.coefs['prob'], na.rm = TRUE), 'lightblue'),
                  backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatRound(c('X1', 'X2', 'X3'), 3) %>%
      formatStyle('X1',
                  background = styleColorBar(range(model.rank.coefs['X1'], na.rm = TRUE), 'lightgreen'),
                  backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('X2',
                  background = styleColorBar(range(model.rank.coefs['X2'], na.rm = TRUE), 'lightgreen'),
                  backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('X3',
                  background = styleColorBar(range(model.rank.coefs['X3'], na.rm = TRUE), 'lightgreen'),
                  backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
  })
  
  # # Hide the loading message when the rest of the server function has executed
  # hide(id = "loading-content", anim = TRUE, animType = "fade")    
  # show("app-content")
  
}

)