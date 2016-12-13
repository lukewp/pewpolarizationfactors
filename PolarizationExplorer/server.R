#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom",legend.direction="vertical")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom", legend.direction="vertical")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom", legend.direction="vertical")
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
      ) + 
      theme(legend.position="bottom")
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
      ) + 
      theme(legend.position="bottom")
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
  
  output$qa1_plot <- renderPlot({
    qa1dist <- reportsetup %>%
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
  
  output$qc1_plot <- renderPlot({
    qc1dist <- reportsetup %>%
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
  
  output$qc1a_plot <- renderPlot({
    qc1adist <- reportsetup %>%
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
  
  output$qb2_plot <- renderPlot({
    qb2dist <- reportsetup %>%
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
  
  output$qb3_plot <- renderPlot({
    qb3dist <- reportsetup %>%
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
  
  output$qb4_plot <- renderPlot({
    qb4dist <- reportsetup %>%
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
  
  output$qb5_plot <- renderPlot({
    qb5dist <- reportsetup %>%
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
  
  output$qa6_plot <- renderPlot({
    qa6dist <- reportsetup %>%
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
  
  output$qa8_plot <- renderPlot({
    qa8dist <- reportsetup %>%
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
  
  output$qa9a_plot <- renderPlot({
    qa9adist <- reportsetup %>%
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
  
  output$qa9b_plot <- renderPlot({
    qa9bdist <- reportsetup %>%
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
  
  output$qa9c_plot <- renderPlot({
    qa9cdist <- reportsetup %>%
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
  
  output$qa9d_plot <- renderPlot({
    qa9ddist <- reportsetup %>%
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
  
  output$qa9e_plot <- renderPlot({
    qa9edist <- reportsetup %>%
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
  
  output$qa9f_plot <- renderPlot({
    qa9fdist <- reportsetup %>%
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
  
  output$qa9g_plot <- renderPlot({
    qa9gdist <- reportsetup %>%
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
  
  output$q11a_plot <- renderPlot({
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
  
  output$q11b_plot <- renderPlot({
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
  
  output$q11c_b_plot <- renderPlot({
    q11c_bdist <- reportsetup %>%
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
  
  output$q11e_b_plot <- renderPlot({
    q11e_bdist <- reportsetup %>%
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
  
  output$q11h_b_plot <- renderPlot({
    q11h_bdist <- reportsetup %>%
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
  
  output$q11i_b_plot <- renderPlot({
    q11i_bdist <- reportsetup %>%
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
  
  output$q11j_b_plot <- renderPlot({
    q11j_bdist <- reportsetup %>%
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
  
  output$q11at_plot <- renderPlot({
    q11atdist <- reportsetup %>%
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
  
  output$q11bt_plot <- renderPlot({
    q11btdist <- reportsetup %>%
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
  
  output$qb12_plot <- renderPlot({
    qb12dist <- reportsetup %>%
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
  
  output$q25a_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25adist
  })
  
  output$q25b_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25bdist
  })
  
  output$q25c_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25cdist
  })
  
  output$q25d_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25ddist
  })
  
  output$q25f_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25fdist
  })
  
  output$q25g_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25gdist
  })
  
  output$q25h_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25hdist
  })
  
  output$q25i_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25idist
  })
  
  output$q25j_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25jdist
  })
  
  output$q25k_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25kdist
  })
  
  output$q25l_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25ldist
  })
  
  output$q25m_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25mdist
  })
  
  output$q25n_plot <- renderPlot({
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25ndist
  })
  
  output$q25o_plot <- renderPlot({
    q25odist <- reportsetup %>%
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25odist
  })
  
  output$q25p_plot <- renderPlot({
    q25pdist <- reportsetup %>%
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
      theme(legend.position="bottom", legend.direction = "vertical")
    gg_prop1 %+%
      q25pdist
  })
    
})
