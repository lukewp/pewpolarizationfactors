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
library(ggdendro)
library(ggtern)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)
library(candisc)
library(NMF)
library(choroplethr)
library(choroplethrMaps)

## Assume all tables from shinySetup.R are in memory
# source("./shinySetup.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Intro explanation:
  withProgress(message = "Top-line plots ...", value = 0, {
    
    ## Tab coefs_table:
    output$coefs_table1 <- renderDataTable({
      withProgress(message = 'Coefficients Table ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        datatable(model.rank.coefs[c("Statement","X1","X2","X3")], 
                  rownames = FALSE,
                  options = list(
                    pageLength = 5, 
                    order = list(0, 'asc')
                  )) %>%
          formatRound(c('X1', 'X2', 'X3'), 3) %>%
          formatStyle('X1',
                      background = styleColorBar(range(model.rank.coefs['X1'], na.rm = TRUE), '#66a3ff'),
                      backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
          formatStyle('X2',
                      background = styleColorBar(range(model.rank.coefs['X2'], na.rm = TRUE), 'yellow'),
                      backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
          formatStyle('X3',
                      background = styleColorBar(range(model.rank.coefs['X3'], na.rm = TRUE), '#ff6666'),
                      backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
        
      })
    })
    
    output$triplot <- renderPlot({
      withProgress(message = 'Ternary Plot of Predicted Factors', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        a<-ggtern(model.rank.basis, aes(X1, X2, X3)) +
          geom_point(size = .3,
                     aes(colour = predict)) +
          scale_color_manual(breaks = c("1", "2", "3"),
                             values = c("royalblue3", "yellow3", "red3")) +
          theme_showarrows() +
          theme_hidegrid() +
          theme_hidemask() +
          theme(legend.position = "none") +
          labs( x       = "X1",
                xarrow  = "Equality and Human Rights",
                y       = "X2",
                yarrow  = "Traditional Values",
                z       = "X3",
                zarrow  = "Free Market Capitalism",
                title   = "2014 Pew Respondents' Factor Distribution Plot"
          )
        print(a)
      })
    })
    
    output$factor1map1 <- renderPlot({
      withProgress(message = 'Factor 1 Choropleth ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        tmpvars <- c("state","1")
        tmpdf <- data.frame(statedisttable[tmpvars])
        colnames(tmpdf) <- c("region.factor", "value")
        tmpdf$region <- as.character(tmpdf$region.factor)
        tmpdf$region.factor <- NULL
        tmpdf$region <- tolower(tmpdf$region)
        tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
        
        gg_prop1 = StateChoropleth$new(tmpdf)
        gg_prop1$set_num_colors(1)
        gg_prop1$title = "Factor 1"
        gg_prop1$legend = "Population %"
        gg_prop1$ggplot_scale = scale_fill_gradient(name = "Concentration", low = "white", high = "royalblue3")
        gg_prop1$render() + theme(legend.position = "none")
        
      })
    })
    
    output$factor2map1 <- renderPlot({
      withProgress(message = 'Factor 2 Choropleth ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        tmpvars <- c("state","2")
        tmpdf <- data.frame(statedisttable[tmpvars])
        colnames(tmpdf) <- c("region.factor", "value")
        tmpdf$region <- as.character(tmpdf$region.factor)
        tmpdf$region.factor <- NULL
        tmpdf$region <- tolower(tmpdf$region)
        tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
        
        gg_prop1 = StateChoropleth$new(tmpdf)
        gg_prop1$set_num_colors(1)
        gg_prop1$title = "Factor 2"
        # gg_prop1$legend = "Population %"
        gg_prop1$ggplot_scale = scale_fill_gradient(name = "Concentration", low = "white", high = "yellow3")
        gg_prop1$render() + theme(legend.position = "none")
        
      })
    })
    
    output$factor3map1 <- renderPlot({
      withProgress(message = 'Factor 3 Choropleth ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        tmpvars <- c("state","3")
        tmpdf <- data.frame(statedisttable[tmpvars])
        colnames(tmpdf) <- c("region.factor", "value")
        tmpdf$region <- as.character(tmpdf$region.factor)
        tmpdf$region.factor <- NULL
        tmpdf$region <- tolower(tmpdf$region)
        tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
        
        gg_prop1 = StateChoropleth$new(tmpdf)
        gg_prop1$set_num_colors(1)
        gg_prop1$title = "Factor 3"
        gg_prop1$legend = "Population %"
        gg_prop1$ggplot_scale = scale_fill_gradient(name = "Concentration", low = "white", high = "red3")
        gg_prop1$render() + theme(legend.position = "none")
        
      })
    })
    
    sliderValues <- reactive({
      data.frame(
        State = statedisttable$state,
        Electoral_Votes = statedisttable$EVs201x,
        Support_Center = (statedisttable$`1`*(input$f1)*.01 + statedisttable$`2`*(input$f2)*.01 + statedisttable$`3`*(input$f3)*.01)
      )
    })
    
    output$evs_banked <- renderText({
      sum(sliderValues()$Electoral_Votes[which(sliderValues()$Support_Center > 0.55)])})

    output$evs_inplay <- renderText({
      sum(sliderValues()$Electoral_Votes[which(sliderValues()$Support_Center <= 0.55 & sliderValues()$Support_Center > 0.45)])})

    output$dynamicmap <- renderPlot({
        tmpvars <- c("State","Support_Center")
        tmpdf <- data.frame(sliderValues()[tmpvars])
        colnames(tmpdf) <- c("region.factor", "value")
        tmpdf$region <- as.character(tmpdf$region.factor)
        tmpdf$region.factor <- NULL
        tmpdf$region <- tolower(tmpdf$region)
        tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
        
        gg_prop1 = StateChoropleth$new(tmpdf)
        gg_prop1$set_num_colors(1)
        gg_prop1$ggplot_scale = scale_fill_gradientn(limits = c(0,1), values = c(0, 0.45, 0.54999, 0.55, 1), breaks = 0.5, colors = c("white", "white", "purple","red", "red"))
        gg_prop1$render() + theme(legend.position = "none")
        
    })
        
    output$states <- renderDataTable({
      datatable(data = sliderValues(),
                rownames = FALSE,
                options = list(
                  pageLength = 10, 
                  order = list(2, 'desc')
                )) %>%
        formatPercentage('Support_Center', 1) %>%
        formatStyle('Support_Center',
                    background = styleColorBar(0:1, 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    })
  })
  
  ## Model explanation:
  withProgress(message = "All model plots ...", value = 0, {
    output$factor2016mfitsummary <- renderPrint({summary(factor2016.mfit)})
    
    output$party2016mfitsummary <- renderPrint({summary(party2016.mfit)})
    
    output$cqfactorplot <- renderPlot({
      withProgress(message = 'Chi-Square Q-Q Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        cqplot(factor2016.mfit)
      })
    })
    
    output$cqpartyplot <- renderPlot({
      withProgress(message = 'Chi-Square Q-Q Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        cqplot(party2016.mfit)
      })
    })
    
    output$manovaheplot <- renderPlot({
      withProgress(message = 'MANOVA Model 3-D Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        heplot(factor2016.mfit, ylim = (0:1), axes = FALSE, 
               xlab = "Modeled Clinton 2016 %",
               ylab = "Modeled Trump 2016 %",
               main = "Plot of Three-Factors' Explanation of 2016 Two-Party Support",
               term.labels = c("Factor 1", "Factor 2", "Factor 3"), 
               label.pos = c("left","center", "left", "left"), 
               col = c("orange", "blue", "yellow", "red"),
               offset.axes = c(-.25, -.5))
        points(mlmdf, cex = 0.2)
        text(mlmdf$D2016, mlmdf$R2016, row.names(mlmdf), cex=0.45, pos=2)
        axis(1, at=pretty(mlmdf$D2016), lab=paste(pretty(mlmdf$D2016)*100,"%"), las=TRUE)
        axis(2, at=pretty(mlmdf$R2016), lab=paste(pretty(mlmdf$R2016)*100,"%"), las=TRUE)
      })
    })
    
    output$factord2016plot <- renderPlot({
      withProgress(message = '3-Factor-based Clinton 2016 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=D2016, y=state)) +
          geom_point(size = 3, col = "blue") + 
          geom_errorbarh(aes(xmin = factord16min, xmax = factord16max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Hillary Clinton 2016",
            y = NULL,
            title = "3-Factor-based Explanation of 2016 Clinton Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factorr2016plot <- renderPlot({
      withProgress(message = '3-Factor-based Trump 2016 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=R2016, y=state)) +
          geom_point(size = 3, col = "red") + 
          geom_errorbarh(aes(xmin = factorr16min, xmax = factorr16max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Donald Trump 2016",
            y = NULL,
            title = "3-Factor-based Explanation of 2016 Trump Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factor2012mfitsummary <- renderPrint({summary(factor2012.mfit)})
    output$party2012mfitsummary <- renderPrint({summary(party2012.mfit)})
    
    output$factord2012plot <- renderPlot({
      withProgress(message = '3-Factor-based Obama 2012 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=D2012, y=state)) +
          geom_point(size = 3, col = "blue") + 
          geom_errorbarh(aes(xmin = factord12min, xmax = factord12max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Barack Obama 2012",
            y = NULL,
            title = "3-Factor-based Explanation of 2012 Obama Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factorr2012plot <- renderPlot({
      withProgress(message = '3-Factor-based Romney 2012 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=R2012, y=state)) +
          geom_point(size = 3, col = "red") + 
          geom_errorbarh(aes(xmin = factorr12min, xmax = factorr12max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Mitt Romney 2012",
            y = NULL,
            title = "3-Factor-based Explanation of 2012 Romney Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factor2008mfitsummary <- renderPrint({summary(factor2008.mfit)})
    
    output$factord2008plot <- renderPlot({
      withProgress(message = '3-Factor-based Obama 2008 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=D2008, y=state)) +
          geom_point(size = 3, col = "blue") + 
          geom_errorbarh(aes(xmin = factord08min, xmax = factord08max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Barack Obama 2008",
            y = NULL,
            title = "3-Factor-based Explanation of 2008 Obama Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factorr2008plot <- renderPlot({
      withProgress(message = '3-Factor-based McCain 2008 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=R2008, y=state)) +
          geom_point(size = 3, col = "red") + 
          geom_errorbarh(aes(xmin = factorr08min, xmax = factorr08max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support John McCain 2008",
            y = NULL,
            title = "3-Factor-based Explanation of 2008 McCain Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factor20163mfitsummary <- renderPrint({summary(factor2016.3mfit)})
    
    output$factordp2016mfitsummary <- renderPrint({summary(factordp2016.mfit)})
    
    output$factorclintondp16plot <- renderPlot({
      withProgress(message = '3-Factor-based Clinton Primary 2016 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=ClintonDP16, y=state)) +
          geom_point(size = 3, col = "blue") + 
          geom_errorbarh(aes(xmin = factorclintondp16min, xmax = factorclintondp16max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Hillary Clinton 2016",
            y = NULL,
            title = "3-Factor-based Explanation of 2016 Clinton Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factorsandersdp16plot <- renderPlot({
      withProgress(message = '3-Factor-based Sanders 2016 Explanation Plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        ggplot(statedisttable, aes(x=SandersDP16, y=state)) +
          geom_point(size = 3, col = "red") + 
          geom_errorbarh(aes(xmin = factorsandersdp2016min, xmax = factorsandersdp2016max), show.legend = TRUE) +
          scale_x_continuous(labels = percent) +
          labs(
            x = "% Support Bernie Sanders 2016",
            y = NULL,
            title = "3-Factor-based Explanation of 2016 Sanders Support %"
          ) + 
          theme(legend.position="bottom")
      })
    })
    
    output$factor19923mfitsummary <- renderPrint({summary(factor1992.3mfit)})
    
  })
  
  ## Tab demographics:
  withProgress(message = 'All demographics plots ...', value = 0, {
    
    output$usr_plot <- renderPlot({
      withProgress(message = 'Location Type plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }  
        
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
    
    output$birth_hisp_plot <- renderPlot({
      withProgress(message = 'Hispanic Birth Location plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = birth_hisp)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Hispanic Birth Location',
            title = 'Hispanic Birth Location by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          birth_hispdist
      })
    })
    incProgress(1/16)
    
    output$usborn_plot <- renderPlot({
      withProgress(message = 'U.S. Born plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = usborn)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'U.S. Born',
            title = 'U.S. Born by Factor'
          ) + 
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          usborndist
      })
    })
    incProgress(1/16)
    
    output$marital_plot <- renderPlot({
      withProgress(message = 'Marital Status plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
            fill = 'If Not US-Born, Citizenship Status',
            title = 'If Not US-Born, Citizenship Status by Factor'
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
    
    output$chr_plot <- renderPlot({
      withProgress(message = 'Christianity plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = chr)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Among "Other/DK": Christian?',
            title = 'Among "Other/DK": Christianity by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          chrdist
      })
    })
    incProgress(1/16)
    
    output$born_plot <- renderPlot({
      withProgress(message = 'Born Again plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = born)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Among Christian Other/DK: Born Again?',
            title = 'Among Christian Other/DK: Born Again by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          borndist
      })
    })
    incProgress(1/16)
    
    output$attend_plot <- renderPlot({
      withProgress(message = 'Church Attendance plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    
    output$inchi_plot <- renderPlot({
      withProgress(message = 'High Income plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = inchi)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Among $150k+: High Income?',
            title = 'Among $150k+: High Income by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          inchidist
      })
    })
    incProgress(1/16)
    
    output$reg_plot <- renderPlot({
      withProgress(message = 'Voter Registration plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
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
  
  ## Tab map:
  output$state_table <- renderDataTable({
    statedistnames <- c('state', '1', '2', '3')
    datatable(statedisttable[statedistnames], options = list(pageLength = 100)) %>%
      formatPercentage(c('1','2','3'), 1) %>%
      formatStyle(c('1','2','3'),
                  background = styleColorBar(0:1, 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  output$factor1map <- renderPlot({
    withProgress(message = 'Factor 1 Choropleth ...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
      } 
      
      tmpvars <- c("state","1")
      tmpdf <- data.frame(statedisttable[tmpvars])
      colnames(tmpdf) <- c("region.factor", "value")
      tmpdf$region <- as.character(tmpdf$region.factor)
      tmpdf$region.factor <- NULL
      tmpdf$region <- tolower(tmpdf$region)
      tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
      
      gg_prop1 = StateChoropleth$new(tmpdf)
      gg_prop1$set_num_colors(1)
      gg_prop1$title = "Factor 1"
      gg_prop1$legend = "Population %"
      gg_prop1$ggplot_scale = scale_fill_gradient(name = "Concentration", low = "white", high = "royalblue3")
      gg_prop1$render()
      
    })
  })
  
  output$factor2map <- renderPlot({
    withProgress(message = 'Factor 2 Choropleth ...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
      } 
      
      tmpvars <- c("state","2")
      tmpdf <- data.frame(statedisttable[tmpvars])
      colnames(tmpdf) <- c("region.factor", "value")
      tmpdf$region <- as.character(tmpdf$region.factor)
      tmpdf$region.factor <- NULL
      tmpdf$region <- tolower(tmpdf$region)
      tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
      
      gg_prop1 = StateChoropleth$new(tmpdf)
      gg_prop1$set_num_colors(1)
      gg_prop1$title = "Factor 2"
      gg_prop1$legend = "Population %"
      gg_prop1$ggplot_scale = scale_fill_gradient(name = "Concentration", low = "white", high = "yellow3")
      gg_prop1$render()
      
    })
  })
  
  output$factor3map <- renderPlot({
    withProgress(message = 'Factor 3 Choropleth ...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
      } 
      
      tmpvars <- c("state","3")
      tmpdf <- data.frame(statedisttable[tmpvars])
      colnames(tmpdf) <- c("region.factor", "value")
      tmpdf$region <- as.character(tmpdf$region.factor)
      tmpdf$region.factor <- NULL
      tmpdf$region <- tolower(tmpdf$region)
      tmpdf$region[tmpdf$region=="washington dc"] <- "district of columbia"
      
      gg_prop1 = StateChoropleth$new(tmpdf)
      gg_prop1$set_num_colors(1)
      gg_prop1$title = "Factor 3"
      gg_prop1$legend = "Population %"
      gg_prop1$ggplot_scale = scale_fill_gradient(name = "Concentration", low = "white", high = "red3")
      gg_prop1$render()
      
    })
  })
  
  ## Tab survey1:
  withProgress(message = 'All survey1 plots ...', value = 0, {
    
    output$qa1_plot <- renderPlot({
      withProgress(message = 'Q.A1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    
    output$q1c1_plot <- renderPlot({
      withProgress(message = 'Q1.C1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q1c1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q1.C1',
            title = 'Q1.C1 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          q1c1dist
      })
    })
    incProgress(1/51)
    
    output$qc1a_plot <- renderPlot({
      withProgress(message = 'Q.C1a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
  
  ## Tab survey2:
  withProgress(message = 'All survey2 plots ...', value = 0, {
    output$qc28a_plot <- renderPlot({
      withProgress(message = 'Q.C28a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28b_plot <- renderPlot({
      withProgress(message = 'Q.C28b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28c_plot <- renderPlot({
      withProgress(message = 'Q.C28c plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28d_plot <- renderPlot({
      withProgress(message = 'Q.C28d plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28e_plot <- renderPlot({
      withProgress(message = 'Q.C28e plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28f_plot <- renderPlot({
      withProgress(message = 'Q.C28f plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28g_plot <- renderPlot({
      withProgress(message = 'Q.C28g plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc28h_plot <- renderPlot({
      withProgress(message = 'Q.C28h plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qa29a_plot <- renderPlot({
      withProgress(message = 'Q.A29a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qa29b_plot <- renderPlot({
      withProgress(message = 'Q.A29b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q40_plot <- renderPlot({
      withProgress(message = 'Q.40 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qb40a_plot <- renderPlot({
      withProgress(message = 'Q.B40.a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qb40b_plot <- renderPlot({
      withProgress(message = 'Q.B40.b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q41_plot <- renderPlot({
      withProgress(message = 'Q.41 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q42_plot <- renderPlot({
      withProgress(message = 'Q.42 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q43_plot <- renderPlot({
      withProgress(message = 'Q.43 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc48a_plot <- renderPlot({
      withProgress(message = 'Q.C48.a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc48b_plot <- renderPlot({
      withProgress(message = 'Q.C48.a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc48c_plot <- renderPlot({
      withProgress(message = 'Q.C48.c plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc49a_plot <- renderPlot({
      withProgress(message = 'Q.C49.a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc49b_plot <- renderPlot({
      withProgress(message = 'Q.C49.b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$qc49c_plot <- renderPlot({
      withProgress(message = 'Q.C49.c plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50q_plot <- renderPlot({
      withProgress(message = 'Q.50.q plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50r_plot <- renderPlot({
      withProgress(message = 'Q.50.r plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50s_plot <- renderPlot({
      withProgress(message = 'Q.50.s plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50t_plot <- renderPlot({
      withProgress(message = 'Q.50.t plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50u_plot <- renderPlot({
      withProgress(message = 'Q.50.u plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50v_plot <- renderPlot({
      withProgress(message = 'Q.50.v plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50w_plot <- renderPlot({
      withProgress(message = 'Q.50.w plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50y_plot <- renderPlot({
      withProgress(message = 'Q.50.y plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50z_plot <- renderPlot({
      withProgress(message = 'Q.50.z plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50aa_plot <- renderPlot({
      withProgress(message = 'Q.50.aa plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50bb_plot <- renderPlot({
      withProgress(message = 'Q.50.bb plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50dd_plot <- renderPlot({
      withProgress(message = 'Q.50.dd plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50ee_plot <- renderPlot({
      withProgress(message = 'Q.50.ee plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50ff_plot <- renderPlot({
      withProgress(message = 'Q.50.ff plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50gg_plot <- renderPlot({
      withProgress(message = 'Q.50.gg plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q50hh_plot <- renderPlot({
      withProgress(message = 'Q.50.hh plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51ii_plot <- renderPlot({
      withProgress(message = 'Q.51.ii plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51jj_plot <- renderPlot({
      withProgress(message = 'Q.51.jj plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51kk_plot <- renderPlot({
      withProgress(message = 'Q.51.kk plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51ll_plot <- renderPlot({
      withProgress(message = 'Q.51.ll plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51mm_plot <- renderPlot({
      withProgress(message = 'Q.51.mm plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51oo_plot <- renderPlot({
      withProgress(message = 'Q.51.oo plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
    output$q51pp_plot <- renderPlot({
      withProgress(message = 'Q.51.pp plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    })
    incProgress(1/46)
    
  })
  
  ## Tab survey3:
  withProgress(message = 'All survey3 plots ...', value = 0, {
    output$q53_plot <- renderPlot({
      withProgress(message = 'Q.53 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q53)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.53',
            title = 'Q.53 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q53dist
      })
    })
    incProgress(1/46)
    
    output$qb54_plot <- renderPlot({
      withProgress(message = 'Q.B54 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb54)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B54',
            title = 'Q.B54 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb54dist
      })
    })
    incProgress(1/46)
    
    output$qb55_plot <- renderPlot({
      withProgress(message = 'Q.B55 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb55)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B55',
            title = 'Q.B55 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb55dist
      })
    })
    incProgress(1/46)
    
    output$qc56_plot <- renderPlot({
      withProgress(message = 'Q.C56 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc56)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C56',
            title = 'Q.C56 by Factor'
          ) +
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          qc56dist
      })
    })
    incProgress(1/46)
    
    output$qc57_plot <- renderPlot({
      withProgress(message = 'Q.C57 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc57)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C57',
            title = 'Q.C57 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc57dist
      })
    })
    incProgress(1/46)
    
    output$qc58a_plot <- renderPlot({
      withProgress(message = 'Q.C58a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc58a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C58a',
            title = 'Q.C58a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc58adist
      })
    })
    incProgress(1/46)
    
    output$qc58b_plot <- renderPlot({
      withProgress(message = 'Q.C58b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc58b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C58b',
            title = 'Q.C58b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc58bdist
      })
    })
    incProgress(1/46)
    
    output$q100_plot <- renderPlot({
      withProgress(message = 'Q.100 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q100)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.100',
            title = 'Q.100 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q100dist
      })
    })
    incProgress(1/46)
    
    output$q101_plot <- renderPlot({
      withProgress(message = 'Q.101 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q101)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.101',
            title = 'Q.101 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q101dist
      })
    })
    incProgress(1/46)
    
    output$q102_plot <- renderPlot({
      withProgress(message = 'Q.102 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q102)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.102',
            title = 'Q.102 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q102dist
      })
    })
    incProgress(1/46)
    
    output$q102a_plot <- renderPlot({
      withProgress(message = 'Q.102a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q102a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.102a',
            title = 'Q.102a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q102adist
      })
    })
    incProgress(1/46)
    
    output$q105a_plot <- renderPlot({
      withProgress(message = 'Q.105a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q105a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.105a',
            title = 'Q.105a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q105adist
      })
    })
    incProgress(1/46)
    
    output$q105b_plot <- renderPlot({
      withProgress(message = 'Q.105b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q105b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.105b',
            title = 'Q.105b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q105bdist
      })
    })
    incProgress(1/46)
    
    output$q105d_plot <- renderPlot({
      withProgress(message = 'Q.105d plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q105d)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.105d',
            title = 'Q.105d by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q105ddist
      })
    })
    incProgress(1/46)
    
    output$q106_plot <- renderPlot({
      withProgress(message = 'Q.106 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q106)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.106',
            title = 'Q.106 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q106dist
      })
    })
    incProgress(1/46)
    
    output$qb106_plot <- renderPlot({
      withProgress(message = 'Q.B106 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb106)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B106',
            title = 'Q.B106 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb106dist
      })
    })
    incProgress(1/46)
    
    output$qb107_plot <- renderPlot({
      withProgress(message = 'Q.B107 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb107)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B107',
            title = 'Q.B107 by Factor'
          ) +
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          qb107dist
      })
    })
    incProgress(1/46)
    
    output$qb108_plot <- renderPlot({
      withProgress(message = 'Q.B108 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb108)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B108',
            title = 'Q.B108 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb108dist
      })
    })
    incProgress(1/46)
    
    output$qb109_plot <- renderPlot({
      withProgress(message = 'Q.B109 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb109)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B109',
            title = 'Q.B109 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb109dist
      })
    })
    incProgress(1/46)
    
    output$qb110_plot <- renderPlot({
      withProgress(message = 'Q.B110 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb110)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B110',
            title = 'Q.B110 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb110dist
      })
    })
    incProgress(1/46)
    
    output$qc111_plot <- renderPlot({
      withProgress(message = 'Q.C111 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc111)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C111',
            title = 'Q.C111 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc111dist
      })
    })
    incProgress(1/46)
    
    output$qc112_plot <- renderPlot({
      withProgress(message = 'Q.C112 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc112)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C112',
            title = 'Q.C112 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc112dist
      })
    })
    incProgress(1/46)
    
    output$qc115_plot <- renderPlot({
      withProgress(message = 'Q.C115 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc115)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C115',
            title = 'Q.C115 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc115dist
      })
    })
    incProgress(1/46)
    
    output$qc116_plot <- renderPlot({
      withProgress(message = 'Q.C116 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc116)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C116',
            title = 'Q.C116 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc116dist
      })
    })
    incProgress(1/46)
    
    output$qc116a_plot <- renderPlot({
      withProgress(message = 'Q.C116a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc116a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C116a',
            title = 'Q.C116a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc116adist
      })
    })
    incProgress(1/46)
    
    output$qc117_plot <- renderPlot({
      withProgress(message = 'Q.C117 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc117)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C117',
            title = 'Q.C117 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc117dist
      })
    })
    incProgress(1/46)
    
    output$q121_plot <- renderPlot({
      withProgress(message = 'Q.121 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q121)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.121',
            title = 'Q.121 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q121dist
      })
    })
    incProgress(1/46)
    
    output$q121a_plot <- renderPlot({
      withProgress(message = 'Q.121a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q121a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.121a',
            title = 'Q.121a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q121adist
      })
    })
    incProgress(1/46)
    
    output$q121b_plot <- renderPlot({
      withProgress(message = 'Q.121b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q121b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.121b',
            title = 'Q.121b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q121bdist
      })
    })
    incProgress(1/46)
    
    output$q122_plot <- renderPlot({
      withProgress(message = 'Q.122 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q122)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.122',
            title = 'Q.122 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q122dist
      })
    })
    incProgress(1/46)
    
    output$q122a_plot <- renderPlot({
      withProgress(message = 'Q.122a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q122a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.122a',
            title = 'Q.122a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q122adist
      })
    })
    incProgress(1/46)
    
    output$q122b_plot <- renderPlot({
      withProgress(message = 'Q.122b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q122b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.122b',
            title = 'Q.122b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q122bdist
      })
    })
    incProgress(1/46)
    
    output$q123_plot <- renderPlot({
      withProgress(message = 'Q.123 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q123)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.123',
            title = 'Q.123 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q123dist
      })
    })
    incProgress(1/46)
    
    output$q123a_plot <- renderPlot({
      withProgress(message = 'Q.123a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q123a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.123a',
            title = 'Q.123a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q123adist
      })
    })
    incProgress(1/46)
    
    output$q123b_plot <- renderPlot({
      withProgress(message = 'Q.123b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q123b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.123b',
            title = 'Q.123b by Factor'
          ) +
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          q123bdist
      })
    })
    incProgress(1/46)
    
    output$q124_plot <- renderPlot({
      withProgress(message = 'Q.124 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q124)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.124',
            title = 'Q.124 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q124dist
      })
    })
    incProgress(1/46)
    
    output$q124a_plot <- renderPlot({
      withProgress(message = 'Q.124a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q124a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.124a',
            title = 'Q.124a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q124adist
      })
    })
    incProgress(1/46)
    
    output$q124b_plot <- renderPlot({
      withProgress(message = 'Q.124b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q124b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.124b',
            title = 'Q.124b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q124bdist
      })
    })
    incProgress(1/46)
    
    output$q125_plot <- renderPlot({
      withProgress(message = 'Q.125 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q125)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.125',
            title = 'Q.125 by Factor'
          ) +
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          q125dist
      })
    })
    incProgress(1/46)
    
    output$q125a_plot <- renderPlot({
      withProgress(message = 'Q.125a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q125a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.125a',
            title = 'Q.125a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q125adist
      })
    })
    incProgress(1/46)
    
    output$q125b_plot <- renderPlot({
      withProgress(message = 'Q.125b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q125b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.125b',
            title = 'Q.125b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q125bdist
      })
    })
    incProgress(1/46)
    
    output$q126_plot <- renderPlot({
      withProgress(message = 'Q.126 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q126)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.126',
            title = 'Q.126 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q126dist
      })
    })
    incProgress(1/46)
    
    output$q126a_plot <- renderPlot({
      withProgress(message = 'Q.126a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q126a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.126a',
            title = 'Q.126a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q126adist
      })
    })
    incProgress(1/46)
    
    output$q126b_plot <- renderPlot({
      withProgress(message = 'Q.126b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q126b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.126b',
            title = 'Q.126b by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q126bdist
      })
    })
    incProgress(1/46)
    
    output$qc127_plot <- renderPlot({
      withProgress(message = 'Q.C127 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc127)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C127',
            title = 'Q.C127 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc127dist
      })
    })
    incProgress(1/46)
    
    output$qc128_plot <- renderPlot({
      withProgress(message = 'Q.C128 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc128)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C128',
            title = 'Q.C128 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qc128dist
      })
    })
    incProgress(1/46)
    
  })
  
  ## Tab survey4:
  withProgress(message = 'All survey4 plots ...', value = 0, {
    
    output$qc135_plot <- renderPlot({
      withProgress(message = 'Q.C135 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc135)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C135',
            title = 'Q.C135 by Factor'
          ) +
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          qc135dist
      })
    })
    incProgress(1/46)
    
    output$qc135a_plot <- renderPlot({
      withProgress(message = 'Q.C135a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc135a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C135a',
            title = 'Q.C135a by Factor'
          ) +
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          qc135adist
      })
    })
    incProgress(1/46)
    
    output$sex_plot1 <- renderPlot({
      withProgress(message = 'Sex plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }  
        # sexdist <- reportsetup[which(is.na(reportsetup$sex)==FALSE), ] %>%
        #   group_by(predict, sex) %>%
        #   summarise(n = sum(weight) # weight variable) 
        #   ) %>%
        #   mutate(prop = prop.table(n))
        
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
            title = 'Sex by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          sexdist
      })
    })
    incProgress(1/46)
    
    output$age_plot1 <- renderPlot({
      withProgress(message = 'Age plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        # agedist <- reportsetup[which(is.na(reportsetup$age.r)==FALSE), ] %>%
        #   group_by(predict, age.r) %>%
        #   summarise(n = sum(weight) # weight variable) 
        #   ) %>%
        #   mutate(prop = prop.table(n))
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = age.r)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
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
    incProgress(1/46)
    
    output$educ_plot1 <- renderPlot({
      withProgress(message = 'Education plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        # educdist <- reportsetup[which(is.na(reportsetup$educ)==FALSE), ] %>%
        #   group_by(predict, educ) %>%
        #   summarise(n = sum(weight) # weight variable) 
        #   ) %>%
        #   mutate(prop = prop.table(n))
        
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
    incProgress(1/46)
    
    output$hisp_plot1 <- renderPlot({
      withProgress(message = 'Hispanic plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        # hispdist <- reportsetup[which(is.na(reportsetup$hisp)==FALSE), ] %>%
        #   group_by(predict, hisp) %>%
        #   summarise(n = sum(weight) # weight variable) 
        #   ) %>%
        #   mutate(prop = prop.table(n))
        
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
    incProgress(1/46)
    
    output$race_plot1 <- renderPlot({
      withProgress(message = 'Race plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        # racedist <- reportsetup[which(is.na(reportsetup$race)==FALSE), ] %>%
        #   group_by(predict, race) %>%
        #   summarise(n = sum(weight) # weight variable) 
        #   ) %>%
        #   mutate(prop = prop.table(n))
        
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
    incProgress(1/46)
    
    output$birth_hisp_plot1 <- renderPlot({
      withProgress(message = 'Birth plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = birth_hisp)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Birth',
            title = 'Birth by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          birth_hispdist
      })
    })
    incProgress(1/46)
    
    output$usborn_plot1 <- renderPlot({
      withProgress(message = 'U.S. Born plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = usborn)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'U.S. Born',
            title = 'U.S. Born by Factor'
          ) + 
          theme(legend.position="bottom",legend.direction="vertical")
        gg_prop1 %+%
          usborndist
      })
    })
    incProgress(1/46)
    
    output$marital_plot1 <- renderPlot({
      withProgress(message = 'Marital Status plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    incProgress(1/46)
    
    output$parent_plot1 <- renderPlot({
      withProgress(message = 'Parent status plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    incProgress(1/46)
    
    output$citizen_plot1 <- renderPlot({
      withProgress(message = 'Citizenship plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
            fill = 'If Not US-Born, Citizenship Status',
            title = 'If Not US-Born, Citizenship Status by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          citizendist
      })
    })
    incProgress(1/46)
    
    output$relig_plot1 <- renderPlot({
      withProgress(message = 'Religion plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    incProgress(1/46)
    
    output$chr_plot1 <- renderPlot({
      withProgress(message = 'Christianity plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = chr)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Among "Other/DK": Christian?',
            title = 'Among "Other/DK": Christianity by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          chrdist
      })
    })
    incProgress(1/46)
    
    output$born_plot1 <- renderPlot({
      withProgress(message = 'Born Again plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = born)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Among Other/DK Christian: Born Again?',
            title = 'Among Other/DK Christian: Born Again by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          borndist
      })
    })
    incProgress(1/46)
    
    output$attend_plot1 <- renderPlot({
      withProgress(message = 'Church Attendance plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    incProgress(1/46)
    
    output$qb139_plot <- renderPlot({
      withProgress(message = 'Q.B139 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb139)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B139',
            title = 'Q.B139 by Factor'
          ) + 
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          qb139dist
      })
    })
    incProgress(1/46)
    
    output$qb139a_plot <- renderPlot({
      withProgress(message = 'Q.B139a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb139a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B139a',
            title = 'Q.B139a by Factor'
          ) + 
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          qb139adist
      })
    })
    incProgress(1/46)
    
    output$income_plot1 <- renderPlot({
      withProgress(message = 'Income plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    incProgress(1/46)
    
    output$inchi_plot1 <- renderPlot({
      withProgress(message = 'High Income plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = inchi)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Among $150k+: High Income?',
            title = 'Among $150k+: High Income by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          inchidist
      })
    })
    incProgress(1/46)
    
    output$reg_plot1 <- renderPlot({
      withProgress(message = 'Voter Registration plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
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
    incProgress(1/46)
    
    output$party_plot1 <- renderPlot({
      withProgress(message = 'Political Party plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
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
    incProgress(1/46)
    
    output$partyln_plot1 <- renderPlot({
      withProgress(message = 'Political Party Leaner plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = partyln)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Party Lean',
            title = '(IF OTHER PARTY): Party Lean by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          partylndist
      })
    })
    incProgress(1/46)
    
    output$partystr_plot1 <- renderPlot({
      withProgress(message = 'Political Party Strength plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = partystr)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Party Strength',
            title = '(IF Dem/Rep PARTY): Party Strength by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          partystrdist
      })
    })
    incProgress(1/46)
    
    output$ideo_plot1 <- renderPlot({
      withProgress(message = 'Political Ideology plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
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
    incProgress(1/46)
    
    output$qb140_plot <- renderPlot({
      withProgress(message = 'Q.B140 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb140)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B140',
            title = 'Q.B140 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb140dist
      })
    })
    incProgress(1/46)
    
    output$qb140b_plot <- renderPlot({
      withProgress(message = 'Q.B140b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb140b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B140b',
            title = 'Q.B140b by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb140bdist
      })
    })
    incProgress(1/46)
    
    output$qb141_plot <- renderPlot({
      withProgress(message = 'Q.B141 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb141)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B141',
            title = 'Q.B141 by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb141dist
      })
    })
    incProgress(1/46)
    
    output$qb141b_plot <- renderPlot({
      withProgress(message = 'Q.B141b plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        } 
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qb141b)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.B141b',
            title = 'Q.B141b by Factor'
          ) + 
          theme(legend.position="bottom")
        gg_prop1 %+%
          qb141bdist
      })
    })
    incProgress(1/46)
    
    output$qc142_plot <- renderPlot({
      withProgress(message = 'Q.C142 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qc142)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.C142',
            title = 'Q.C142 by Factor'
          ) +
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          qc142dist
      })
    })
    incProgress(1/46)
    
    output$q148_plot <- renderPlot({
      withProgress(message = 'Q.148 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q148)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.148',
            title = 'Q.148 by Factor'
          ) +
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          q148dist
      })
    })
    incProgress(1/46)
    
    output$q148correct_plot <- renderPlot({
      withProgress(message = 'Q.148 correct plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q148correct)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.148 Correct',
            title = 'Q.148 Correct by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          q148correctdist
      })
    })
    incProgress(1/46)
    
    output$q149_plot <- renderPlot({
      withProgress(message = 'Q.149 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q149)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.149',
            title = 'Q.149 by Factor'
          ) +
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          q149dist
      })
    })
    incProgress(1/46)
    
    output$teaparty2_plot <- renderPlot({
      withProgress(message = 'Tea Party plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = teaparty2)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Tea Party',
            title = 'Tea Party by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          teaparty2dist
      })
    })
    incProgress(1/46)
    
    output$q150_plot <- renderPlot({
      withProgress(message = 'Q.150 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = q150)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.150',
            title = 'Q.150 by Factor'
          ) +
          theme(legend.position="bottom", legend.direction="vertical")
        gg_prop1 %+%
          q150dist
      })
    })
    incProgress(1/46)
    
    output$hh1_plot <- renderPlot({
      withProgress(message = 'HH1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = hh1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'HH1',
            title = 'HH1 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          hh1dist
      })
    })
    incProgress(1/46)
    
    output$hh3_plot <- renderPlot({
      withProgress(message = 'HH3 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = hh3)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'HH3',
            title = 'HH3 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          hh3dist
      })
    })
    incProgress(1/46)
    
    output$ql1_plot <- renderPlot({
      withProgress(message = 'Q.L1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = ql1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.L1',
            title = 'Q.L1 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          ql1dist
      })
    })
    incProgress(1/46)
    
    output$ql1a_plot <- renderPlot({
      withProgress(message = 'Q.L1a plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = ql1a)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Q.L1a',
            title = 'Q.L1a by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          ql1adist
      })
    })
    incProgress(1/46)
    
    output$qc1_plot <- renderPlot({
      withProgress(message = 'Q.C1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
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
    incProgress(1/46)
    
    output$lc2_plot <- renderPlot({
      withProgress(message = 'LC2 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = lc2)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'LC2',
            title = 'LC2 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          lc2dist
      })
    })
    incProgress(1/46)
    
    output$lc3_plot <- renderPlot({
      withProgress(message = 'LC3 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = lc3)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'LC3',
            title = 'LC3 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          lc3dist
      })
    })
    incProgress(1/46)
    
    output$qzip_plot <- renderPlot({
      withProgress(message = 'QZIP plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = qzip)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'QZIP',
            title = 'QZIP by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          qzipdist
      })
    })
    incProgress(1/46)
    
    output$money_plot <- renderPlot({
      withProgress(message = 'MONEY plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = money)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'MONEY',
            title = 'MONEY by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          moneydist
      })
    })
    incProgress(1/46)
    
    output$isex_plot <- renderPlot({
      withProgress(message = 'Interviewer Sex plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = isex)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Interviewer Sex',
            title = 'Interviewer Sex by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          isexdist
      })
    })
    incProgress(1/46)
    
    output$ihisp1_plot <- renderPlot({
      withProgress(message = 'Interviewer Race (Hispanic) plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = ihisp1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Interviewer Race (Hispanic)',
            title = 'Interviewer Race (Hispanic) by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          ihisp1dist
      })
    })
    incProgress(1/46)
    
    output$irace1m1_plot <- renderPlot({
      withProgress(message = 'Interviewer Race 1 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = irace1m1)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Interviewer Race 1',
            title = 'Interviewer Race 1 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          irace1m1dist
      })
    })
    incProgress(1/46)
    
    output$irace1m2_plot <- renderPlot({
      withProgress(message = 'Interviewer Race 2 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = irace1m2)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Interviewer Race 2',
            title = 'Interviewer Race 2 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          irace1m2dist
      })
    })
    incProgress(1/46)
    
    output$irace1m3_plot <- renderPlot({
      withProgress(message = 'Interviewer Race 3 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = irace1m3)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Interviewer Race 3',
            title = 'Interviewer Race 3 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          irace1m3dist
      })
    })
    incProgress(1/46)
    
    output$irace1m4_plot <- renderPlot({
      withProgress(message = 'Interviewer Race 4 plot ...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
        }
        
        gg_prop1 <- ggplot(data = data.frame(),
                           aes(x = predict, y = prop, fill = irace1m4)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(label = paste(round(100.0 * prop, 0), '%')),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25) +
          scale_y_continuous(labels = percent) +
          labs(
            x = 'Factor',
            y = NULL,
            fill = 'Interviewer Race 4',
            title = 'Interviewer Race 4 by Factor'
          ) +
          theme(legend.position="bottom")
        gg_prop1 %+%
          irace1m4dist
      })
    })
    incProgress(1/46)
    
  })
  
  ## Tab coefs_table:
  output$coefs_table <- renderDataTable({
    withProgress(message = 'Coefficients Table ...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
      } 
      datatable(model.rank.coefs, 
                rownames = FALSE,
                options = list(
                  pageLength = 100, 
                  order = list(3, 'desc')
                )) %>%
        formatPercentage('prob', 1) %>%
        formatStyle('prob',
                    background = styleColorBar(range(0:1), 'lightgreen'),
                    backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
        formatRound(c('X1', 'X2', 'X3'), 3) %>%
        formatStyle('X1',
                    background = styleColorBar(range(model.rank.coefs['X1'], na.rm = TRUE), 'lightblue'),
                    backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
        formatStyle('X2',
                    background = styleColorBar(range(model.rank.coefs['X2'], na.rm = TRUE), 'yellow'),
                    backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
        formatStyle('X3',
                    background = styleColorBar(range(model.rank.coefs['X3'], na.rm = TRUE), '#ff6666'),
                    backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
      
    })
  })
  
  output$nmf_dendrogram <- renderPlot({
    withProgress(message = 'Factor Dendrogram ...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
      } 
      # ggdendrogram(consensushc(model.rank), rotate = TRUE) +
      #   labs(title = "Factor Assignment Variables")
      hc <- consensushc(model.rank, dendrogram = FALSE)
      ddata <- dendro_data(as.dendrogram(hc), type = "rectangle")
      # ddata$labels$group <- as.factor(cutree(hc, 3)[order(match(cutree(hc, 3), hc$order))])
      ggplot(segment(ddata)) +
        geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
        geom_text(data = ddata$labels, size = 5, vjust = 0, hjust = -0.25,
                  aes(x=x, y=y, label=label
                      # , colour = group
                  )) +
        # scale_colour_manual(values=c("blue","yellow","red")) +
        ggtitle("Factor Assignment Variables") +
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        coord_flip() +
        scale_y_reverse(expand=c(0.2, 0)) +
        scale_x_reverse() +
        theme_dendro()
    })
  })
  
  output$factor_rtplot <- renderPlot({
    withProgress(message = 'Regression Tree Model plot ...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
      } 
      # fancyRpartPlot(modrt$finalModel) # Requires rattle
      rtdata <- dendro_data(modrt$finalModel, dendrogram = FALSE)
      ggplot() +
        geom_segment(data = rtdata$segments,
                     aes(x=x, y=y, xend=xend, yend=yend)) +
        geom_text(data = rtdata$labels,
                  aes(x=x, y=y, label=label), size = 5, vjust = -0.25) +
        geom_text(data = rtdata$leaf_labels,
                  aes(x=x, y=y, label=label), size = 5, vjust = 1.25) +
        ggtitle("Simple Partition Tree Factor Assignment Model") +
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        theme_dendro()
      
      
    })
  })
  
  # Diagnostic on the model?
  output$rtsummary <- renderPrint({summary(modrt$results)})
  
})