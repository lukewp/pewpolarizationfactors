#
# This is the user-interface definition of a Shiny web application.

library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)

## Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "3-Factors Analysis of Pew Political Polarization Survey 2014",
    titleWidth = 500),
  dashboardSidebar(
    width = 150,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("file-text"), selected = TRUE),
      menuItem(
        "Demographics",
        tabName = "demographics",
        icon = icon("bar-chart")
      ),
      menuItem("States", tabName = "map", icon = icon("globe")),
      ## Breaking into different sections to ease render speed
      menuItem(
        "Survey Q.1-27",
        tabName = "survey1",
        icon = icon("file-text")
      ),
      menuItem(
        "Survey Q.28-52",
        tabName = "survey2",
        icon = icon("file-text")
      ),
      menuItem(
        "Survey Q.53-134",
        tabName = "survey3",
        icon = icon("file-text")
      ),
      menuItem(
        "Survey Q.135-End",
        tabName = "survey4",
        icon = icon("file-text")
      ),
      menuItem("Factor Coefs", tabName = "coefs_table", icon = icon("file-text"))
    )
  ),
  dashboardBody(
    
    # Add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
                              .skin-blue .main-header .logo {
                              background-color: #3c8dbc;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #3c8dbc;
                              }
                              '))),
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Introduction"),
        fluidRow(box(
          width = 12, 
          HTML("
               <p>Study of the 2014 Pew Political Polarization Survey</p>
               <p>This visualization and analysis is based on the Pew Political Polarization Dataset from 2014, available at http://www.people-press.org/2014/03/16/2014-political-polarization-survey/</p>
               <p>The analysis reveals 3 dominant factors in US political life, to help explain some American political dynamics. </p>
               <p>Instead of thinking of America's electorate as right, left, and in the middle, it's better to think of a triangle with a major (equally-sized) constituency at each point.</p>
               <p>In order to build a majority coalition, any two of these points must align behind an issue or a candidate.</p>
               <ul>
               <li>Factor 1: Human Rights and Equality -- Blue, highly correlated with the Democratic Party</li>
               <li>Factor 2: Traditional Values -- Yellow, split between the two mainstream American political parties, like neither party is consistent with their values</li>
               <li>Factor 3: Free Market Capitalism -- Red, highly correlated with the Republican Party</li>
               </ul>
               <p>In terms of electoral votes, different coalitions offer promise of delivering majorities in different states. </p>
               <p>In the 2008 election, the coalition was a 'green' coalition -- that is, a combination of the blue and yellow factor constituencies.
               In 2016, the coalition was an 'orange' coalition -- that is, a combination of the red and yellow factor constituencies.</p>
               <p>A 'purple' coalition may also be possible, one that finds common ground between the Blue and Red constituencies at the expense of the Traditional Values factor.</p>
               <p>And of course, a new party could emerge that more perfectly captures the values of the Traditional Values factor, which would introduce entirely new dynamics to the American political landscape, and appear a very viable possibility based on this analysis.</p>
               <p>Explore the 2014 Pew survey in terms of these factors with this visualization, code for which is available here: https://github.com/lukewp/pewpolarizationfactors.</p>
               ")
        ))),
      tabItem(
        tabName = "demographics",
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
          width = 12, plotOutput("educ_plot", height = 500)
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
          width = 12, plotOutput("relig_plot", height = 600)
        )),
        fluidRow(box(
          width = 12, plotOutput("attend_plot", height = 250)
        )),
        fluidRow(box(
          width = 12, plotOutput("income_plot", height = 250)
        )),
        # fluidRow(box(
        #   width = 12, plotOutput("inchi_plot", height = 250)
        # )),
        fluidRow(box(
          width = 12, plotOutput("reg_plot", height = 350)
        )),
        fluidRow(box(
          width = 12, plotOutput("party_plot", height = 250)
        )),
        fluidRow(box(
          width = 12, plotOutput("ideo_plot", height = 250)
        ))
      ),
      tabItem(
        tabName = "map",
        h2("Factor Concentration by State"),
        fluidRow(box(
          width = 12, dataTableOutput("state_table")
        ))),
      tabItem(
        tabName = "survey1",
        fluidPage(fluidRow(
          box(
            width = 12,
            HTML(
              "
                  <p><strong>PEW RESEARCH CENTER</strong></p>
                  <p><strong>POLITICAL TYPOLOGY/POLARIZATION </strong></p>
                  <p><strong>FINAL QUESTIONNAIRE</strong></p>
                  <p>&nbsp;</p>
                  <p>N=10,000 interviews of adults 18+ (5,000 landline, 5,000 cell phone) in English and Spanish.</p>
                  <p>Form 1/Form 2: each a random half sample</p>
                  <p>50 states (include Alaska and Hawaii)</p>
                  <p>Field Period: January 23-March 16, 2014</p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>LANDLINE INTRO:</strong></p>
                  <p>Hello, I am _____ calling on behalf of the Pew Research Center. We are conducting a telephone opinion survey for leading newspapers and TV stations around the country. I&rsquo;d like to ask a few questions of the <strong>[RANDOMIZE</strong>: &ldquo;YOUNGEST MALE, 18 years of age or older, who is now at home&rdquo; <strong>AND</strong> &ldquo;YOUNGEST FEMALE, 18 years of age or older, who is now at home?&rdquo;<strong>]</strong> <strong>[IF NO MALE/FEMALE, ASK:</strong> May I please speak with the YOUNGEST FEMALE/MALE, 18 years of age or older, who is now at home?] <strong>GO TO MAIN INTERVIEW</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>CELL PHONE INTRO:</strong></p>
                  <p>Hello, I am _____ calling on behalf of the Pew Research Center. We are conducting a telephone opinion survey for leading newspapers and TV stations around the country. I know I am calling you on a cell phone. As a token of our appreciation for your time, we will pay all eligible respondents $5 for participating in this survey. This is not a sales call. <strong>[IF R SAYS DRIVING/UNABLE TO TAKE CALL: </strong>Thank you. We will try you another time&hellip;<strong>]</strong>.</p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>VOICE MAIL MESSAGE (LEAVE ONLY ONCE -- THE FIRST TIME A CALL GOES TO VOICEMAIL): </strong>I am calling on behalf of the Pew Research Center. We are conducting a national opinion survey of cell phone users. This is NOT a sales call. We will try to reach you again.</p>
                  <p>&nbsp;</p>
                  <p><strong>SCREENING INTERVIEW:</strong></p>
                  <p>S1.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you under 18 years old, OR are you 18 or older?</p>
                  <table>
                  <tbody>
                  <tr>
                  <td>
                  <p>1</p>
                  </td>
                  <td>
                  <p>Under 18</p>
                  </td>
                  </tr>
                  <tr>
                  <td>
                  <p>2</p>
                  </td>
                  <td>
                  <p>18 or older</p>
                  </td>
                  </tr>
                  <tr>
                  <td>
                  <p>9</p>
                  </td>
                  <td>
                  <p>Don&rsquo;t know/Refused</p>
                  </td>
                  </tr>
                  </tbody>
                  </table>
                  <p>&nbsp;</p>
                  <p><strong>IF S1=2, CONTINUE WITH MAIN INTERVIEW</strong></p>
                  <p><strong>IF S1=1,9 THANK AND TERMINATE: This survey is limited to adults age 18 and over.&nbsp; I won&rsquo;t take any more of your time&hellip;</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>READ TO ALL CELL PHONE</strong></p>
                  <p><strong>INTRODUCTION TO MAIN INTERVIEW:</strong> If you are now driving a car or doing any activity requiring your full attention, I need to call you back later. The first question is&hellip;</p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>INTERVIEWER: </strong></p>
                  <p><strong>IF R SAYS IT IS NOT A GOOD TIME, TRY TO ARRANGE A TIME TO CALL BACK.&nbsp; OFFER THE TOLL-FREE CALL-IN NUMBER THEY CAN USE TO COMPLETE THE SURVEY BEFORE ENDING THE CONVERSATION.</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>[PROGRAMMER NOTE: PLEASE INCLUDE THE INTRODUCTION RANDOMIZATION VARIABLES IN THE ALL CONTACTS FILES. WE WOULD LIKE TO BE ABLE TO RUN RESPONSE RATES SEPARATELY FOR EACH VERSION OF THE INTRODUCTION FOR THE LANDLINE AND CELL FRAMES SEPARATELY.]</strong></p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.A1&nbsp;&nbsp; Generally, how would you say things are these days in your life -- would you say that you are very happy, pretty happy, or not too happy?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very happy</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pretty happy</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not too happy</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qa1_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C1&nbsp;&nbsp; Do you approve or disapprove of the way Barack Obama is handling his job as President? <strong>[IF DK ENTER AS DK. IF DEPENDS PROBE ONCE WITH:</strong> Overall do you approve or disapprove of the way Barack Obama is handling his job as President? <strong>IF STILL DEPENDS ENTER AS DK] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Approve</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Disapprove</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc1_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF AND APPROVE OR DISAPPROVE (Q.C1=1,2):</strong></p>
                  <p>Q.C1a Do you <strong>[</strong>approve/disapprove<strong>]</strong> very strongly, or not so strongly?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very strongly</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not so strongly</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc1a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B2&nbsp;&nbsp; All in all, are you satisfied or dissatisfied with the way things are going in this country today?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Satisfied</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Dissatisfied</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb2_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B3&nbsp;&nbsp; And thinking about the local community where you live, are you satisfied or dissatisfied with the way things are going in your local community today?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Satisfied</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Dissatisfied</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb3_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B4&nbsp;&nbsp; <strong>Thinking about the future of the United States, do you think the country's best years are ahead of us or behind us?</strong></p>
                  <p><strong><em>&nbsp;</em></strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Ahead of us</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Behind us</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb4_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B5&nbsp;&nbsp; Thinking about the Democratic and Republican parties, would you say there is a great deal of difference in what they stand for, a fair amount of difference, or hardly any difference at all?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A great deal</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A fair amount</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Hardly any</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb5_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.A6&nbsp;&nbsp; If you could live anywhere in the United States that you wanted to, would you prefer a city, a suburban area, a small town or a rural area?<em> <br /> <br /> </em></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; City</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Suburban area</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Small town</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Rural area</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qa6_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>NO QUESTION 7</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Imagine for a moment that you are moving to another community.</p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.A8<strong>&nbsp;&nbsp; </strong> Would you prefer to live in <strong>[INSERT ITEM; RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A community where the houses are larger and farther apart, but schools, stores, and restaurants are several miles away [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A community where the houses are smaller and closer to each other, but schools, stores, and restaurants are within walking distance</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don't know/Refused</p>"
            ),
            plotOutput("qa8_plot", height = 400),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.A9 &nbsp; Still imagining that you are moving to another community. In deciding where to live, would each of the following be important, or not too important to you. First, would <strong>[INSERT ITEM; RANDOMIZE] </strong>be important, or not too important? What about<strong> [NEXT ITEM]</strong>?</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>Living in a place where most people share your political views</li>"
            ),
            plotOutput("qa9a_plot", height = 250),
            HTML("
                     <li>Having high quality public schools</li>"),
            plotOutput("qa9b_plot", height = 250),
            HTML(
              "
                  <li>Living in a place with a mix of people from different racial and ethnic backgrounds</li>"
            ),
            plotOutput("qa9c_plot", height = 250),
            HTML(
              "
                  <li>Living in a place with many people who share your religious faith</li>"
            ),
            plotOutput("qa9d_plot", height = 250),
            HTML("
                     <li>Being near art museums and theaters</li>"),
            plotOutput("qa9e_plot", height = 250),
            HTML(
              "
                  <li>Having easy access to the outdoors for things like hiking, fishing, and camping</li>"
            ),
            plotOutput("qa9f_plot", height = 250),
            HTML("
                     <li>Being near your extended family</li>"),
            plotOutput("qa9g_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Important</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not too important</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>NO QUESTION 10</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Next,</p>
                  <p>Q.11&nbsp;&nbsp; Would you say your overall opinion of&hellip; <strong>[INSERT ITEM; RANDOMIZE]</strong> is very favorable, mostly favorable, mostly UNfavorable, or very unfavorable? <strong>[INTERVIEWERS: PROBE TO DISTINGUISH BETWEEN &ldquo;NEVER HEARD OF&rdquo; AND &ldquo;CAN&rsquo;T RATE.&rdquo;] </strong>How about <strong>[NEXT ITEM]</strong>?</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>The Republican Party</li></ol>"
            ),
            plotOutput("q11a_plot", height = 250),
            HTML(
              "
                  <ol>
                  <li>The Democratic Party</li>
                  </ol>
                  <p><strong><br /> </strong></p>"
            ),
            plotOutput("q11b_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK PHASE B ONLY:</strong></p>
                  <p>c.B&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Congress</p>"
            ),
            plotOutput("q11c_b_plot", height = 250),
            HTML(
              "
                  <p><strong>NO ITEM d</strong></p>
                  <p>e.B&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Labor unions</p>"
            ),
            plotOutput("q11e_b_plot", height = 250),
            HTML(
              "
                  <p><strong>NO ITEMS f AND g</strong></p>
                  <p>h.B&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The National Rifle Association</p>"
            ),
            plotOutput("q11h_b_plot", height = 250),
            HTML(
              "
                  <p>i.B &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Federal Reserve</p>"
            ),
            plotOutput("q11i_b_plot", height = 250),
            HTML(
              "
                  <p>j.B&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Environmental Protection Agency, the EPA</p>"
            ),
            plotOutput("q11j_b_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very favorable</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Mostly favorable</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Mostly unfavorable</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very unfavorable</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Never heard of <strong>(VOL.)</strong></p>
                  <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Can't rate <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Refused<strong> (VOL.)</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK PHASE C IF VERY UNFAVORABLE VIEW OF REP PARTY (Q11a=4):</strong></p>
                  <p>Q.11at&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Would you say the Republican Party&rsquo;s policies are so misguided that they threaten the nation&rsquo;s well-being, or wouldn&rsquo;t you go that far?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, Republican Party&rsquo;s policies pose a threat to the nation&rsquo;s well-being [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, wouldn&rsquo;t go that far</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q11at_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK PHASE C IF VERY UNFAVORABLE VIEW OF DEM PARTY (Q11b=4):</strong></p>
                  <p>Q.11bt&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Would you say the Democratic Party&rsquo;s policies are so misguided that they threaten the nation&rsquo;s well-being, or wouldn&rsquo;t you go that far?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, Democratic Party&rsquo;s policies pose a threat to the nation&rsquo;s well-being [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, wouldn&rsquo;t go that far</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q11bt_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B12 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Thinking about elected officials in Washington who share your positions on the most important issues facing the nation. <strong>[READ AND RANDOMIZE]</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should they work with elected officials they disagree with, even if it results in some policies you don&rsquo;t like [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should they stand up for their positions, even if that means little gets done in Washington</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("qb12_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 13-24</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.25&nbsp;&nbsp; I'm going to read you some pairs of statements that will help us understand how you feel about a number of things. As I read each pair, tell me whether the FIRST statement or the SECOND statement comes closer to your own views &mdash; even if neither is exactly right. The first pair is <strong>[READ AND RANDOMIZE PAIRS BUT <u>NOT </u>STATEMENTS WITHIN EACH PAIR]</strong>. Next, <strong>[NEXT PAIR] [IF NECESSARY: </strong>&ldquo;Which statement comes closer to your views, even if neither is exactly right?&rdquo;]</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>Government is almost always wasteful and inefficient [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Government often does a better job than people give it credit for</p>"
            ),
            plotOutput("q25a_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p>&nbsp;</p>
                  <li>Government regulation of business is necessary to protect the public interest [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Government regulation of business usually does more harm than good</p>"
            ),
            plotOutput("q25b_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Poor people today have it easy because they can get government benefits without doing anything in return [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Poor people have hard lives because government benefits don't go far enough to help them live decently</p>"
            ),
            plotOutput("q25c_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>The government should do more to help needy Americans, even if it means going deeper into debt [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The government today can't afford to do much more to help the needy</p>"
            ),
            plotOutput("q25d_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li><strong>NO ITEM e</strong></li>
                  <p>&nbsp;</p>
                  <li>Racial discrimination is the main reason why many black people can't get ahead these days [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Blacks who can't get ahead in this country are mostly responsible for their own condition</p>"
            ),
            plotOutput("q25f_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Immigrants today strengthen our country because of their hard work and talents [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Immigrants today are a burden on our country because they take our jobs, housing and health care</p>"
            ),
            plotOutput("q25g_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Society is better off if people make marriage and having children a priority [OR]</li>
                  <p>Society is just as well off if people have priorities other than marriage and children</p>"
            ),
            plotOutput("q25h_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>The best way to ensure peace is through military strength [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Good diplomacy is the best way to ensure peace</p>"
            ),
            plotOutput("q25i_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <li>U.S. efforts to solve problems around the world usually end up making things worse [OR] Problems in the world would be even worse without U.S. involvement</li>"
            ),
            plotOutput("q25j_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Most people who want to get ahead can make it if they're willing to work hard [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Hard work and determination are no guarantee of success for most people</p>"
            ),
            plotOutput("q25k_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Success in life is pretty much determined by forces outside of our control [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Everyone has it in their own power to succeed</p>"
            ),
            plotOutput("q25l_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Too much power is concentrated in the hands of a few large companies [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The largest companies do NOT have too much power</p>"
            ),
            plotOutput("q25m_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <li>Business corporations make too much profit [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Most corporations make a fair and reasonable amount of profit</p>"
            ),
            plotOutput("q25n_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <li>Elected officials in Washington lose touch with the people pretty quickly [OR]</li>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Elected officials in Washington try hard to stay in touch with voters back home</p>"
            ),
            plotOutput("q25o_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <li>Most elected officials care what people like me think [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Most elected officials don't care what people like me think</p>"
            ),
            plotOutput("q25p_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #1</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #2</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Neither/Both equally <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B26 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And in your view, has this country been successful more because of its <strong>[INSERT ITEM; RANDOMIZE] </strong>or more because of its<strong> [ITEM]? </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Ability to change [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Reliance on long-standing principles</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("qb26_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C26 Next, <strong>[READ AND RANDOMIZE] </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>Americans are united and in agreement about the most important values </strong>[OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>Americans are greatly divided when it comes to the most important values</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("qc26_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>OFTVOTE&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; How often would you say you vote...<strong>[READ IN ORDER]?</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Always</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Nearly always</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Part of the time</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Seldom</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Never vote</p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Other</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("oftvote_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK PHASE A FORM 1:</strong></p>
                  <p>Now a different kind of question,</p>
                  <p>Q.26F1 Thinking about how Barack Obama and Republican leaders should address the most important issues facing the country. Imagine a scale from zero to 100 where 100 means Republican leaders get everything they want and Obama gets nothing he wants, and zero means Obama gets everything and Republican leaders get nothing.&nbsp;Where on this scale from zero to 100 do you think they should end up? <strong>[OPEN END ENTER NUMBER 0-100] [IF NECESSARY: </strong>&ldquo;100 means Republicans get everything they want, ZERO means Obama gets everything he wants, about where, from 0 to 100 should they end up?<strong>]</strong> <strong>[INTERVIEWER, IF RESPONDENT STRUGGLES WITH PRECISE NUMBER YOU CAN SAY: </strong>&ldquo;you can just give me a number close to what you think&rdquo;<strong>] <br /> </strong><strong><br /> [IF RESPONDENT SAYS A NUMBER BETWEEN 0-49, CLARIFY: </strong>&ldquo;Just to be sure I get this right, <strong>[INSERT NUMBER CHOSEN]</strong>, means Obama should get more than Republican leaders, is that what you meant?&rdquo; <strong>[IF NO, RESPONDENT MEANT REP LEADERS SHOULD GET MORE:</strong> &ldquo;A number between 51 and 100 would mean Republican leaders get more than Obama&rdquo;<strong>]<br /> <br /> [IF RESPONDENT SAYS A NUMBER BETWEEN 51-100, CLARIFY: </strong>&ldquo;Just to be sure I get this right, <strong>[INSERT NUMBER CHOSEN]</strong>, means Republican leaders should get more than Obama, is that what you meant?&rdquo; <strong>[IF NO, RESPONDENT MEANT OBAMA SHOULD GET MORE:</strong> &ldquo;A number between 0 and 49 would mean Obama gets more than Republican leaders&rdquo;<strong>] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[ENTER NUMBER 0-100]</strong></p>
                  <p>999&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("q26f1_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK PHASE A FORM 2:</strong></p>
                  <p>Now a different kind of question,</p>
                  <p>Q.26F2 Thinking about how Barack Obama and Republican leaders should address the most important issues facing the country. Imagine a scale from zero to 100 where 100 means Obama gets everything he wants and Republican leaders get nothing they want, and zero means Republican leaders get everything and Obama gets nothing.&nbsp;Where on this scale from zero to 100 do you think they should end up? [<strong>OPEN END ENTER NUMBER 0-100] [IF NECESSARY: </strong>&ldquo;100 means Obama gets everything he wants, ZERO means Republicans gets everything they want, about where, from 0 to 100 should they end up?<strong>]</strong> <strong>[INTERVIEWER, IF RESPONDENT STRUGGLES WITH PRECISE NUMBER YOU CAN SAY: </strong>&ldquo;you can just give me a number close to what you think&rdquo;<strong>] <br /> <br /> [IF RESPONDENT SAYS A NUMBER BETWEEN 51-100, CLARIFY: </strong>&ldquo;Just to be sure I get this right, <strong>[INSERT NUMBER CHOSEN]</strong>, means Obama should get more than Republican leaders, is that what you meant?&rdquo; <strong>[IF NO, RESPONDENT MEANT REP LEADERS SHOULD GET MORE:</strong> &ldquo;A number between 0 and 49 would mean Republican leaders get more than Obama&rdquo;<strong>]<br /> <br /> [IF RESPONDENT SAYS A NUMBER BETWEEN 0-49, CLARIFY: </strong>&ldquo;Just to be sure I get this right, <strong>[INSERT NUMBER CHOSEN]</strong>, means Republican leaders should get more than Obama, is that what you meant?&rdquo; <strong>[IF NO, RESPONDENT MEANT OBAMA SHOULD GET MORE:</strong> &ldquo;A number between 51 and 100 would mean Obama gets more than Republican leaders&rdquo;<strong>] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[ENTER NUMBER 0-100]</strong></p>
                  <p>999&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("q26f2_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>The next congressional elections will be coming up later this year&hellip;</p>
                  <p>Q.B27 If the elections for U.S. Congress were being held TODAY, would you vote for the Republican Party&rsquo;s candidate or the Democratic Party&rsquo;s candidate for Congress in your district?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Republican Party&rsquo;s candidate</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Democratic Party&rsquo;s candidate</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Other<strong> (VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb27_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF &lsquo;OTHER&rsquo; &lsquo;DON&rsquo;T KNOW/REFUSED&rsquo; (Q.B27=3,9):</strong></p>
                  <p>Q.B27a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; As of TODAY, would you LEAN more to the Republican or the Democrat?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Republican Party&rsquo;s candidate</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Democratic Party&rsquo;s candidate</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Other<strong> (VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb27a_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Next,</p>
                  <p>INT1&nbsp;&nbsp; Do you use the internet, at least occasionally?</p>
                  <p>&nbsp;</p>
                  <ul>
                  <li>Yes</li>
                  <li>No</li>
                  </ul>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t Know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("int1_plot", height = 350),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK IF DOES NOT USE THE INTERNET (INT1=2,9):</strong></p>
                  <p>INT2&nbsp;&nbsp; Do you send or receive email, at least occasionally?</p>
                  <p>&nbsp;</p>
                  <ul>
                  <li>Yes</li>
                  <li>No</li>
                  </ul>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t Know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("int2_plot", height = 350),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF DOES NOT USE THE INTERNET OR EMAIL (INT2=2,9):</strong></p>
                  <p>INT3M&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you access the internet on a cell phone, tablet or other mobile handheld device, at least occasionally?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("int3m_plot", height = 350)
          )
        ))),
      tabItem(
        tabName = "survey2",
        fluidPage(fluidRow(
          box(
            width = 12,
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Now, I have a short set of questions on marriage and family.</p>
                  <p>Q.C28 First, how do you think you would react if a member of your immediate family told you they were going to marry... [<strong>INSERT ITEM; RANDOMIZE]</strong>? Would you be generally happy about this, generally unhappy, or wouldn&rsquo;t it matter to you at all? What about <strong>[NEXT ITEM]? [IF NECESSARY: </strong>if a member of your immediate family told you they were going to marry <strong>[ITEM]</strong>, would you be generally happy, generally unhappy, or wouldn&rsquo;t it matter to you at all?] [<strong>INTERVIEWER INSTRUCTION: IF RESPONDENT SAYS THEY HAVE FAMILY MEMBER(S) MARRIED TO SOMEONE OF THAT GROUP]:</strong> &ldquo;Are you happy about that, unhappy about that, or doesn&rsquo;t it matter?&rdquo;]</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>A Republican</li>"
            ),
            plotOutput("qc28a_plot", height = 250),
            HTML("
                     <li>A Democrat</li>"),
            plotOutput("qc28b_plot", height = 250),
            HTML("
                     <li>Someone who didn&rsquo;t go to college</li>"),
            plotOutput("qc28c_plot", height = 250),
            HTML("
                     <li>Someone born and raised outside the U.S.</li>"),
            plotOutput("qc28d_plot", height = 250),
            HTML("
                     <li>Someone who does not believe in God</li>"),
            plotOutput("qc28e_plot", height = 250),
            HTML("
                     <li>A &ldquo;born again&rdquo; Christian</li>"),
            plotOutput("qc28f_plot", height = 250),
            HTML("
                     <li>A gun owner</li>"),
            plotOutput("qc28g_plot", height = 250),
            HTML("
                     <li>Someone of a different race</li>"),
            plotOutput("qc28h_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Happy</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Unhappy</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Wouldn&rsquo;t it matter to you at all</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.A29 Thinking about some news organizations, would you say your overall opinion of&hellip; <strong>[INSERT ITEM; RANDOMIZE]</strong> is favorable, unfavorable, or neither in particular? How about <strong>[NEXT ITEM]</strong>? <strong>[IF NECESSARY</strong>: Is your overall opinion of <strong>[ITEM] </strong>favorable, unfavorable, or neither in particular<strong>]</strong></p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>MSNBC cable news</li>"
            ),
            plotOutput("qa29a_plot", height = 250),
            HTML("
                     <li>The Fox News Cable Channel</li>"),
            plotOutput("qa29b_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Favorable</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Unfavorable</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Neither in particular</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 30-39</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.40&nbsp;&nbsp; Would you say you follow what's going on in government and public affairs...<strong>[READ]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Most of the time</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some of the time</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Only now and then [OR]</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Hardly at all</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("q40_plot", height = 250),
            HTML(
              "
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B40a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some people say they are basically content with the federal government, others say they are frustrated, and others say they are angry. Which of these best describes how you feel?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Basically content</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Frustrated</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Angry</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb40a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B40b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; How much of the time do you think you can trust the government in Washington to do what is right? Just about always, most of the time, or only some of the time?</p>
                  <p><em>&nbsp;</em></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Just about always</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Most of the time</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Only some of the time</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Never <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb40b_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Just as far as you know&hellip;</p>
                  <p>Q.41&nbsp;&nbsp; Which political party has a majority in the U.S. House of Representatives <strong>[READ AND RANDOMIZE]</strong>? <strong>[IF</strong><strong> NECESSARY:</strong> Just as far as you know<strong>] [INTERVIEWER INSTRUCTION: DO NOT PROBE, PUNCH 9 IF RESPONDENT SAYS THEY DON&rsquo;T KNOW]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Republican Party [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Democratic Party</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("q41_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.42&nbsp;&nbsp; Which political party, has a majority in the U.S. Senate<strong> [READ AND RANDOMIZE]</strong>?<strong> [INTERVIEWER INSTRUCTION: DO NOT PROBE, PUNCH 9 IF RESPONDENT SAYS THEY DON&rsquo;T KNOW]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Republican Party [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Democratic Party</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("q42_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.43&nbsp;&nbsp; Which political party is more in favor of raising taxes on higher income people <strong>[READ AND RANDOMIZE]</strong>? <strong>[IF NECESSARY:</strong> Just as far as you know<strong>] [INTERVIEWER INSTRUCTION: DO NOT PROBE, PUNCH 9 IF RESPONDENT SAYS THEY DON&rsquo;T KNOW]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Republican Party [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Democratic Party</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("q43_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 44-47</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RANDOMIZE Q.C48/Q.C49</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Now some questions about your views of the political parties&hellip;</p>
                  <p>Q.C48 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think the Republican Party <strong>[INSERT ITEM; RANDOMIZE]</strong> or not?</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>Is too extreme</li>"
            ),
            plotOutput("qc48a_plot", height = 250),
            HTML("
                     <li>Cares about the middle class</li>"),
            plotOutput("qc48b_plot", height = 250),
            HTML(
              "
                  <li>Is too willing to cut government programs, even when they work</li>"
            ),
            plotOutput("qc48c_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, describes Republican Party</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>RANDOMIZE Q.C48/Q.C49</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>[Next,]</p>
                  <p>Q.C49 Do you think the Democratic Party <strong>[INSERT ITEM; RANDOMIZE] </strong>or not?</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>Is too extreme</li>"
            ),
            plotOutput("qc49a_plot", height = 250),
            HTML("<li>Cares about the middle class</li>"),
            plotOutput("qc49b_plot", height = 250),
            HTML(
              "
                  <li>Too often sees government as the only way to solve problems</li>"
            ),
            plotOutput("qc49c_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, describes Democratic Party</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.50&nbsp;&nbsp; Now I'm going to read a few more pairs of statements. Again, just tell me whether the FIRST statement or the SECOND statement comes closer to your own views &mdash; even if neither is exactly right. The first pair is <strong>[READ AND RANDOMIZE ITEMS Q THRU Z FOLLOWED BY RANDOMIZED ITEMS AA THRU HH; RANDOMIZE PAIRS BUT NOT STATEMENTS WITHIN EACH PAIR]. </strong>Next, <strong>[NEXT PAIR] [IF NECESSARY: </strong>&ldquo;Which statement comes closer to your views, even if neither is exactly right?&rdquo;]</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>This country should do whatever it takes to protect the environment [OR]</li>
                  </ol>
                  <p>This country has gone too far in its efforts to protect the environment</p>"
            ),
            plotOutput("q50q_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <ol>
                  <li>Stricter environmental laws and regulations cost too many jobs and hurt the economy [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Stricter environmental laws and regulations are worth the cost</p>"
            ),
            plotOutput("q50r_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <ol>
                  <li>There are no real limits to growth in this country today [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; People in this country should learn to live with less</p>"
            ),
            plotOutput("q50s_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <ol>
                  <li>As Americans, we can always find ways to solve our problems and get what we want [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; This country can't solve many of its important problems</p>"
            ),
            plotOutput("q50t_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <ol>
                  <li>Homosexuality should be accepted by society [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Homosexuality should be discouraged by society</p>"
            ),
            plotOutput("q50u_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <ol>
                  <li>It&rsquo;s not the government&rsquo;s job to protect people from themselves [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Sometimes laws to protect people from themselves are necessary</p>"
            ),
            plotOutput("q50v_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <ol>
                  <li>Religion is a very important part of my life [OR]</li>
                  </ol>
                  <p>Religion is not that important to me</p>"
            ),
            plotOutput("q50w_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>NO ITEM x</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <ol>
                  <li>I'm generally satisfied with the way things are going for me financially [OR]</li>
                  </ol>
                  <p>I'm not very satisfied with my financial situation</p>"
            ),
            plotOutput("q50y_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <ol>
                  <li>I often don't have enough money to make ends meet [OR]</li>
                  </ol>
                  <p>Paying the bills is generally not a problem for me</p>"
            ),
            plotOutput("q50z_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <ol>
                  <li>It IS NOT necessary to believe in God in order to be moral and have good values [OR]</li>
                  </ol>
                  <p>It IS necessary to believe in God in order to be moral and have good values</p>"
            ),
            plotOutput("q50aa_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <ol>
                  <li>Using overwhelming military force is the best way to defeat terrorism around the world [OR]</li>
                  </ol>
                  <p>Relying too much on military force to defeat terrorism creates hatred that leads to more terrorism</p>"
            ),
            plotOutput("q50bb_plot", height = 250),
            HTML(
              "
                  <p><em>&nbsp;</em></p>
                  <p><strong>NO ITEM cc</strong></p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>The growing number of newcomers from other countries threatens traditional American customs and values [OR]</li>
                  </ol>
                  <p>The growing number of newcomers from other countries strengthens American society</p>"
            ),
            plotOutput("q50dd_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <ol>
                  <li>It&rsquo;s best for the future of our country to be active in world affairs [OR]</li>
                  </ol>
                  <p>We should pay less attention to problems overseas and concentrate on problems here at home</p>"
            ),
            plotOutput("q50ee_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <ol>
                  <li>Americans need to be willing to give up privacy and freedom in order to be safe from terrorism [OR] Americans shouldn&rsquo;t have to give up privacy and freedom in order to be safe from terrorism</li>"
            ),
            plotOutput("q50ff_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <ol>
                  <li>The government should do more to protect morality in society [OR]</li>
                  </ol>
                  <p>I worry the government is getting too involved in the issue of morality</p>"
            ),
            plotOutput("q50gg_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <ol>
                  <li>Our country has made the changes needed to give blacks equal rights with whites [OR]</li>
                  </ol>
                  <p>Our country needs to continue making changes to give blacks equal rights with whites</p>"
            ),
            plotOutput("q50hh_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #1</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #2</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Neither/Both equally <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.51&nbsp;&nbsp; Next, <strong>[ASK ITEM ii FIRST, FOLLOWED BY RANDOMIZED ITEMS jj THROUGH mm <u>AND </u>RANDOMIZE STATEMENTS WITHIN PAIRS]</strong>. <strong>[IF NECESSARY: </strong>&ldquo;Which statement comes closer to your views, even if neither is exactly right?&rdquo;<strong>] </strong>Next, <strong>[NEXT PAIR]</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <ol>
                  <li>Government should do more to solve problems [OR]</li>
                  </ol>
                  <p>Government is doing too many things better left to businesses and individuals</p>"
            ),
            plotOutput("q51ii_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <ol>
                  <li>Children are better off when a parent stays home to focus on the family</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Children are just as well off when their parents work outside the home</p>"
            ),
            plotOutput("q51jj_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <ol>
                  <li>Government aid to the poor does more harm than good, by making people too dependent on government assistance [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Government aid to the poor does more good than harm, because people can&rsquo;t get out of poverty until their basic needs are met</p>"
            ),
            plotOutput("q51kk_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <ol>
                  <li>The economic system in this country unfairly favors powerful interests [OR]</li>
                  </ol>
                  <p>The economic system in this country is generally fair to most Americans</p>"
            ),
            plotOutput("q51ll_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <ol>
                  <li>I like elected officials who make compromises with people they disagree with [OR]</li>
                  </ol>
                  <p>I like elected officials who stick to their positions</p>"
            ),
            plotOutput("q51mm_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO ITEM nn</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <ol>
                  <li>The police should be allowed to stop and search anyone who fits the general description of a crime suspect [OR]</li>
                  </ol>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The police should not be able to search people just because they think they look suspicious</p>"
            ),
            plotOutput("q51oo_plot", height = 250),
            HTML(
              "
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <ol>
                  <li>Wall Street HELPS the American economy more than it hurts [OR]</li>
                  </ol>
                  <p>Wall Street HURTS the American economy more than it helps</p>"
            ),
            plotOutput("q51pp_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #1</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #2</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Neither/Both equally <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTION 52</strong></p>"
            )
          )
        ))),
      tabItem(
        tabName = "survey3",
        fluidPage(fluidRow(
          box(
            width = 12,
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.53&nbsp;&nbsp; In your opinion, which is generally more often to blame if a person is poor? Lack of effort on his or her own part, or circumstances beyond his or her control?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Lack of effort</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Circumstances beyond control</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Both <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q53_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B54 Next, <strong>[IF NECESSARY: </strong>Which comes closer to your own views &mdash; even if neither is exactly right<strong>]</strong>. <strong>[READ DO NOT RANDOMIZE STATEMENTS]</strong></p>
                  <p>&nbsp;</p>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Islamic religion is more likely than others to encourage violence among its believers [OR]</p>
                  <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The Islamic religion does not encourage violence more than others</p>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #1</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Statement #2</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Neither/Both equally <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb54_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B55 Should the U.S. Supreme Court base its rulings on its understanding of what the U.S. Constitution meant as it was originally written, or should the court base its rulings on its understanding of what the US Constitution means in current times?</p>
                  <p><em>&nbsp;</em></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What it meant as originally written</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What it means in current times</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Somewhere in between <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb55_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C56 Which of these statements best describes your opinion about the United States? <strong>[READ IN ORDER; REVERSE ORDER FOR HALF OF SAMPLE] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The U.S. stands above all other countries in the world.</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The U.S. is one of the greatest countries in the world, along with some others [OR]</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; There are other countries that are better than the U.S.</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qc56_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C57 From what you&rsquo;ve read and heard, is there solid evidence that the average temperature on earth has been getting warmer over the past few decades, or not?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Mixed/some evidence <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc57_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK IF EARTH IS GETTING WARMER (Q.C57=1):</strong></p>
                  <p>Q.C58a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you believe that the earth is getting warmer <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Mostly because of human activity such as burning fossil fuels [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Mostly because of natural patterns in the earth&rsquo;s environment</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qc58a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF EARTH IS NOT GETTING WARMER (Q.C57=2):</strong></p>
                  <p>Q.C58b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think that we just don&rsquo;t know enough yet about whether the Earth is getting warmer or do you think it&rsquo;s just not happening?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Just don&rsquo;t know enough yet</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Just not happening</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc58b_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 59-99</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.100 Have you ever contributed money to a candidate running for public office or to a group working to elect a candidate?</p>
                  <p>&nbsp;</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q100_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK IF HAVE EVER CONTRIBUTED MONEY (Q.100=1):</strong></p>
                  <p>Q.101 Have you done this over the last two years, that is, during or since the 2012 elections, or not? <strong>[IF NECESSARY:</strong> Have you contributed money to any candidates or political groups over the last two years, or not?<strong>]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q101_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF HAVE CONTRIBUTED MONEY DURING 2012/2013 (Q.101=1):</strong></p>
                  <p>Q.102 Over the last two years, would you say all of those contributions added up to more than $100 or less than that?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; More than $100</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Less than that</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q102_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF MORE THAN $100 (Q.102=1):</strong></p>
                  <p>Q.102a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And did they add up to more than $250 or not?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; More than $250</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, less than that</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q102a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>NO QUESTIONS 103-104</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.105 And again, just thinking about the last two years&hellip;Please tell me if you have done any of the following. First, over the last two years have you <strong>[INSERT ITEM; RANDOMIZE], </strong>or not? And over the last two years have you <strong>[INSERT NEXT ITEM</strong>], or not?</p>
                  <p>&nbsp;</p>
                  <ol>
                  <li>Worked or volunteered for a political candidate or campaign</li>"
            ),
            plotOutput("q105a_plot", height = 250),
            HTML(
              "
                  <li>Contacted any elected official</li>"
            ),
            plotOutput("q105b_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p><strong>NO ITEM C</strong></p>
                  <ol>
                  <li>Attended a campaign event</li>"
            ),
            plotOutput("q105d_plot", height = 250),
            HTML(
              "
                  </ol>
                  <p>&nbsp;</p>
                  <p><strong>RESPONSE CATEGORIES:</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, have done this within the last 2 years</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, have not done this within the last 2 years</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.106 And have you, yourself, ever run for federal, state, or local elected office, or not?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, have run for elected office</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, have not run for elected office</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q106_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Next,</p>
                  <p>Q.B106&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you favor or oppose legalized casino gambling in your state?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Favor</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Oppose&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb106_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B107&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Right now, which ONE of the following do you think should be the more important priority for addressing America&rsquo;s energy supply? <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Developing alternative sources, such as wind, solar and hydrogen technology [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Expanding exploration and production of oil, coal and natural gas</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Both should be given equal priority</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qb107_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B108&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you strongly favor, favor, oppose, or strongly oppose allowing gays and lesbians to marry legally?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Strongly favor</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Favor</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Oppose</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Strongly oppose</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb108_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B109&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Thinking about our economic and trade policy toward China, which is more important <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p><em>&nbsp;</em></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Building a stronger relationship with China on economic issues [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Getting tougher with China on economic issues</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qb109_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B110&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think the use of marijuana should be made legal, or not?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, legal</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, illegal</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qb110_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C111&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; How much, if anything, have you read or heard about COMMON CORE, a set of education standards for students in grades K-12? Have you heard &hellip; <strong>[READ] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A lot</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A little</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Nothing at all</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qc111_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF HEARD OF COMMON CORE (Q.C111=1,2):</strong></p>
                  <p>Q.C112&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; From what you&rsquo;ve read and heard, do you strongly favor, favor, oppose or strongly oppose the Common Core education standards?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Strongly favor</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Favor</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Oppose</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Strongly oppose</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc112_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 113-114</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C115&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; In general, do you think that free trade agreements between the U.S. and other countries have been a good thing or a bad thing for the United States?</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Good thing</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Bad thing</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc115_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C116&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you approve or disapprove of the health care law passed by Barack Obama and Congress in 2010?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Approve</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Disapprove</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc116_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF APPROVE OR DISAPPROVE (Q.C116=1,2)</strong></p>
                  <p>Q.C116a Do you <strong>[</strong>approve/disapprove<strong>] </strong>very strongly, or not so strongly?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very strongly</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not so strongly</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc116a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF &lsquo;DISAPPROVE&rsquo; (Q.C116=2):</strong></p>
                  <p>Q.C117 What do you think elected officials who oppose the health care law should do now that the law has started to take effect? Should they <strong>[READ AND RANDOMIZE] </strong>or should they <strong>[ITEM]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do what they can to make the law work as well as possible</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do what they can to make the law fail</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("qc117_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 118-120</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RANDOMIZE IN BLOCKS:</strong></p>
                  <p><strong>Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.121 Do you think it is the responsibility of the federal government to make sure all Americans have health care coverage, or is that not the responsibility of the federal government?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, government responsibility</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, not government responsibility</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q121_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF GOVERNMENT RESPONSIBILITY (Q121=1):</strong></p>
                  <p>Q.121a<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </strong>S<strong>hould health insurance</strong> <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Be provided through a single national health insurance system run by the government [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Continue to be provided through a mix of private insurance companies and government programs</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("q121a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF NOT GOVERNMENT RESPONSIBILITY (Q121=2):</strong></p>
                  <p>Q.121b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>Should the government</strong><strong> [</strong><strong>READ AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not be involved in providing health insurance at all [OR SHOULD THE GOVERNMENT]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Continue programs like Medicare and Medicaid for seniors and the very poor</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("q121b_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RANDOMIZE IN BLOCKS:</strong></p>
                  <p><strong>Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.122 Which comes closer to your view about how to handle immigrants who are now living in the U.S. illegally? Should they <strong>[READ AND RANDOMIZE] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not be eligible for citizenship [OR SHOULD THEY]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Be eligible for citizenship if they meet certain requirements</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("q122_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK IF NOT ELIGIBLE FOR CITIZENSHIP (Q122=1):</strong></p>
                  <p>Q.122a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think there should be a national law enforcement effort to deport all immigrants who are now living in the U.S. illegally, or should that not be done?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should be national law enforcement effort to deport</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should not be national law enforcement effort to deport</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q122a_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK IF BE ELIGIBLE FOR CITIZENSHIP (Q122=2):</strong></p>
                  <p>Q.122b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And if immigrants meet these requirements, should they be eligible for citizenship? <strong>[READ AND RANDOMIZE] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Right away [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Only after a period of time</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q122b_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>RANDOMIZE IN BLOCKS:</strong></p>
                  <p><strong>Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.123 What do you think is more important &ndash; to protect the right of Americans to own guns, OR to control gun ownership?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Protect the right of Americans to own guns</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Control gun ownership</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q123_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF MORE IMPORTANT TO PROTECT OWNERSHIP (Q.123=1):</strong></p>
                  <p>Q.123a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And do you think there should be <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some restrictions on gun ownership [OR SHOULD THERE BE]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No restrictions on gun ownership</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q123a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF MORE IMPORTANT TO CONTROL OWNERSHIP (Q.123=2):</strong></p>
                  <p>Q.123b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And do you think <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Most Americans should be able to own guns with certain limits in place [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Only law enforcement and security personnel should be able to own guns</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q123b_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.124 Do you think abortion should be <strong>[READ AND RANDOMIZE]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; LEGAL in all or most cases [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ILLEGAL in all or most cases</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don't know/Refused</p>"
            ),
            plotOutput("q124_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF LEGAL IN ALL/MOST (q124=1):</strong></p>
                  <p>Q.124a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think there are any situations in which abortion should be restricted, or should there be no restrictions at all on abortion?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Situations in which abortion should be restricted</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No restrictions at all on abortion</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q124a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF ILLEGAL IN ALL/MOST (q124=2):</strong></p>
                  <p>Q.124b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think there are any situations in which abortion should be allowed, or should there be no situations at all where abortion is allowed?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Situations in which abortion should be allowed</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No situations where abortion should be allowed</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q124b_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RANDOMIZE IN BLOCKS:</strong></p>
                  <p><strong>Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>Q.125 Thinking about the long term future of Social Security, do you think <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p><strong>&nbsp;</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some reductions in benefits for future retirees need to be considered [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Social Security benefits should not be reduced in any way</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("q125_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF ACCEPTABLE (Q.125=1):</strong></p>
                  <p>Q.125a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should Social Security be <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p><strong>&nbsp;</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Phased out as a government program [OR SHOULD IT BE]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Maintained at a reduced level</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don't know/Refused</p>"
            ),
            plotOutput("q125a_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF UNACCEPTABLE (Q.125=2):</strong></p>
                  <p>Q.125b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should Social Security <strong>[READ AND RANDOMIZE]</strong>?</p>
                  <p><strong>&nbsp;</strong></p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Cover more people, with greater benefits [OR SHOULD IT]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Be kept about as it is</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don't know/Refused</p>"
            ),
            plotOutput("q125b_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>RANDOMIZE IN BLOCKS:</strong></p>
                  <p><strong>Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Q.126 Overall, do you approve or disapprove of the government&rsquo;s collection of telephone and internet data as part of anti-terrorism efforts?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Approve</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Disapprove</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q126_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK PHASE A IF APPROVE (Q.126=1):</strong></p>
                  <p>Q.126a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think the National Security Agency should be allowed to collect whatever data it needs, or should there be limits on what it collects?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; NSA should be allowed to collect whatever data it needs</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Should be limits on what NSA collects</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q126a_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK PHASE A IF DISAPPROVE (Q.126=2):</strong></p>
                  <p>Q.126b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you think the National Security Agency should be prevented from collecting any data about U.S. citizens, or should it be allowed to collect some limited information?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; NSA prevented from collecting any data on citizens</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; NSA should be allowed to collect some limited information</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("q126b_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C127 In general, do you think affirmative action programs designed to increase the number of black and minority students on college campuses are a good thing or a bad thing?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Good thing</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Bad thing</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc127_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Q.C128&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Do you favor or oppose building the Keystone XL pipeline that would transport oil from Canada&rsquo;s oil sands region through the Midwest to refineries in Texas?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Favor</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Oppose</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("qc128_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>NO QUESTIONS 129-134</strong></p>
                  "
            )
          )
        ))),
      tabItem(
        tabName = "survey4",
        fluidPage(fluidRow(
          box(
            width = 12,
            HTML(
              "   <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE C:</strong></p>
                  <p>Next,</p>
                  <p>Q.C135&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Which comes closer to your view? <strong>[READ AND RANDOMIZE]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Humans and other living things have evolved over time <strong>[OR]</strong></p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Humans and other living things have existed in their present</p>
                  <p>form since the beginning of time</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qc135_plot", height = 250),
            HTML(
              "   <p>&nbsp;</p>
                  <p><strong>ASK IF EVOLVED (Q.C135=1):</strong></p>
                  <p>Q.C135a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And do you think that&hellip;<strong>[READ OPTIONS AND RANDOMIZE]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Humans and other living things have evolved due to natural processes such as natural selection, OR</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A supreme being guided the evolution of living things for the purpose of creating humans and other life in the form it exists today</p>
                  <p>9 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qc135a_plot", height = 250),
            HTML(
              "   <p><strong>&nbsp;</strong></p>
                  <p><strong>NO QUESTIONS 136-138</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>Now, just a few questions for statistical purposes only.</p>
                  <p>SEX&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[ENTER RESPONDENT'S SEX:] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Male</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Female</p>"
            ),
            plotOutput("sex_plot1", height = 250),
            HTML(
              "   <p>&nbsp;</p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>AGE&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What is your age?</p>
                  <p>&nbsp;</p>
                  <p>________ years</p>
                  <p>97&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 97 or older</p>
                  <p>99 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("age_plot1", height = 250),
            HTML(
              "   <p><strong>ASK ALL:</strong></p>
                  <p>EDUC&nbsp; What is the highest level of school you have completed or the highest degree you have received? <strong>[DO NOT READ] [INTERVIEWER NOTE: </strong>Enter code 3-HS grad if R completed training that did NOT count toward a degree<strong>] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Less than high school (Grades 1-8 or no formal schooling)</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; High school incomplete (Grades 9-11 or Grade 12 with NO diploma)</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; High school graduate (Grade 12 with diploma or GED certificate)</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some college, no degree (includes community college)</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Two year associate degree from a college or university</p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Four year college or university degree/Bachelor&rsquo;s degree (e.g., BS, BA, AB)</p>
                  <p>7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)</p>
                  <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Postgraduate or professional degree, including master&rsquo;s, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD, graduate school)</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>[MAKE FULL NOTE AVAILABLE FOR INTERVIEWERS: </strong>Enter code 3-HS graduate&rdquo; if R completed vocational, business, technical, or training courses after high school that did NOT count toward an associate degree from a college, community college or university (e.g., training for a certificate or an apprenticeship)<strong>]</strong></p>"
            ),
            plotOutput("educ_plot1", height = 500),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>HISP&nbsp;&nbsp; Are you of Hispanic, Latino, or Spanish origin, such as Mexican, Puerto Rican or Cuban?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("hisp_plot1", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>RACE&nbsp; Which of the following describes your race? You can select as many as apply. White, Black or African American, Asian or Asian American or some other race. <strong>[RECORD UP TO FOUR IN ORDER MENTIONED BUT DO NOT PROBE FOR ADDITIONAL]</strong> <strong>[IF R VOLS MIXED BIRACIAL, PROBE ONCE: </strong>What race or races is that?<strong>] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Black or African-American (e.g., Negro, Kenyan, Nigerian, Haitian)</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Asian or Asian-American (e.g., Asian Indian, Chinese, Filipino, Vietnamese or other Asian origin groups)</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some other race <strong>(SPECIFY____ IF NEEDED: </strong>What race or races is that?<strong>)</strong></p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Native American/American Indian/Alaska Native <strong>(VOL.)</strong></p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pacific Islander/Native Hawaiian <strong>(VOL.)</strong></p>
                  <p>7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Hispanic/Latino <strong>(VOL.)</strong> (e.g., Mexican, Puerto Rican, Cuban)</p>
                  <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know <strong>(VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Refused (e.g., non-race answers like American, Human, purple) <strong>(VOL.)</strong></p>"
            ),
            plotOutput("race_plot1", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF HISPANIC (HISP=1 OR RACE=7):</strong></p>
                  <p>BIRTH_HISP&nbsp; Were you born in the United States, on the island of Puerto Rico, or in another country?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; U.S.</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Puerto Rico</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Another country</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("birth_hisp_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF NOT HISPANIC (HISP=2,9 AND RACE&ne;7):</strong></p>
                  <p>USBORN &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Were you born in the United States or in another country?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, born in U.S.</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, some other country</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Puerto Rico <strong>(VOL.)</strong></p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Other U.S. Territories (includes Guam, Samoa, U.S. Virgin Islands)<strong> (VOL.)</strong></p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("usborn_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>MARITAL&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you currently married, living with a partner, divorced, separated, widowed, or have you never been married? <strong>[IF R SAYS &ldquo;SINGLE,&rdquo; PROBE TO DETERMINE WHICH CATEGORY IS APPROPRIATE] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Married</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Living with a partner</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Divorced</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Separated</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Widowed</p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Never been married</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("marital_plot1", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL PHASE A:</strong></p>
                  <p>PARENT Are you the parent or guardian of any children under 18 now living in your household?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("parent_plot1", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF NOT BORN IN US, PUERTO RICO OR US TERRITORIES (BIRTH_HISP=3,9 OR USBORN=2,9):</strong></p>
                  <p>CITIZEN&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you a citizen of the United States, or not?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("citizen_plot1", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>RELIG What is your present religion, if any? Are you Protestant, Roman Catholic, Mormon, Orthodox such as Greek or Russian Orthodox, Jewish, Muslim, Buddhist, Hindu, atheist, agnostic, something else, or nothing in particular?</p>
                  <p>&nbsp;</p>
                  <p><strong>[INTERVIEWER: IF R VOLUNTEERS &ldquo;nothing in particular, none, no religion, etc.&rdquo; BEFORE REACHING END OF LIST, PROMPT WITH: And would you say that&rsquo;s atheist, agnostic, or just nothing in particular?]</strong></p>
                  <p>&nbsp;</p>
                  <ul>
                  <li>Protestant (Baptist, Methodist, Non-denominational, Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, Church of Christ, Jehovah&rsquo;s Witness, etc.)</li>
                  </ul>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Roman Catholic (Catholic)</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Mormon (Church of Jesus Christ of Latter-day Saints/LDS)</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Orthodox (Greek, Russian, or some other orthodox church)</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Jewish (Judaism)</p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Muslim (Islam)</p>
                  <p>7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Buddhist</p>
                  <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Hindu</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Atheist (do not believe in God)</p>
                  <p>10&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Agnostic (not sure if there is a God)</p>
                  <p>11 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Something else <strong>(SPECIFY:______)</strong></p>
                  <p>12&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Nothing in particular</p>
                  <p>13&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Christian <strong>(VOL.)</strong></p>
                  <p>14&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Unitarian (Universalist) <strong>(VOL.)</strong></p>
                  <p>99&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't Know/Refused<strong> (VOL.)</strong></p>"
            ),
            plotOutput("relig_plot1", height = 600),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF SOMETHING ELSE OR DK/REF (RELIG=11, 99):</strong></p>
                  <p>CHR &nbsp;&nbsp; Do you think of yourself as a Christian or not? <strong>[IF R NAMED A NON-CHRISTIAN RELIGION IN PREVIOUS QUESTION (e.g. Native American, Wiccan, Pagan, etc.), DO NOT READ (ENTER NO CODE 2)] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("chr_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF CHRISTIAN (RELIG=1-4, 13 OR CHR=1):</strong></p>
                  <p>BORN&nbsp; Would you describe yourself as a born again or evangelical Christian, or not?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, would</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, would not</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("born_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>ATTEND&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Aside from weddings and funerals, how often do you attend religious services... more than once a week, once a week, once or twice a month, a few times a year, seldom, or never?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; More than once a week</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Once a week</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Once or twice a month</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A few times a year</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Seldom</p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Never</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            ),
            plotOutput("attend_plot1", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK ALL PHASE B:</strong></p>
                  <p>Q.B139&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Which comes closest to your view? <strong>[READ IN ORDER]</strong></p>
                  <p>&nbsp;</p>
                  <p><strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; [Holy book: If Christian or no religion (RELIG =1-4, 9, 10, 12, 13 OR CHR=1) insert &ldquo;the Bible&rdquo;; If Jewish (RELIG =5), insert &ldquo;the Torah&rdquo;; If Muslim (RELIG=6), insert, &ldquo;the Koran&rdquo;; If other non-Christian affiliations (RELIG=7,8,14 OR (RELIG=11 AND CHR=2,9)), insert &ldquo;the Holy Scripture&rdquo;; IF DK/REF IN RELIGION (RELIG=99) AND CHR=2,9, insert the Bible] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[Holy book]</strong> is the word of God, [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[Holy book]</strong> is a book written by men and is not the word of God.</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Other</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qb139_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK IF BELIEVE HOLY BOOK IS WORD OF GOD (Q.B139=1):</strong></p>
                  <p>Q.B139a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And would you say that <strong>[READ IN ORDER]</strong>?</p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[Holy book</strong>] is to be taken literally, word for word [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not everything in <strong>[Holy book</strong>] should be taken literally, word for word.</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Other</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            ),
            plotOutput("qb139a_plot", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong><br /> </strong></p>
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>INCOME&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Last year, that is in 2013, what was your total family income from all sources, before taxes? Just stop me when I get to the right category. <strong>[READ]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Less than $10,000</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 10 to under $20,000</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 20 to under $30,000</p>
                  <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 30 to under $40,000</p>
                  <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 40 to under $50,000</p>
                  <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 50 to under $75,000</p>
                  <p>7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 75 to under $100,000</p>
                  <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 100 to under $150,000 [OR]</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; $150,000 or more</p>
                  <p>10&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("income_plot1", height = 250),
            HTML(
              "
                  <p>&nbsp;</p>
                  <p><strong>ASK IF INCOME $150K+ (INCOME=9):</strong></p>
                  <p>INCHI&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; And was your total 2013 family income before taxes <strong>[READ] </strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Under $250,000 [or]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; $250,000 or more</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            ),
            plotOutput("inchi_plot", height = 250),
            HTML(
              "
                  <p><strong>&nbsp;</strong></p>
                  <p><strong>ASK ALL:</strong></p>
                  <p>REG&nbsp;&nbsp;&nbsp;&nbsp; Which of these statements best describes you? <strong>[READ IN ORDER] </strong><strong>[INSTRUCTION: BE SURE TO CLARIFY WHETHER RESPONDENT IS ABSOLUTELY CERTAIN THEY ARE REGISTERED OR ONLY PROBABLY REGISTERED; IF RESPONDENT VOLUNTEERS THAT THEY ARE IN NORTH DAKOTA AND DON&rsquo;T HAVE TO REGISTER, PUNCH 1]</strong></p>
                  <p>&nbsp;</p>
                  <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you ABSOLUTELY CERTAIN that you are registered to vote at your current address [OR]</p>
                  <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you PROBABLY registered, but there is a chance your registration has lapsed [OR]</p>
                  <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you NOT registered to vote at your current address</p>
                  <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            # ),
            # plotOutput("reg_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL:</strong></p>
            #       <p>PARTY&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; In politics TODAY, do you consider yourself a Republican, Democrat, or independent?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Republican</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Democrat</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Independent</p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No preference <strong>(VOL.)</strong></p>
            #       <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Other party <strong>(VOL.)</strong></p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.) </strong></p>"
            # ),
            # plotOutput("party_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK IF INDEP/NO PREF/OTHER/DK/REF (PARTY=3,4,5,9):</strong></p>
            #       <p>PARTYLN&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; As of today do you lean more to the Republican Party or more to the Democratic Party?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Republican</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Democrat</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Other/Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("partyln_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong><br /> </strong></p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK IF REPUBLICAN OR DEMOCRAT (PARTY=1,2): </strong></p>
            #       <p>PARTYSTR&nbsp;&nbsp;&nbsp;&nbsp; Do you consider yourself a STRONG <strong>[</strong>Republican/Democrat<strong>]</strong> or NOT a strong <strong>[</strong>Republican/Democrat<strong>]</strong>?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Strong</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Not strong</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know<strong>/</strong>Refused&nbsp;<strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("partystr_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL:</strong></p>
            #       <p>IDEO&nbsp; In general, would you describe your political views as... <strong>[READ]</strong></p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very conservative</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Conservative</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Moderate</p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Liberal [OR]</p>
            #       <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Very liberal</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            # ),
            # plotOutput("ideo_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS</strong></p>
            #       <p><strong>ASK IF NOT CURRENTLY DEMOCRAT (PARTY=1,3,4,5,9 AND REG=1):</strong></p>
            #       <p>Q.B140&nbsp; Has there ever been a time when you have thought of yourself as a DEMOCRAT, or not?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("qb140_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS</strong></p>
            #       <p><strong>ASK IF EVER THOUGHT OF SELF AS DEMOCRAT (Q.B140=1):</strong></p>
            #       <p>Q.B140b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What about in the past ten years, have you thought of yourself as a Democrat in the past ten years, or not?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("qb140b_plot", height = 250),
            # HTML(
            #   "
            #       <p>&nbsp;</p>
            #       <p><strong>RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS</strong></p>
            #       <p><strong>ASK NOT CURRENTLY REPUBLICAN (PARTY=2,3,4,5,9 AND REG=1):</strong></p>
            #       <p>Q.B141&nbsp; Has there ever been a time when you have thought of yourself as a REPUBLICAN, or not? {9-10 mod filter} {QID:qid20140301qb141}</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("qb141_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS</strong></p>
            #       <p><strong>ASK IF EVER THOUGHT OF SELF AS REPUBLICAN (Q.B141=1):</strong></p>
            #       <p>Q.B141b&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What about in the past ten years, have you thought of yourself as a Republican in the past ten years, or not?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("qb141b_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong><br /> </strong></p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL REGISTERED VOTERS (REG=1):</strong></p>
            #       <p>Q.C142&nbsp; <strong>Thinking about the elections you have voted in over the past several years, including national and statewide elections. Would you say you</strong><strong> [READ IN ORDER; REVERSE ORDER FOR RANDOM HALF OF SAMPLE]</strong>?</p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Always<strong> vote Republican</strong></p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>Usually vote Republican</strong></p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>Vote about equally for both parties</strong></p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Usually vote Democratic [OR]</p>
            #       <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Always vote Democratic</p>
            #       <p>7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Have never voted</p>
            #       <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t vote for either party/vote for other parties</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            # ),
            # plotOutput("qc142_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>NO QUESTIONS 143-147</strong></p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL REGISTERED VOTERS (REG=1):</strong></p>
            #       <p>Q.148 As you may know, primary elections, where parties select their nominees, take place in the months before general elections. Thinking about the primary elections for Congress this year, do you happen to know in what month your state&rsquo;s primary will be held? <strong>[OPEN END; SINGLE PUNCH; DO NOT READ, USE PRECODES, IF RESPONDENT IS NOT SURE, DO NOT PROBE, ENTER AS DON&rsquo;T KNOW] </strong></p>
            #       <p>&nbsp;</p>
            #       <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>PRECODES</strong></p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; January/February</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; March</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; April</p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; May</p>
            #       <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; June</p>
            #       <p>6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; July</p>
            #       <p>7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; August</p>
            #       <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; September</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; October/November/December</p>
            #       <p>99&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>"
            # ),
            # plotOutput("q148_plot", height = 250),
            # HTML(
            #   "
            #       <p>&nbsp;</p>
            #       <p><strong>ASK ALL REGISTERED VOTERS (REG=1):</strong></p>
            #       <p>Q.149 And how often would you say you vote in Congressional PRIMARY elections. Would you say you vote in Congressional primary elections <strong>[READ IN ORDER]</strong>?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Always</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Nearly always</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Part of the time [OR]</p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Seldom or never</p>
            #       <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Not registered with a party/Can&rsquo;t vote in primaries</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ]</strong> Don&rsquo;t know/Refused</p>"
            # ),
            # plotOutput("q149_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL:</strong></p>
            #       <p>TEAPARTY2&nbsp; From what you know, do you agree or disagree with the Tea Party movement, or don&rsquo;t you have an opinion either way?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Agree</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Disagree</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No opinion either way</p>
            #       <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Haven&rsquo;t heard of <strong>(VOL.)</strong></p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("teaparty2_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong><br /> </strong></p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK PHASE A IF AGREE WITH TEA PARTY (TEAPARTY2=1):</strong></p>
            #       <p>Q.150 Have you ever attended a Tea Party rally or meeting, or not? <strong>[IF YES:</strong> Was that in the last two years, or not?<strong>] </strong></p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, within the 2 years</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, but NOT within the last 2 years</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, but don&rsquo;t know if within last 2 years <strong>(VOL.)</strong></p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("q150_plot", height = 250),
            # HTML(
            #   "
            #       <p>&nbsp;</p>
            #       <p><strong>ASK ALL:</strong></p>
            #       <p>HH1&nbsp;&nbsp;&nbsp; How many people, including yourself, live in your household?</p>
            #       <p><strong>INTERVIEWER NOTE: HOUSEHOLD MEMBERS INCLUDE PEOPLE WHO THINK OF THIS HOUSEHOLD AS THEIR PRIMARY PLACE OF RESIDENCE, INCLUDING THOSE WHO ARE TEMPORARILY AWAY ON BUSINESS, VACATION, IN A HOSPITAL, OR AWAY AT SCHOOL</strong>. <strong>THIS INCLUDES INFANTS, CHILDREN AND ADULTS. </strong></p>
            #       <p>&nbsp;</p>
            #       <p>______&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Enter number 1-7</p>
            #       <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 8 or more</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused</p>"
            # ),
            # plotOutput("hh1_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK IF MORE THAN ONE PERSON IN HH (HH1&gt;1):</strong></p>
            #       <p>HH3&nbsp;&nbsp;&nbsp; How many, including yourself, are adults, age 18 and older?</p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p>______&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Enter number 1-7</p>
            #       <p>8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 8 or more</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused</p>"
            # ),
            # plotOutput("hh3_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL LANDLINE SAMPLE:</strong></p>
            #       <p>L1.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Now thinking about your telephone use&hellip; Do you have a working cell phone?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, have cell phone</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, do not</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("l1_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK IF NO CELL PHONE AND MULTI-PERSON HOUSEHOLD (L1=2,9 AND HH1&gt;1):</strong></p>
            #       <p>L1a.&nbsp;&nbsp;&nbsp;&nbsp; Does anyone in your household have a working cell phone?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes, someone in household has cell phone</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <ul>
            #       <li>Don't know/Refused <strong>(VOL.)</strong></li>"
            # ),
            # plotOutput("l1a_plot", height = 250),
            # HTML(
            #   "
            #       </ul>
            #       <p><strong>ASK ALL CELL PHONE SAMPLE:</strong></p>
            #       <p>C1.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Now thinking about your telephone use&hellip; Is there at least one telephone INSIDE your home that is currently working and is not a cell phone?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes home telephone</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No, home telephone</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>"
            # ),
            # plotOutput("c1_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK IF DUAL AND SINGLE-PERSON HOUSEHOLD ((L1=1 OR C1=1) AND HH1=1):</strong></p>
            #       <p>LC2.&nbsp;&nbsp;&nbsp; Of all the telephone calls that you receive, do you get <strong>[READ AND RANDOMIZE OPTIONS 1 AND 3&mdash;KEEP 2 ALWAYS IN THE MIDDLE]</strong>?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; All or almost all calls on a cell phone</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some on a cell phone and some on a regular home phone</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; All or almost all calls on a regular home phone</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            # ),
            # plotOutput("lc2_plot", height = 250),
            # HTML(
            #   "
            #       <p>&nbsp;</p>
            #       <p><strong>ASK IF DUAL AND MULTI-PERSON HOUSEHOLD ((L1=1 OR L1a=1 OR C1=1) AND HH1&gt;1):</strong></p>
            #       <p>LC3.&nbsp;&nbsp;&nbsp; Now thinking about all the people in your household, including yourself, of all the telephone calls that your household receives, are <strong>[READ AND </strong><strong>RANDOMIZE OPTIONS 1 AND 3&mdash;KEEP 2 ALWAYS IN THE MIDDLE]</strong>?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; All or almost all calls on a cell phone</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Some on a cell phone and some on a regular home phone</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; All or almost all calls on a regular home phone</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don&rsquo;t know/Refused</p>"
            # ),
            # plotOutput("lc3_plot", height = 250),
            # HTML(
            #   "
            #       <p><strong>&nbsp;</strong></p>
            #       <p><strong>ASK ALL:</strong></p>
            #       <p>ZIPCODE&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What is your zipcode?</p>
            #       <p>&nbsp;</p>
            #       <p>_____&nbsp; Enter Zipcode</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don&rsquo;t know/Refused</p>"
            # ),
            # # plotOutput("zipcode_plot", height = 250),
            # HTML(
            #   "
            #       <p>&nbsp;</p>
            #       <p><strong>ASK CELL PHONE SAMPLE:</strong></p>
            #       <p>MONEY We&rsquo;d like to send you $5 for your time. Can I please have your name and a mailing address where we can send you the money? <strong>[INTERVIEWER NOTE:</strong> If R does not want to give full name, explain we only need it so we can send the $5 to them personally.<strong>]</strong></p>
            #       <p>&nbsp;</p>
            #       <p>1<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; [ENTER FULL NAME] &ndash; INTERVIEWER: PLEASE VERIFY SPELLING, MAKE SURE TO GET BOTH FIRST AND LAST NAME</strong></p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[ENTER MAILING ADDRESS]</strong></p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[City]</strong></p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[State]</strong></p>
            #       <p>5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>CONFIRM ZIP from above</strong></p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>(VOL.)</strong> Respondent does not want the money</p>
            #       <p>&nbsp;</p>
            #       <p><strong>INTERVIEWER ASK IF RESPONDENT GAVE FULL ADDRESS IN MONEY:</strong></p>
            #       <p>POBOX1<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; INTERVIEWER: </strong>DID RESPONDENT GIVE A STREET ADDRESS OR A PO BOX?</p>
            #       <p>&nbsp;</p>
            #       <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Street address</p>
            #       <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; PO BOX</p>
            #       <p>&nbsp;</p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p>Thank you very much for your time. This survey is being conducted by the Pew Research Center, which will be issuing a report on the results of this survey on their website, pewresearch <em>dot </em>ORG, in the coming weeks.</p>
            #       <p>&nbsp;</p>
            #       <p>I HEREBY ATTEST THAT THIS IS A TRUE AND HONEST INTERVIEW.</p>
            #       <p>INTERVIEWER GENDER:</p>
            #       <p>ISEX</p>
            #       <p><strong>&nbsp;</strong></p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Male</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Female</p>
            #       <p>&nbsp;</p>
            #       <p>INTERVIEWER RACE:</p>
            #       <p>IHISP1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Are you, yourself, of Hispanic origin or descent, such as Mexican, Puerto Rican, Cuban, or some other Spanish background?</p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Yes</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; No</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Don't know/Refused <strong>(VOL.)</strong></p>
            #       <p>&nbsp;</p>
            #       <p>&nbsp;</p>
            #       <p>&nbsp;</p>
            #       <p>IRACE1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Which of the following describes your race? You can select as many as apply.</p>
            #       <p><strong>[READ LIST. RECORD UP TO FOUR RESPONSES IN ORDER MENTIONED] </strong></p>
            #       <p>&nbsp;</p>
            #       <p>1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; White</p>
            #       <p>2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Black or African-American</p>
            #       <p>3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Asian or Asian-American</p>
            #       <p>4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Or some other race</p>
            #       <p>9&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong>[VOL. DO NOT READ] </strong>Don't know/Refused</p>
            #       <p>&nbsp;</p>
            #       <p><strong>[PLEASE MAKE THE FOLLOWING TEXT AVAILABLE TO INTERVIEWERS ANYTIME A RESPONDENT ASKS ABOUT THE NATURE OF THE PEW RESEARCH CENTER]</strong>The Pew Research Center is an independent nonpartisan public opinion research organization that studies attitudes toward politics, the press and issues facing the nation. The Center has no connection to the government, political parties, or any campaigns. Reports about its surveys are made available free of charge on their website pewresearch <em>dot </em>ORG.</p>
            #       <p><strong>&nbsp;</strong></p>
                  # "
            )
          )
        ))),
      tabItem(
        tabName = "coefs_table",
        h2("Factor Coefficients by Variable"),
        fluidRow(box(
          width = 12, 
          HTML("
                   <p>The 51 variables below, corresponding to survey questions, were used to determine factors. These were picked because they were asked of all respondents, but specifically don't ask about partisan identity or demographics -- as these survey questions were reserved as independent variables and not part of a complex DV distilled via factorization.</p>
                   <p>A row.name can be translated easily into a survey question -- for example, 'q25k.c1' = 'Q.25, sub-question K, statement #1'</p>
                   <p>A row.name ending in .ca or .cd indicate an agree/disagree question that was recoded into two variables with positive response ranges from 0.0-1.0 for inclusion in NMF.</p>
                   <p>'predict' indicates the factor number predicted by a particular variable, and 'prob' indicates the probability of assignment to that factor for positive correlation to the variable.</p>
                   <p>X1, X2, and X3 correspond to the factors 1, 2, and 3 respectively. A higher coefficient means a variable is highly-correlated. Some variables are related to multiple factors.</p>"),
          dataTableOutput("coefs_table")
        )))
    ))
)

