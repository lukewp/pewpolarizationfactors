# pewpolarizationfactors
Study of the 2014 Pew Political Polarization Survey

This approach and visualization dashboard require acquisition of the Pew Political Polarization Dataset from 2014, available here:
http://www.people-press.org/2014/03/16/2014-political-polarization-survey/

The analysis reveals 3 dominant factors in US political life, to help explain some American political dynamics. 

Instead of thinking of America's electorate as right, left, and in the middle, it's better to think of a triangle with a major (equally-sized) constituency at each point.

In order to build a majority coalition, any two of these points must align behind an issue or a candidate.

- Factor 1: Human Rights and Equality -- Blue, highly correlated with the Democratic Party
- Factor 2: Traditional Values -- Yellow or Gold, split between the two mainstream American political parties, like neither party is consistent with their values
- Factor 3: Free Market Capitalism -- Red, highly correlated with the Republican Party

In terms of electoral votes, different coalitions offer promise of delivering majorities in different states. 

In the 2008 election, the coalition was a "green" coalition -- that is, a combination of the blue and gold factor constituencies.
In 2016, the coalition was an "orange" coalition -- that is, a combination of the red and gold factor constituencies.

A "purple" coalition may also be possible, one that finds common ground between the Blue and Red constituencies at the expense of the Traditional Values factor.

Explore these datasets by going through code in this package, or also look at the shinyapps.io page here (assuming I haven't used all my free hours this month!): https://lwp-viz.shinyapps.io/usfactors/

TODO: Decision tree for factor assignment by demographic

TODO: Interactive "what factor are you?" assignment form

Steps to building this dashboard yourself:

1. Clone this repository
2. Acquire the Pew data, put "Polarization 2014.zip" file into ./data subdirectory and unzip so a file called 'Polarization 2014 public.sav' is in the ./data/Polarization 2014/ folder
3. Run load_and_set_up_polarization2014.R to establish the survey object and recode some variables for NMF.
4. Run NMFsetup.R to build the rawinput and factormatrix_clean objects that'll be referred to by subsequent scripts
5. Run RunNMF.R: By default this will just load the .RData file containing the NMFfit object factormatrix_clean.3rank and assign it to the generic handle model.rank. But if you want to try creating new factorization models with other ranks or have a look at the NMF diagnostic process, dig into that code a bit. Whatever you end up doing, make sure there's an object of type NMFfit called 'model.rank' to drive subsequent steps.
6. Run shinySetup.R to build the supporting tables for the Shiny dashboard
7. In RStudio (https://www.rstudio.com/) open either server.R or ui.R and hit 'Run App' to launch the dashboard.