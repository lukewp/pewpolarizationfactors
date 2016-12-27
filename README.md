# pewpolarizationfactors
Study of the 2014 Pew Political Polarization Survey

This approach and visualization dashboard require acquisition of the Pew Political Polarization Dataset from 2014, available here:
http://www.people-press.org/2014/03/16/2014-political-polarization-survey/

The analysis reveals 3 dominant factors in US political life, to help explain some American political dynamics. 

Instead of thinking of America's electorate as right, left, and in the middle, it's better to think of a triangle with a major (equally-sized) constituency at each point.

In order to build a majority coalition, any two of these points must align behind an issue or a candidate.

- Factor 1: Human Rights and Equality -- Blue, highly correlated with the Democratic Party
- Factor 2: Traditional Values -- Yellow, split between the two mainstream American political parties, like neither party is consistent with their values
- Factor 3: Free Market Capitalism -- Red, highly correlated with the Republican Party

In terms of electoral votes, different coalitions offer promise of delivering majorities in different states. 

In the 2008 election, the coalition was a "green" coalition -- that is, a combination of the blue and yellow factor constituencies.
In 2016, the coalition was an "orange" coalition -- that is, a combination of the red and yellow factor constituencies.

A "purple" coalition may also be possible, one that finds common ground between the Blue and Red constituencies at the expense of the Traditional Values factor.

Explore these datasets by going through code in this package, or also look at the shinyapps.io page here (assuming I haven't used all my free hours this month!): https://lwp-viz.shinyapps.io/usfactors/

TODO: Regression explanation of state-by-state 2012 votes by factor

TODO: Explain three-DV mlm models' fit explanation

TODO: Venn diagram for all voters

TODO: Venn diagram for electoral votes

TODO: Choropleths for electoral votes

TODO: Scenario-builder

TODO: Decision tree for factor assignment by survey

TODO: Decision tree for factor assignment by demographic

TODO: Interactive "what factor are you?" assignment form
