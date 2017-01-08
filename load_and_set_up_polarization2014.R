install.packages("car")

library(foreign)
library(car)

## This is the SPSS-format data output file, available for download from Pew:
survey <- read.spss("./data/Polarization 2014/Polarization 2014 public.sav", to.data.frame=TRUE)

# Hard-code Rownums into an index:
survey$row.id <- rownames(survey)

# PEW RESEARCH CENTER
# POLITICAL TYPOLOGY/POLARIZATION 
# FINAL QUESTIONNAIRE
# 
# N=10,000 interviews of adults 18+ (5,000 landline, 5,000 cell phone) in English and Spanish.
# Form 1/Form 2: each a random half sample
# 50 states (include Alaska and Hawaii)
# Field Period: January 23-March 16, 2014 
# 
# LANDLINE INTRO:
#   Hello, I am _____ calling on behalf of the Pew Research Center. We are conducting a telephone opinion survey for leading newspapers and TV stations around the country. I’d like to ask a few questions of the [RANDOMIZE: “YOUNGEST MALE, 18 years of age or older, who is now at home” AND “YOUNGEST FEMALE, 18 years of age or older, who is now at home?”] [IF NO MALE/FEMALE, ASK: May I please speak with the YOUNGEST FEMALE/MALE, 18 years of age or older, who is now at home?] GO TO MAIN INTERVIEW
# 
# CELL PHONE INTRO:
#   Hello, I am _____ calling on behalf of the Pew Research Center. We are conducting a telephone opinion survey for leading newspapers and TV stations around the country. I know I am calling you on a cell phone. As a token of our appreciation for your time, we will pay all eligible respondents $5 for participating in this survey. This is not a sales call. [IF R SAYS DRIVING/UNABLE TO TAKE CALL: Thank you. We will try you another time…].
# 
# VOICE MAIL MESSAGE (LEAVE ONLY ONCE -- THE FIRST TIME A CALL GOES TO VOICEMAIL): I am calling on behalf of the Pew Research Center.  We are conducting a national opinion survey of cell phone users. This is NOT a sales call. We will try to reach you again.
# 
# SCREENING INTERVIEW:
#   S1.	Are you under 18 years old, OR are you 18 or older? 
# 1	Under 18
# 2	18 or older
# 9	Don’t know/Refused
# 
# IF S1=2, CONTINUE WITH MAIN INTERVIEW
# IF S1=1,9 THANK AND TERMINATE: This survey is limited to adults age 18 and over.  I won’t take any more of your time…
# 
# READ TO ALL CELL PHONE
# INTRODUCTION TO MAIN INTERVIEW: If you are now driving a car or doing any activity requiring your full attention, I need to call you back later. The first question is…
# 
# INTERVIEWER: 
#   IF R SAYS IT IS NOT A GOOD TIME, TRY TO ARRANGE A TIME TO CALL BACK.  OFFER THE TOLL-FREE CALL-IN NUMBER THEY CAN USE TO COMPLETE THE SURVEY BEFORE ENDING THE CONVERSATION.
# 
# [PROGRAMMER NOTE: PLEASE INCLUDE THE INTRODUCTION RANDOMIZATION VARIABLES IN THE ALL CONTACTS FILES. WE WOULD LIKE TO BE ABLE TO RUN RESPONSE RATES SEPARATELY FOR EACH VERSION OF THE INTRODUCTION FOR THE LANDLINE AND CELL FRAMES SEPARATELY.]
# 
# ASK ALL PHASE A:
#   Q.A1	Generally, how would you say things are these days in your life -- would you say that you are very happy, pretty happy, or not too happy? 
# 
# 1	Very happy
# 2	Pretty happy
# 3	Not too happy
# 9	Don’t know/Refused (VOL.)
# 
# ASK ALL PHASE C:
#   Q.C1	Do you approve or disapprove of the way Barack Obama is handling his job as President? [IF DK ENTER AS DK. IF DEPENDS PROBE ONCE WITH:  Overall do you approve or disapprove of the way Barack Obama is handling his job as President? IF STILL DEPENDS ENTER AS DK] 
# 
# 1	Approve
# 2	Disapprove
# 9	Don't know/Refused (VOL.)
# 
# ASK IF AND APPROVE OR DISAPPROVE (Q.C1=1,2):
# Q.C1a	Do you [approve/disapprove] very strongly, or not so strongly?
# 
# 1	Very strongly
# 2	Not so strongly
# 9	Don’t know/Refused (VOL.)
# 
# ASK ALL PHASE B:
# Q.B2	All in all, are you satisfied or dissatisfied with the way things are going in this country today? 
# 
# 1	Satisfied
# 2	Dissatisfied
# 9	Don’t know/Refused (VOL.)
# 
# ASK ALL PHASE B:
# Q.B3	And thinking about the local community where you live, are you satisfied or dissatisfied with the way things are going in your local community today?
# 
# 1	Satisfied
# 2	Dissatisfied
# 9	Don't know/Refused (VOL.)
# 
# ASK ALL PHASE B:
#   Q.B4	Thinking about the future of the United States, do you think the country's best years are ahead of us or behind us?
# 
# 1	Ahead of us
# 2	Behind us
# 9	Don’t know/Refused (VOL.)  
# 
# ASK ALL PHASE B:
# Q.B5	Thinking about the Democratic and Republican parties, would you say there is a great deal of difference in what they stand for, a fair amount of difference, or hardly any difference at all? 
# 
# 1	A great deal
# 2	A fair amount
# 3	Hardly any
# 9	Don’t know/Refused (VOL.)
# 
# 
# ASK ALL PHASE A:
# Q.A6	If you could live anywhere in the United States that you wanted to, would you prefer a city, a suburban area, a small town or a rural area? 
# 
# 1	City
# 2	Suburban area
# 3	Small town
# 4	Rural area
# 9	Don't know/Refused (VOL.)
# 
# NO QUESTION 7
# 
# ASK ALL PHASE A:
#   Imagine for a moment that you are moving to another community.
# ASK ALL:
#   Q.A8	 Would you prefer to live in [INSERT ITEM; RANDOMIZE]? 
# 
# 1	A community where the houses are larger and farther apart, but schools, stores, and restaurants are several miles away [OR]
# 2	A community where the houses are smaller and closer to each other, but schools, stores, and restaurants are within walking distance
# 9	[VOL. DO NOT READ] Don't know/Refused
# 
# ASK ALL PHASE A:
# Q.A9 	Still imagining that you are moving to another community. In deciding where to live, would each of the following be important, or not too important to you. First, would [INSERT ITEM; RANDOMIZE] be important, or not too important? What about [NEXT ITEM]? 
# 
# a.	Living in a place where most people share your political views
# b.	Having high quality public schools 
# c.	Living in a place with a mix of people from different racial and ethnic backgrounds 
# d.	Living in a place with many people who share your religious faith 
# e.	Being near art museums and theaters 
# f.	Having easy access to the outdoors for things like hiking, fishing, and camping 
# g.	Being near your extended family 
# 
# RESPONSE CATEGORIES:
# 1	Important
# 2	Not too important
# 9	Don't know/Refused (VOL.)
# 
# NO QUESTION 10
# 
# ASK ALL:
#   Next, 
# Q.11	Would you say your overall opinion of… [INSERT ITEM; RANDOMIZE] is very favorable, mostly favorable, mostly UNfavorable, or very unfavorable?  [INTERVIEWERS: PROBE TO DISTINGUISH BETWEEN “NEVER HEARD OF” AND “CAN’T RATE.”] How about [NEXT ITEM]? 
# 
# a.	The Republican Party 
# b.	The Democratic Party 
# 
# ASK PHASE B ONLY:
#   c.B	Congress 
# NO ITEM d
# e.B	Labor unions 
# NO ITEMS f AND g
# h.B	The National Rifle Association 
# i.B 	The Federal Reserve 
# j.B	The Environmental Protection Agency, the EPA 
# 
# RESPONSE CATEGORIES:
#   1	Very favorable
# 2	Mostly favorable
# 3	Mostly unfavorable
# 4	Very unfavorable
# 5	Never heard of (VOL.)
# 8	Can't rate (VOL.)
# 9	Refused (VOL.)
# 
# ASK PHASE C IF VERY UNFAVORABLE VIEW OF REP PARTY (Q11a=4):
# Q.11at	Would you say the Republican Party’s policies are so misguided that they threaten the nation’s well-being, or wouldn’t you go that far?
# 
# 1	Yes, Republican Party’s policies pose a threat to the nation’s well-being [OR]
# 2	No, wouldn’t go that far
# 9	Don’t know/Refused (VOL.)
# 
# ASK PHASE C IF VERY UNFAVORABLE VIEW OF DEM PARTY (Q11b=4):
# Q.11bt	Would you say the Democratic Party’s policies are so misguided that they threaten the nation’s well-being, or wouldn’t you go that far? 
# 
# 1	Yes, Democratic Party’s policies pose a threat to the nation’s well-being [OR]
# 2	No, wouldn’t go that far
# 9	Don’t know/Refused (VOL.)
# 
# ASK ALL PHASE B:
# Q.B12 	Thinking about elected officials in Washington who share your positions on the most important issues facing the nation. [READ AND RANDOMIZE]
# 
# 1	Should they work with elected officials they disagree with, even if it results in some policies you don’t like [OR]
# 2	Should they stand up for their positions, even if that means little gets done in Washington
# 9	[VOL. DO NOT READ] Don't know/Refused
# 
# NO QUESTIONS 13-24
# 
# ASK ALL:
#   Q.25	I'm going to read you some pairs of statements that will help us understand how you feel about a number of things.  As I read each pair, tell me whether the FIRST statement or the SECOND statement comes closer to your own views — even if neither is exactly right.  The first pair is [READ AND RANDOMIZE PAIRS BUT NOT STATEMENTS WITHIN EACH PAIR]. Next, [NEXT PAIR] [IF NECESSARY: “Which statement comes closer to your views, even if neither is exactly right?”]
# 
# a.	Government is almost always wasteful and inefficient [OR]
# Government often does a better job than people give it credit for 
# 
survey$q25a.c1 <- as.numeric(recode(survey$q25a, "'Statement #1'=1;else=0"))
survey$q25a.c2 <- as.numeric(recode(survey$q25a, "'Statement #2'=1;else=0"))

# 
# b.	Government regulation of business is necessary to protect the public interest [OR]
# Government regulation of business usually does more harm than good 
# 
survey$q25b.c1 <- as.numeric(recode(survey$q25b, "'Statement #1'=1;else=0"))
survey$q25b.c2 <- as.numeric(recode(survey$q25b, "'Statement #2'=1;else=0"))

# c.	Poor people today have it easy because they can get government benefits without doing anything in return [OR]
# Poor people have hard lives because government benefits don't go far enough to help them live decently 
# 
survey$q25c.c1 <- as.numeric(recode(survey$q25c, "'Statement #1'=1;else=0"))
survey$q25c.c2 <- as.numeric(recode(survey$q25c, "'Statement #2'=1;else=0"))

# d.	The government should do more to help needy Americans, even if it means going deeper into debt [OR]
# The government today can't afford to do much more to help the needy 
# 
survey$q25d.c1 <- as.numeric(recode(survey$q25d, "'Statement #1'=1;else=0"))
survey$q25d.c2 <- as.numeric(recode(survey$q25d, "'Statement #2'=1;else=0"))

# NO ITEM e
# 
# f.	Racial discrimination is the main reason why many black people can't get ahead these days [OR]
# Blacks who can't get ahead in this country are mostly responsible for their own condition 
# 
survey$q25f.c1 <- as.numeric(recode(survey$q25f, "'Statement #1'=1;else=0"))
survey$q25f.c2 <- as.numeric(recode(survey$q25f, "'Statement #2'=1;else=0"))

# g.	Immigrants today strengthen our country because of their hard work and talents [OR]
# Immigrants today are a burden on our country because they take our jobs, housing and health care 
# 
survey$q25g.c1 <- as.numeric(recode(survey$q25g, "'Statement #1'=1;else=0"))
survey$q25g.c2 <- as.numeric(recode(survey$q25g, "'Statement #2'=1;else=0"))

# h.	Society is better off if people make marriage and having children a priority [OR]
# Society is just as well off if people have priorities other than marriage and children 
# 
survey$q25h.c1 <- as.numeric(recode(survey$q25h, "'Statement #1'=1;else=0"))
survey$q25h.c2 <- as.numeric(recode(survey$q25h, "'Statement #2'=1;else=0"))

# i.	The best way to ensure peace is through military strength [OR]
# Good diplomacy is the best way to ensure peace 
# 
survey$q25i.c1 <- as.numeric(recode(survey$q25i, "'Statement #1'=1;else=0"))
survey$q25i.c2 <- as.numeric(recode(survey$q25i, "'Statement #2'=1;else=0"))

# j.	U.S. efforts to solve problems around the world usually end up making things worse [OR] Problems in the world would be even worse without U.S. involvement
# 
survey$q25j.c1 <- as.numeric(recode(survey$q25j, "'Statement #1'=1;else=0"))
survey$q25j.c2 <- as.numeric(recode(survey$q25j, "'Statement #2'=1;else=0"))

# k.	Most people who want to get ahead can make it if they're willing to work hard [OR]
# Hard work and determination are no guarantee of success for most people 
# 
survey$q25k.c1 <- as.numeric(recode(survey$q25k, "'Statement #1'=1;else=0"))
survey$q25k.c2 <- as.numeric(recode(survey$q25k, "'Statement #2'=1;else=0"))

# l.	Success in life is pretty much determined by forces outside of our control [OR]
# Everyone has it in their own power to succeed 
# 
survey$q25l.c1 <- as.numeric(recode(survey$q25l, "'Statement #1'=1;else=0"))
survey$q25l.c2 <- as.numeric(recode(survey$q25l, "'Statement #2'=1;else=0"))

# m.	Too much power is concentrated in the hands of a few large companies [OR]
# The largest companies do NOT have too much power 
# 
survey$q25m.c1 <- as.numeric(recode(survey$q25m, "'Statement #1'=1;else=0"))
survey$q25m.c2 <- as.numeric(recode(survey$q25m, "'Statement #2'=1;else=0"))

# n.	Business corporations make too much profit [OR]
# Most corporations make a fair and reasonable amount of profit 
# 
survey$q25n.c1 <- as.numeric(recode(survey$q25n, "'Statement #1'=1;else=0"))
survey$q25n.c2 <- as.numeric(recode(survey$q25n, "'Statement #2'=1;else=0"))

# ASK ALL PHASE B:
#   o.	Elected officials in Washington lose touch with the people pretty quickly [OR]
# Elected officials in Washington try hard to stay in touch with voters back home 
# 
# ASK ALL PHASE C:
#   p.	Most elected officials care what people like me think [OR]
# Most elected officials don't care what people like me think 
# 
# RESPONSE CATEGORIES:
# 1	Statement #1 
# 2	Statement #2 
# 5	Neither/Both equally (VOL.)
# 9	Don't know/Refused (VOL.)
# 
# 
# 
# ASK ALL PHASE B:
#   Q.B26 	And in your view, has this country been successful more because of its [INSERT ITEM; RANDOMIZE] or more because of its [ITEM]? 
# 
# 1	Ability to change [OR]
# 2	Reliance on long-standing principles
# 9	[VOL. DO NOT READ] Don't know/Refused
# 
# ASK ALL PHASE C:
# Q.C26	Next, [READ AND RANDOMIZE] 
# 
# 1	Americans are united and in agreement about the most important values [OR]
# 2	Americans are greatly divided when it comes to the most important values
# 9	[VOL. DO NOT READ] Don't know/Refused
# 
# ASK ALL:
#   OFTVOTE	How often would you say you vote...[READ IN ORDER]? 
# 
# 1	Always
# 2	Nearly always
# 3	Part of the time
# 4	Seldom
# 5	[VOL. DO NOT READ] Never vote
# 6	[VOL. DO NOT READ] Other
# 9	[VOL. DO NOT READ] Don't know/Refused
# 
# ASK PHASE A FORM 1:
# Now a different kind of question, 
# Q.26F1 Thinking about how Barack Obama and Republican leaders should address the most important issues facing the country. Imagine a scale from zero to 100 where 100 means Republican leaders get everything they want and Obama gets nothing he wants, and zero means Obama gets everything and Republican leaders get nothing. Where on this scale from zero to 100 do you think they should end up? [OPEN END ENTER NUMBER 0-100] [IF NECESSARY: “100 means Republicans get everything they want, ZERO means Obama gets everything he wants, about where, from 0 to 100 should they end up?] [INTERVIEWER, IF RESPONDENT STRUGGLES WITH PRECISE NUMBER YOU CAN SAY: “you can just give me a number close to what you think”] 
# 
# [IF RESPONDENT SAYS A NUMBER BETWEEN 0-49, CLARIFY: “Just to be sure I get this right, [INSERT NUMBER CHOSEN], means Obama should get more than Republican leaders, is that what you meant?” [IF NO, RESPONDENT MEANT REP LEADERS SHOULD GET MORE: “A number between 51 and 100 would mean Republican leaders get more than Obama”]
# 
# [IF RESPONDENT SAYS A NUMBER BETWEEN 51-100, CLARIFY: “Just to be sure I get this right, [INSERT NUMBER CHOSEN], means Republican leaders should get more than Obama, is that what you meant?” [IF NO, RESPONDENT MEANT OBAMA SHOULD GET MORE: “A number between 0 and 49 would mean Obama gets more than Republican leaders”] 
# 
# 1	[ENTER NUMBER 0-100]
# 999	[VOL. DO NOT READ] Don't know/Refused
# 
# 
# ASK PHASE A FORM 2:
#   Now a different kind of question, 
# Q.26F2 Thinking about how Barack Obama and Republican leaders should address the most important issues facing the country. Imagine a scale from zero to 100 where 100 means Obama gets everything he wants and Republican leaders get nothing they want, and zero means Republican leaders get everything and Obama gets nothing. Where on this scale from zero to 100 do you think they should end up? [OPEN END ENTER NUMBER 0-100] [IF NECESSARY: “100 means Obama gets everything he wants, ZERO means Republicans gets everything they want, about where, from 0 to 100 should they end up?] [INTERVIEWER, IF RESPONDENT STRUGGLES WITH PRECISE NUMBER YOU CAN SAY: “you can just give me a number close to what you think”] 
# 
# [IF RESPONDENT SAYS A NUMBER BETWEEN 51-100, CLARIFY: “Just to be sure I get this right, [INSERT NUMBER CHOSEN], means Obama should get more than Republican leaders, is that what you meant?” [IF NO, RESPONDENT MEANT REP LEADERS SHOULD GET MORE: “A number between 0 and 49 would mean Republican leaders get more than Obama”]
#   
#   [IF RESPONDENT SAYS A NUMBER BETWEEN 0-49, CLARIFY: “Just to be sure I get this right, [INSERT NUMBER CHOSEN], means Republican leaders should get more than Obama, is that what you meant?” [IF NO, RESPONDENT MEANT OBAMA SHOULD GET MORE: “A number between 51 and 100 would mean Obama gets more than Republican leaders”] 
#     
#     1	[ENTER NUMBER 0-100]
#     999	[VOL. DO NOT READ] Don't know/Refused
#     
#     ASK ALL PHASE B:
#     The next congressional elections will be coming up later this year…
#     Q.B27	If the elections for U.S. Congress were being held TODAY, would you vote for the Republican Party’s candidate or the Democratic Party’s candidate for Congress in your district? 
#     
#     1	Republican Party’s candidate 
#     2	Democratic Party’s candidate
#     3	Other (VOL.)
#     9	Don’t know/Refused (VOL.)
#     
#     ASK IF ‘OTHER’ ‘DON’T KNOW/REFUSED’ (Q.B27=3,9):
#     Q.B27a	As of TODAY, would you LEAN more to the Republican or the Democrat? 
#     
#     1	Republican Party’s candidate
#     2	Democratic Party’s candidate
#     3	Other (VOL.)
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL:
#     Next,
#     INT1	Do you use the internet, at least occasionally? 
#     
#     1	Yes
#     2	No
#     9	Don’t Know/Refused (VOL.)
#     
#     ASK IF DOES NOT USE THE INTERNET (INT1=2,9):
#     INT2	Do you send or receive email, at least occasionally? 
#     
#     1	Yes
#     2	No
#     9	Don’t Know/Refused (VOL.)
#     
#     ASK IF DOES NOT USE THE INTERNET OR EMAIL (INT2=2,9):
#     INT3M	Do you access the internet on a cell phone, tablet or other mobile handheld device, at least occasionally? 
#     
#     1	Yes
#     2	No
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE C:
#     Now, I have a short set of questions on marriage and family.
#     Q.C28	First, how do you think you would react if a member of your immediate family told you they were going to marry... [INSERT ITEM; RANDOMIZE]? Would you be generally happy about this, generally unhappy, or wouldn’t it matter to you at all? What about [NEXT ITEM]? [IF NECESSARY: if a member of your immediate family told you they were going to marry [ITEM], would you be generally happy, generally unhappy, or wouldn’t it matter to you at all?] [INTERVIEWER INSTRUCTION: IF RESPONDENT SAYS THEY HAVE FAMILY MEMBER(S) MARRIED TO SOMEONE OF THAT GROUP]: “Are you happy about that, unhappy about that, or doesn’t it matter?”] 
#     
#     a.	A Republican 
#     b.	A Democrat 
#     c.	Someone who didn’t go to college 
#     d.	Someone born and raised outside the U.S. 
#     e.	Someone who does not believe in God
#     f.	A “born again” Christian 
#     g.	A gun owner 
#     h.	Someone of a different race 
#     
#     RESPONSE CATEGORIES:
#     1	Happy 
#     2	Unhappy
#     3	Wouldn’t it matter to you at all
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE A:
#     Q.A29	Thinking about some news organizations, would you say your overall opinion of… [INSERT ITEM; RANDOMIZE] is favorable, unfavorable, or neither in particular?  How about [NEXT ITEM]? [IF NECESSARY: Is your overall opinion of [ITEM] favorable, unfavorable, or neither in particular]
#     
#     a.	MSNBC cable news 
#     b.	The Fox News Cable Channel 
#     
#     RESPONSE CATEGORIES:
#     1	Favorable
#     2	Unfavorable
#     3	Neither in particular
#     9	Don't know/Refused (VOL.)
#     
#     NO QUESTIONS 30-39
#     
#     ASK ALL:
#       Q.40	Would you say you follow what's going on in government and public affairs...[READ]? 
#     
#     1	Most of the time
#     2	Some of the time
#     3	Only now and then [OR]
#     4	Hardly at all
#     9	[VOL. DO NOT READ] Don't know/Refused
survey$q40.c <- as.numeric(recode(survey$q40, 
                           "'Most of the time'=1;
                           'Some of the time'=0.75;
                           'Only now and then [OR]'=0.5;
                           'Hardly at all'=0.25;
                           else=0"))

#     ASK ALL PHASE B:
#       Q.B40a	Some people say they are basically content with the federal government, others say they are frustrated, and others say they are angry. Which of these best describes how you feel?
#     
#     1	Basically content
#     2	Frustrated
#     3	Angry
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE B:
#       Q.B40b	How much of the time do you think you can trust the government in Washington to do what is right? Just about always, most of the time, or only some of the time? 
#     
#     1	Just about always
#     2	Most of the time
#     3	Only some of the time
#     4	Never (VOL.)
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL:
#       Just as far as you know…
#     Q.41	Which political party has a majority in the U.S. House of Representatives [READ AND RANDOMIZE]? [IF NECESSARY: Just as far as you know] [INTERVIEWER INSTRUCTION: DO NOT PROBE, PUNCH 9 IF RESPONDENT SAYS THEY DON’T KNOW] 
#     
#     1	The Republican Party [OR]
#     2	The Democratic Party
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK ALL:
#       Q.42	Which political party, has a majority in the U.S. Senate [READ AND RANDOMIZE]? [INTERVIEWER INSTRUCTION: DO NOT PROBE, PUNCH 9 IF RESPONDENT SAYS THEY DON’T KNOW] 
#     
#     1	The Republican Party [OR]
#     2	The Democratic Party
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK ALL PHASE A:
#       Q.43	Which political party is more in favor of raising taxes on higher income people [READ AND RANDOMIZE]? [IF NECESSARY: Just as far as you know] [INTERVIEWER INSTRUCTION: DO NOT PROBE, PUNCH 9 IF RESPONDENT SAYS THEY DON’T KNOW]
#     
#     1	The Republican Party [OR]
#     2	The Democratic Party
#     9	[VOL. DO NOT READ] Don't know/Refused
#     
#     NO QUESTIONS 44-47
#     
#     RANDOMIZE Q.C48/Q.C49
#     ASK ALL PHASE C:
#     Now some questions about your views of the political parties…
#     Q.C48 	Do you think the Republican Party [INSERT ITEM; RANDOMIZE] or not? 
#     
#     a.	Is too extreme 
#     b.	Cares about the middle class 
#     c.	Is too willing to cut government programs, even when they work
#     
#     RESPONSE CATEGORIES:
#     1	Yes, describes Republican Party
#     2	No
#     9	Don’t know/Refused (VOL.)
#     
#     RANDOMIZE Q.C48/Q.C49
#     ASK ALL PHASE C:
#     [Next,]
#     Q.C49	Do you think the Democratic Party [INSERT ITEM; RANDOMIZE] or not?
#     
#     a.	Is too extreme 
#     b.	Cares about the middle class 
#     c.	Too often sees government as the only way to solve problems
#     
#     RESPONSE CATEGORIES:
#     1	Yes, describes Democratic Party
#     2	No
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL:
#     Q.50	Now I'm going to read a few more pairs of statements. Again, just tell me whether the FIRST statement or the SECOND statement comes closer to your own views — even if neither is exactly right. The first pair is [READ AND RANDOMIZE ITEMS Q THRU Z FOLLOWED BY RANDOMIZED ITEMS AA THRU HH; RANDOMIZE PAIRS BUT NOT STATEMENTS WITHIN EACH PAIR]. Next, [NEXT PAIR] [IF NECESSARY: “Which statement comes closer to your views, even if neither is exactly right?”]
#     
#     q.	This country should do whatever it takes to protect the environment [OR]
#     This country has gone too far in its efforts to protect the environment 
#     
survey$q50q.c1 <- as.numeric(recode(survey$q50q, "'Statement #1'=1;else=0"))
survey$q50q.c2 <- as.numeric(recode(survey$q50q, "'Statement #2'=1;else=0"))

#     r.	Stricter environmental laws and regulations cost too many jobs and hurt the economy [OR]
#     Stricter environmental laws and regulations are worth the cost 
#     
survey$q50r.c1 <- as.numeric(recode(survey$q50r, "'Statement #1'=1;else=0"))
survey$q50r.c2 <- as.numeric(recode(survey$q50r, "'Statement #2'=1;else=0"))

#     ASK ALL PHASE B:
#       s.	There are no real limits to growth in this country today [OR]
#     People in this country should learn to live with less 
#     
#     ASK ALL PHASE C:
#       t.	As Americans, we can always find ways to solve our problems and get what we want [OR]
#     This country can't solve many of its important problems 
#     
#     ASK ALL:
#     u.	Homosexuality should be accepted by society [OR]
#     Homosexuality should be discouraged by society 
#     
survey$q50u.c1 <- as.numeric(recode(survey$q50u, "'Statement #1'=1;else=0"))
survey$q50u.c2 <- as.numeric(recode(survey$q50u, "'Statement #2'=1;else=0"))

#     ASK ALL PHASE C:
#     v.	It’s not the government’s job to protect people from themselves [OR]
#     Sometimes laws to protect people from themselves are necessary 
#     
#     ASK ALL PHASE A:
#     w.	Religion is a very important part of my life [OR]
#     Religion is not that important to me 
#     
#     NO ITEM x
#     
#     ASK ALL:
#     y.	I'm generally satisfied with the way things are going for me financially [OR]
#     I'm not very satisfied with my financial situation 
#     
#     z.	I often don't have enough money to make ends meet [OR] 
#     Paying the bills is generally not a problem for me 
#     
#     aa.	It IS NOT necessary to believe in God in order to be moral and have good values [OR]
#     It IS necessary to believe in God in order to be moral and have good values 
#     
survey$q50aa.c1 <- as.numeric(recode(survey$q50aa, "'Statement #1'=1;else=0"))
survey$q50aa.c2 <- as.numeric(recode(survey$q50aa, "'Statement #2'=1;else=0"))

#     bb.	Using overwhelming military force is the best way to defeat terrorism around the world [OR]
#     Relying too much on military force to defeat terrorism creates hatred that leads to more terrorism 
#     
survey$q50bb.c1 <- as.numeric(recode(survey$q50bb, "'Statement #1'=1;else=0"))
survey$q50bb.c2 <- as.numeric(recode(survey$q50bb, "'Statement #2'=1;else=0"))

#     NO ITEM cc
#     
#     dd.	The growing number of newcomers from other countries threatens traditional American customs and values [OR]
#     The growing number of newcomers from other countries strengthens American society
#     
survey$q50dd.c1 <- as.numeric(recode(survey$q50dd, "'Statement #1'=1;else=0"))
survey$q50dd.c2 <- as.numeric(recode(survey$q50dd, "'Statement #2'=1;else=0"))

#     ee.	It’s best for the future of our country to be active in world affairs [OR]
#     We should pay less attention to problems overseas and concentrate on problems here at home 
#     
survey$q50ee.c1 <- as.numeric(recode(survey$q50ee, "'Statement #1'=1;else=0"))
survey$q50ee.c2 <- as.numeric(recode(survey$q50ee, "'Statement #2'=1;else=0"))

#     ff.	Americans need to be willing to give up privacy and freedom in order to be safe from terrorism [OR] Americans shouldn’t have to give up privacy and freedom in order to be safe from terrorism 
# 
# Not asked of all:
# survey$q50ff.c1 <- recode(survey$q50ff, "'Statement #1'=1;else=0")
# survey$q50ff.c2 <- recode(survey$q50ff, "'Statement #2'=1;else=0")

#     ASK ALL PHASE B:
#       gg.	The government should do more to protect morality in society [OR]
#     I worry the government is getting too involved in the issue of morality 
#     
#     hh.	Our country has made the changes needed to give blacks equal rights with whites [OR]
#     Our country needs to continue making changes to give blacks equal rights with whites 
#     
#     RESPONSE CATEGORIES:
#       1	Statement #1 
#     2	Statement #2 
#     5	Neither/Both equally (VOL.)
#     9	Don't know/Refused (VOL.)
#     
#     ASK ALL:
#     Q.51	Next, [ASK ITEM ii FIRST, FOLLOWED BY RANDOMIZED ITEMS jj THROUGH mm AND RANDOMIZE STATEMENTS WITHIN PAIRS]. [IF NECESSARY: “Which statement comes closer to your views, even if neither is exactly right?”] Next, [NEXT PAIR]
#     
#     ASK ALL PHASE A:
#     ii.	Government should do more to solve problems [OR]
#     Government is doing too many things better left to businesses and individuals 
#     
#     ASK ALL PHASE A:
#     jj.	Children are better off when a parent stays home to focus on the family
#     Children are just as well off when their parents work outside the home 
#     
#     ASK ALL:
#     kk.	Government aid to the poor does more harm than good, by making people too dependent on government assistance [OR]
#     Government aid to the poor does more good than harm, because people can’t get out of poverty until their basic needs are met 
#     
survey$q51kk.c1 <- as.numeric(recode(survey$q51kk, "'Statement #1'=1;else=0"))
survey$q51kk.c2 <- as.numeric(recode(survey$q51kk, "'Statement #2'=1;else=0"))

#     ASK ALL PHASE A:
#     ll.	The economic system in this country unfairly favors powerful interests [OR]
#     The economic system in this country is generally fair to most Americans 
#     
#     ASK ALL PHASE A:
#     mm.	I like elected officials who make compromises with people they disagree with [OR] 
#     I like elected officials who stick to their positions 
#     
#     NO ITEM nn
#     
#     ASK ALL PHASE C:
#     oo.	The police should be allowed to stop and search anyone who fits the general description of a crime suspect [OR]
#     The police should not be able to search people just because they think they look suspicious 
#     
#     ASK ALL PHASE C:
#     pp.	Wall Street HELPS the American economy more than it hurts [OR]
#     Wall Street HURTS the American economy more than it helps 
#     
#     RESPONSE CATEGORIES:
#     1	Statement #1 
#     2	Statement #2 
#     5	Neither/Both equally (VOL.)
#     9	Don't know/Refused (VOL.)
#     
#     NO QUESTION 52
#     
#     ASK ALL PHASE A:
#       Q.53	In your opinion, which is generally more often to blame if a person is poor?  Lack of effort on his or her own part, or circumstances beyond his or her control? 
#     
#     1	Lack of effort
#     2	Circumstances beyond control
#     3	Both (VOL.)
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE B:
#       Q.B54	Next, [IF NECESSARY: Which comes closer to your own views — even if neither is exactly right]. [READ DO NOT RANDOMIZE STATEMENTS] 
#     
#     The Islamic religion is more likely than others to encourage violence among its believers [OR]
#     The Islamic religion does not encourage violence more than others
#     
#     RESPONSE CATEGORIES:
#       1	Statement #1 
#     2	Statement #2 
#     5	Neither/Both equally (VOL.)
#     9	Don't know/Refused (VOL.)
#     
#     ASK ALL PHASE B:
#     Q.B55	Should the U.S. Supreme Court base its rulings on its understanding of what the U.S. Constitution meant as it was originally written, or should the court base its rulings on its understanding of what the US Constitution means in current times? 
#     
#     1	What it meant as originally written
#     2	What it means in current times
#     3	Somewhere in between (VOL.)
#     9	Don’t know/Refused (VOL.)
#     
#     
#     ASK ALL PHASE C:
#     Q.C56	Which of these statements best describes your opinion about the United States? [READ IN ORDER; REVERSE ORDER FOR HALF OF SAMPLE] 
#     
#     1	The U.S. stands above all other countries in the world. 
#     2	The U.S. is one of the greatest countries in the world, along with some others [OR]
#     3	There are other countries that are better than the U.S. 
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK ALL PHASE C:
#     Q.C57	From what you’ve read and heard, is there solid evidence that the average temperature on earth has been getting warmer over the past few decades, or not? 
#     
#     1	Yes
#     2	No
#     3	Mixed/some evidence (VOL.)
#     9	Don’t know/Refused (VOL.)
#     
#     ASK IF EARTH IS GETTING WARMER (Q.C57=1):
#     Q.C58a	Do you believe that the earth is getting warmer [READ AND RANDOMIZE]? 
#     
#     1	Mostly because of human activity such as burning fossil fuels [OR]
#     2	Mostly because of natural patterns in the earth’s environment
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK IF EARTH IS NOT GETTING WARMER (Q.C57=2):
#     Q.C58b	Do you think that we just don’t know enough yet about whether the Earth is getting warmer or do you think it’s just not happening? 
#     
#     1	Just don’t know enough yet
#     2	Just not happening
#     9	Don’t know/Refused (VOL.)
#     
#     NO QUESTIONS 59-99
#     
#     ASK ALL:
#     Q.100	Have you ever contributed money to a candidate running for public office or to a group working to elect a candidate? 
#     
#     
#     1	Yes
#     2	No
#     9	Don’t know/Refused (VOL.)
#     
survey$q100.ca <- as.numeric(recode(survey$q100, "'Yes'=1;else=0"))
survey$q100.cd <- as.numeric(recode(survey$q100, "'No'=1;else=0"))

#     ASK IF HAVE EVER CONTRIBUTED MONEY (Q.100=1):
#     Q.101	Have you done this over the last two years, that is, during or since the 2012 elections, or not? [IF NECESSARY: Have you contributed money to any candidates or political groups over the last two years, or not?] 
#     
#     1	Yes
#     2	No
#     9	Don’t know/Refused (VOL.)
#     
#     
#     ASK IF HAVE CONTRIBUTED MONEY DURING 2012/2013 (Q.101=1):
#     Q.102	Over the last two years, would you say all of those contributions added up to more than $100 or less than that? 
#     
#     1	More than $100
#     2	Less than that 
#     9	Don’t know/Refused (VOL.)
#     
#     ASK IF MORE THAN $100 (Q.102=1):
#     Q.102a	And did they add up to more than $250 or not? 
#     
#     1	More than $250
#     2	No, less than that
#     9	Don’t know/Refused (VOL.)
#     
#     NO QUESTIONS 103-104
#     
#     ASK ALL:
#     Q.105	And again, just thinking about the last two years…Please tell me if you have done any of the following. First, over the last two years have you [INSERT ITEM; RANDOMIZE], or not? And over the last two years have you [INSERT NEXT ITEM], or not?
#     
#     a.	Worked or volunteered for a political candidate or campaign 
survey$q105a.ca <- as.numeric(recode(survey$q105a, "'Yes, have done this within the 2 years'=1;else=0"))
survey$q105a.cd <- as.numeric(recode(survey$q105a, "'No, have not done this within the 2 years'=1;else=0"))

#     b.	Contacted any elected official
survey$q105b.ca <- as.numeric(recode(survey$q105b, "'Yes, have done this within the 2 years'=1;else=0"))
survey$q105b.cd <- as.numeric(recode(survey$q105b, "'No, have not done this within the 2 years'=1;else=0"))

#     NO ITEM C
#     d.	Attended a campaign event 
#     
#     RESPONSE CATEGORIES:
#     1	Yes, have done this within the last 2 years
#     2	No, have not done this within the last 2 years
#     9	Don't know/Refused (VOL.)
#     
#     ASK ALL PHASE A:
#       Q.106	And have you, yourself, ever run for federal, state, or local elected office, or not? 
#     
#     1	Yes, have run for elected office
#     2	No, have not run for elected office
#     9	Don't know/Refused (VOL.)
#     
#     ASK ALL PHASE B:
#     Next,
#     Q.B106	Do you favor or oppose legalized casino gambling in your state? 
#     
#     1	Favor 
#     2	Oppose	
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE B:
#     Q.B107	Right now, which ONE of the following do you think should be the more important priority for addressing America’s energy supply? [READ AND RANDOMIZE]? 
#     
#     1	Developing alternative sources, such as wind, solar and hydrogen technology [OR]
#     2	Expanding exploration and production of oil, coal and natural gas
#     3	[VOL. DO NOT READ] Both should be given equal priority 
#     9	[VOL. DO NOT READ] Don’t know/Refused 
#     
#     
#     ASK ALL PHASE B:
#     Q.B108	Do you strongly favor, favor, oppose, or strongly oppose allowing gays and lesbians to marry legally? 
#     
#     1	Strongly favor
#     2	Favor
#     3	Oppose
#     4	Strongly oppose
#     9	Don't know/Refused (VOL.)
#     
#     ASK ALL PHASE B:
#       Q.B109	Thinking about our economic and trade policy toward China, which is more important [READ AND RANDOMIZE]?
#     
#     1	Building a stronger relationship with China on economic issues [OR]
#     2	Getting tougher with China on economic issues
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK ALL PHASE B:
#       Q.B110	Do you think the use of marijuana should be made legal, or not? 
#     
#     1	Yes, legal
#     2	No, illegal
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE C:
#       Q.C111	How much, if anything, have you read or heard about COMMON CORE, a set of education standards for students in grades K-12? Have you heard … [READ] 
#     
#     1	A lot
#     2	A little
#     3	Nothing at all
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK IF HEARD OF COMMON CORE (Q.C111=1,2):
#       Q.C112	From what you’ve read and heard, do you strongly favor, favor, oppose or strongly oppose the Common Core education standards? 
#     
#     1	Strongly favor
#     2	Favor
#     3	Oppose
#     4	Strongly oppose
#     9	Don’t know/Refused (VOL.)
#     
#     NO QUESTIONS 113-114
#     
#     ASK ALL PHASE C:
#       Q.C115	In general, do you think that free trade agreements between the U.S. and other countries have been a good thing or a bad thing for the United States? 
#     
#     1	Good thing
#     2	Bad thing
#     9	Don’t know/Refused (VOL.)
#     
#     
#     ASK ALL PHASE C:
#       Q.C116	Do you approve or disapprove of the health care law passed by Barack Obama and Congress in 2010? 
#     
#     1	Approve
#     2	Disapprove
#     9	Don't know/Refused (VOL.)
#     
#     ASK IF APPROVE OR DISAPPROVE (Q.C116=1,2)
#     Q.C116a Do you [approve/disapprove] very strongly, or not so strongly? 
#     
#     1	Very strongly
#     2	Not so strongly
#     9	Don't know/Refused (VOL.) 
#     
#     ASK IF ‘DISAPPROVE’ (Q.C116=2):
#       Q.C117 What do you think elected officials who oppose the health care law should do now that the law has started to take effect? Should they [READ AND RANDOMIZE] or should they [ITEM]? 
#     
#     1	Do what they can to make the law work as well as possible
#     2	Do what they can to make the law fail
#     9	[VOL. DO NOT READ] Don't know/Refused
#     
#     NO QUESTIONS 118-120
#     
#     RANDOMIZE IN BLOCKS:
#     Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b
#     ASK ALL PHASE A:
#     Q.121	Do you think it is the responsibility of the federal government to make sure all Americans have health care coverage, or is that not the responsibility of the federal government?
#     
#     1	Yes, government responsibility
#     2	No, not government responsibility
#     9	Don't know/Refused (VOL.)
#     
#     ASK IF GOVERNMENT RESPONSIBILITY (Q121=1):
#       Q.121a	Should health insurance [READ AND RANDOMIZE]? 
#     
#     1	Be provided through a single national health insurance system run by the government  [OR]
#     2	Continue to be provided through a mix of private insurance companies and government programs
#     9	[VOL. DO NOT READ] Don't know/Refused
#     
#     ASK IF NOT GOVERNMENT RESPONSIBILITY (Q121=2):
#     Q.121b	Should the government [READ AND RANDOMIZE]? 
#     
#     1	Not be involved in providing health insurance at all [OR SHOULD THE GOVERNMENT]
#     2	Continue programs like Medicare and Medicaid for seniors and the very poor 
#     9	[VOL. DO NOT READ] Don't know/Refused
#     
#     
#     RANDOMIZE IN BLOCKS:
#       Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b
#     ASK ALL PHASE A:
#       Q.122	Which comes closer to your view about how to handle immigrants who are now living in the U.S. illegally? Should they [READ AND RANDOMIZE] 
#     
#     1	Not be eligible for citizenship [OR SHOULD THEY]
#     2	Be eligible for citizenship if they meet certain requirements 
#     9	[VOL. DO NOT READ] Don’t know/Refused 
#     
#     ASK IF NOT ELIGIBLE FOR CITIZENSHIP (Q122=1):
#       Q.122a	Do you think there should be a national law enforcement effort to deport all immigrants who are now living in the U.S. illegally, or should that not be done? 
#     
#     1	Should be national law enforcement effort to deport
#     2	Should not be national law enforcement effort to deport
#     9	Don’t know/Refused  (VOL.)
#     
#     ASK IF BE ELIGIBLE FOR CITIZENSHIP (Q122=2):
#       Q.122b	And if immigrants meet these requirements, should they be eligible for citizenship? [READ AND RANDOMIZE] 
#     
#     1	Right away [OR]
#     2	Only after a period of time 
#     9	Don’t know/Refused (VOL.)
#     
#     RANDOMIZE IN BLOCKS:
#       Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b
#     ASK ALL PHASE A:
#       Q.123	What do you think is more important – to protect the right of Americans to own guns, OR to control gun ownership? 
#     
#     1	Protect the right of Americans to own guns
#     2	Control gun ownership
#     9	Don't know/Refused (VOL.)
#     
#     ASK IF MORE IMPORTANT TO PROTECT OWNERSHIP (Q.123=1):
#     Q.123a	And do you think there should be [READ AND RANDOMIZE]? 
#     
#     1	Some restrictions on gun ownership [OR SHOULD THERE BE]
#     2	No restrictions on gun ownership
#     9	Don't know/Refused (VOL.)
#     
#     ASK IF MORE IMPORTANT TO CONTROL OWNERSHIP (Q.123=2):
#       Q.123b	And do you think [READ AND RANDOMIZE]? 
#     
#     1	Most Americans should be able to own guns with certain limits in place [OR]
#     2	Only law enforcement and security personnel should be able to own guns
#     9	Don't know/Refused (VOL.)
#     
#     Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b
#     ASK ALL PHASE A:
#     Q.124	Do you think abortion should be [READ AND RANDOMIZE] 
#     
#     1	LEGAL in all or most cases [OR]
#     2	ILLEGAL in all or most cases
#     9	[VOL. DO NOT READ] Don't know/Refused 
#     
#     
#     ASK IF LEGAL IN ALL/MOST (q124=1):
#       Q.124a	Do you think there are any situations in which abortion should be restricted, or should there be no restrictions at all on abortion? 
#     
#     1	Situations in which abortion should be restricted
#     2	No restrictions at all on abortion
#     9	Don't know/Refused (VOL.)
#     
#     ASK IF ILLEGAL IN ALL/MOST (q124=2):
#     Q.124b	Do you think there are any situations in which abortion should be allowed, or should there be no situations at all where abortion is allowed? 
#     
#     1	Situations in which abortion should be allowed
#     2	No situations where abortion should be allowed
#     9	Don't know/Refused (VOL.)
#     
#     RANDOMIZE IN BLOCKS:
#       Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b
#     ASK ALL PHASE A:
#       Q.125	Thinking about the long term future of Social Security, do you think [READ AND RANDOMIZE]? 
#     
#     1	Some reductions in benefits for future retirees need to be considered [OR]
#     2	Social Security benefits should not be reduced in any way 
#     9	[VOL. DO NOT READ] Don’t know/Refused 
#     
#     ASK IF ACCEPTABLE (Q.125=1):
#       Q.125a	Should Social Security be [READ AND RANDOMIZE]?
#     
#     1	Phased out as a government program [OR SHOULD IT BE]
#     2	Maintained at a reduced level
#     9	[VOL. DO NOT READ] Don't know/Refused 
#     
#     ASK IF UNACCEPTABLE (Q.125=2):
#     Q.125b	Should Social Security [READ AND RANDOMIZE]?
#     
#     1	Cover more people, with greater benefits [OR SHOULD IT]
#     2	Be kept about as it is
#     9	[VOL. DO NOT READ] Don't know/Refused 
#     
#     RANDOMIZE IN BLOCKS:
#       Q121/121a/b, Q122/122a/b, Q123/a/b, Q124/a/b, Q125/a/b, Q126/a/b
#     ASK ALL:
#       Q.126	Overall, do you approve or disapprove of the government’s collection of telephone and internet data as part of anti-terrorism efforts? 
#     
#     1	Approve
#     2	Disapprove
#     9	Don't know/Refused (VOL.)
survey$q126.ca <- as.numeric(recode(survey$q126, "'Approve'=1;else=0"))
survey$q126.cd <- as.numeric(recode(survey$q126, "'Disapprove'=1;else=0"))

#     
#     ASK PHASE A IF APPROVE (Q.126=1):
#     Q.126a	Do you think the National Security Agency should be allowed to collect whatever data it needs, or should there be limits on what it collects?
#     
#     1	NSA should be allowed to collect whatever data it needs
#     2	Should be limits on what NSA collects
#     9	Don’t know/Refused (VOL.)
#     
#     
#     ASK PHASE A IF DISAPPROVE (Q.126=2):
#     Q.126b	Do you think the National Security Agency should be prevented from collecting any data about U.S. citizens, or should it be allowed to collect some limited information? 
#     
#     1	NSA prevented from collecting any data on citizens
#     2	NSA should be allowed to collect some limited information
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE C:
#     Q.C127 In general, do you think affirmative action programs designed to increase the number of black and minority students on college campuses are a good thing or a bad thing? 
#     
#     1	Good thing
#     2	Bad thing 
#     9	Don’t know/Refused (VOL.)
#     
#     ASK ALL PHASE C:
#     Q.C128	Do you favor or oppose building the Keystone XL pipeline that would transport oil from Canada’s oil sands region through the Midwest to refineries in Texas? 
#     
#     1	Favor
#     2	Oppose
#     9	Don’t know/Refused (VOL.)
#     
#     NO QUESTIONS 129-134
#     
#     ASK ALL PHASE C:
#     Next,
#     Q.C135	Which comes closer to your view? [READ AND RANDOMIZE] 
#     
#     1	Humans and other living things have evolved over time [OR]
#     2	Humans and other living things have existed in their present
#     form since the beginning of time
#     9	[VOL. DO NOT READ] Don’t know/Refused
#     
#     ASK IF EVOLVED (Q.C135=1):
#     Q.C135a	And do you think that…[READ OPTIONS AND RANDOMIZE]? 
#     
#     1	Humans and other living things have evolved due to natural processes such as natural selection, OR 
#     2	A supreme being guided the evolution of living things for the purpose of creating humans and other life in the form it exists today
#     9 	[VOL. DO NOT READ] Don’t know/Refused
#     
#     NO QUESTIONS 136-138
#     
#     ASK ALL:
#     Now, just a few questions for statistical purposes only. 
#     SEX	[ENTER RESPONDENT'S SEX:] 
#   
#   1	Male
#   2	Female
#   
#   ASK ALL:
#     AGE	What is your age? 
#   
#   ________  years
#   97	97 or older
#   99 	Don’t know/Refused (VOL.)
#   ASK ALL:
#     EDUC	What is the highest level of school you have completed or the highest degree you have received? [DO NOT READ] [INTERVIEWER NOTE:  Enter code 3-HS grad if R completed training that did NOT count toward a degree] 
#   
#   1	Less than high school (Grades 1-8 or no formal schooling)
#   2	High school incomplete (Grades 9-11 or Grade 12 with NO diploma)
#   3	High school graduate (Grade 12 with diploma or GED certificate)
#   4	Some college, no degree (includes community college)
#   5	Two year associate degree from a college or university
#   6	Four year college or university degree/Bachelor’s degree (e.g., BS, BA, AB)
#   7	Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)
#   8	Postgraduate or professional degree, including master’s, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD, graduate school)
#   9	Don't know/Refused (VOL.)
#   
#   [MAKE FULL NOTE AVAILABLE FOR INTERVIEWERS:  Enter code 3-HS graduate” if R completed vocational, business, technical, or training courses after high school that did NOT count toward an associate degree from a college, community college or university (e.g., training for a certificate or an apprenticeship)]
#   
#   ASK ALL:
#   HISP	Are you of Hispanic, Latino, or Spanish origin, such as Mexican, Puerto Rican or Cuban? 
#   
#   1	Yes
#   2	No
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL:
#     RACE	Which of the following describes your race? You can select as many as apply. White, Black or African American, Asian or Asian American or some other race. [RECORD UP TO FOUR IN ORDER MENTIONED BUT DO NOT PROBE FOR ADDITIONAL] [IF R VOLS MIXED BIRACIAL, PROBE ONCE: What race or races is that?] 
#   
#   1	White (e.g., Caucasian, European, Irish, Italian, Arab, Middle Eastern)
#   2	Black or African-American (e.g., Negro, Kenyan, Nigerian, Haitian)
#   3	Asian or Asian-American (e.g., Asian Indian, Chinese, Filipino, Vietnamese or other Asian origin groups)
#   4	Some other race (SPECIFY____ IF NEEDED: What race or races is that?)
#   5	Native American/American Indian/Alaska Native (VOL.)
#   6	Pacific Islander/Native Hawaiian (VOL.)
#   7	Hispanic/Latino (VOL.) (e.g., Mexican, Puerto Rican, Cuban)
#   8	Don't know (VOL.)
#   9	Refused (e.g., non-race answers like American, Human, purple) (VOL.)
#   
#   ASK IF HISPANIC (HISP=1 OR RACE=7):
#   BIRTH_HISP	Were you born in the United States, on the island of Puerto Rico, or in another country? 
#   
#   1	U.S. 
#   2	Puerto Rico 
#   3	Another country
#   9	Don't know/Refused (VOL.)
#   
#   
#   ASK IF NOT HISPANIC (HISP=2,9 AND RACE≠7):
#     USBORN 	Were you born in the United States or in another country?
#   
#   1	Yes, born in U.S.
#   2	No, some other country
#   3	Puerto Rico (VOL.)
#   4	Other U.S. Territories (includes Guam, Samoa, U.S. Virgin Islands) (VOL.)
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL:
#   MARITAL	Are you currently married, living with a partner, divorced, separated, widowed, or have you never been married? [IF R SAYS “SINGLE,” PROBE TO DETERMINE WHICH CATEGORY IS APPROPRIATE] 
#   
#   1	Married
#   2	Living with a partner
#   3	Divorced
#   4	Separated
#   5	Widowed
#   6	Never been married
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL PHASE A:
#     PARENT	Are you the parent or guardian of any children under 18 now living in your household? 
#   
#   1	Yes
#   2	No 
#   9	Don't know/Refused (VOL.)
#   
#   ASK IF NOT BORN IN US, PUERTO RICO OR US TERRITORIES (BIRTH_HISP=3,9 OR USBORN=2,9):
#   CITIZEN	Are you a citizen of the United States, or not? 
#   
#   1	Yes
#   2	No
#   9	Don’t know/Refused (VOL.)
#   
#   ASK ALL:
#   RELIG	What is your present religion, if any? Are you Protestant, Roman Catholic, Mormon, Orthodox such as Greek or Russian Orthodox, Jewish, Muslim, Buddhist, Hindu, atheist, agnostic, something else, or nothing in particular? 
#   
#   [INTERVIEWER: IF R VOLUNTEERS “nothing in particular, none, no religion, etc.” BEFORE REACHING END OF LIST, PROMPT WITH: And would you say that’s atheist, agnostic, or just nothing in particular?]
#   
#   1	Protestant (Baptist, Methodist, Non-denominational, Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, Church of Christ, Jehovah’s Witness, etc.)
#   2	Roman Catholic (Catholic)
#   3	Mormon (Church of Jesus Christ of Latter-day Saints/LDS)
#   4	Orthodox (Greek, Russian, or some other orthodox church)
#   5	Jewish (Judaism)
#   6	Muslim (Islam)
#   7	Buddhist
#   8	Hindu
#   9	Atheist (do not believe in God)
#   10	Agnostic (not sure if there is a God)
#   11 	Something else (SPECIFY:______)
#   12	Nothing in particular
#   13	Christian (VOL.)
#   14	Unitarian (Universalist) (VOL.)
#   99	Don't Know/Refused (VOL.)
#   
#   ASK IF SOMETHING ELSE OR DK/REF (RELIG=11, 99):
#     CHR 	Do you think of yourself as a Christian or not? [IF R NAMED A NON-CHRISTIAN RELIGION IN PREVIOUS QUESTION (e.g. Native American, Wiccan, Pagan, etc.), DO NOT READ (ENTER "NO" CODE 2)] 
#   
#   1	Yes
#   2	No
#   9	Don't know/Refused (VOL.)
#   
#   ASK IF CHRISTIAN (RELIG=1-4, 13 OR CHR=1):
#   BORN	Would you describe yourself as a "born again" or evangelical Christian, or not?
#   
#   1	Yes, would
#   2	No, would not
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL:
#     ATTEND	Aside from weddings and funerals, how often do you attend religious services... more than once a week, once a week, once or twice a month, a few times a year, seldom, or never? 
#   
#   1	More than once a week
#   2	Once a week
#   3	Once or twice a month
#   4	A few times a year
#   5	Seldom
#   6	Never
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL PHASE B:
#   Q.B139	Which comes closest to your view? [READ IN ORDER]
#   
#   [Holy book: If Christian or no religion (RELIG =1-4, 9, 10, 12, 13 OR CHR=1) insert “the Bible”; If Jewish (RELIG =5), insert “the Torah”; If Muslim (RELIG=6), insert, “the Koran”; If other non-Christian affiliations (RELIG=7,8,14 OR (RELIG=11 AND CHR=2,9)), insert “the Holy Scripture”; IF DK/REF IN RELIGION (RELIG=99) AND CHR=2,9, insert "the Bible"] 
#   
#   1	[Holy book] is the word of God, [OR] 
#   2	[Holy book] is a book written by men and is not the word of God.
#   3	[VOL. DO NOT READ] Other 
#   9	[VOL. DO NOT READ] Don’t know/Refused
#   
#   ASK IF BELIEVE HOLY BOOK IS WORD OF GOD (Q.B139=1):
#   Q.B139a	And would you say that [READ IN ORDER]? 
#   
#   1	[Holy book] is to be taken literally, word for word [OR]
#   2	Not everything in [Holy book] should be taken literally, word for word.
#   3	[VOL. DO NOT READ] Other 
#   9	[VOL. DO NOT READ] Don’t know/Refused
#   
#   
#   ASK ALL:
#   INCOME	Last year, that is in 2013, what was your total family income from all sources, before taxes? Just stop me when I get to the right category. [READ]
#   
#   1	Less than $10,000
#   2	10 to under $20,000
#   3	20 to under $30,000
#   4	30 to under $40,000
#   5	40 to under $50,000
#   6	50 to under $75,000
#   7	75 to under $100,000
#   8	100 to under $150,000 [OR]
#   9	$150,000 or more
#   10	[VOL. DO NOT READ] Don't know/Refused
#   
#   ASK IF INCOME $150K+ (INCOME=9):
#     INCHI	And was your total 2013 family income before taxes [READ] 
#   
#   1	Under $250,000 [or]
#   2	$250,000 or more
#   9	[VOL. DO NOT READ] Don't know/Refused
#   
#   ASK ALL:
#   REG	Which of these statements best describes you? [READ IN ORDER] [INSTRUCTION: BE SURE TO CLARIFY WHETHER RESPONDENT IS ABSOLUTELY CERTAIN THEY ARE REGISTERED OR ONLY PROBABLY REGISTERED; IF RESPONDENT VOLUNTEERS THAT THEY ARE IN NORTH DAKOTA AND DON’T HAVE TO REGISTER, PUNCH 1]
#   
#   1	Are you ABSOLUTELY CERTAIN that you are registered to vote at your current address [OR]
#   2	Are you PROBABLY registered, but there is a chance your registration has lapsed [OR]
#   3	Are you NOT registered to vote at your current address
#   9	[VOL. DO NOT READ] Don’t know/Refused
#   
#   ASK ALL:
#   PARTY	In politics TODAY, do you consider yourself a Republican, Democrat, or independent? 
#   
#   1	Republican 
#   2	Democrat 
#   3	Independent 
#   4	No preference (VOL.)
#   5	Other party (VOL.)
#   9	Don't know/Refused (VOL.) 
#   
#   ASK IF INDEP/NO PREF/OTHER/DK/REF (PARTY=3,4,5,9):
#     PARTYLN	As of today do you lean more to the Republican Party or more to the Democratic Party? 
#   
#   1	Republican
#   2	Democrat
#   9	Other/Don't know/Refused (VOL.)
#   
#   
#   ASK IF REPUBLICAN OR DEMOCRAT (PARTY=1,2): 
#   PARTYSTR	Do you consider yourself a STRONG [Republican/Democrat] or NOT a strong [Republican/Democrat]? 
#   
#   1	Strong
#   2	Not strong
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL:
#     IDEO	In general, would you describe your political views as... [READ]
#   
#   1	Very conservative
#   2	Conservative
#   3	Moderate
#   4	Liberal [OR]
#   5	Very liberal
#   9	[VOL. DO NOT READ] Don't know/Refused
#   
#   RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS
#   ASK IF NOT CURRENTLY DEMOCRAT (PARTY=1,3,4,5,9 AND REG=1):
#   Q.B140	Has there ever been a time when you have thought of yourself as a DEMOCRAT, or not? 
#   
#   1	Yes
#   2	No
#   9	Don't know/Refused (VOL.)
#   
#   RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS
#   ASK IF EVER THOUGHT OF SELF AS DEMOCRAT (Q.B140=1):
#     Q.B140b	What about in the past ten years, have you thought of yourself as a Democrat in the past ten years, or not? 
#   
#   1	Yes
#   2	No
#   9	Don't know/Refused (VOL.)
#   
#   RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS
#   ASK NOT CURRENTLY REPUBLICAN (PARTY=2,3,4,5,9 AND REG=1):
#   Q.B141	Has there ever been a time when you have thought of yourself as a REPUBLICAN, or not? {9-10 mod filter} {QID:qid20140301qb141}
#   
#   1	Yes
#   2	No
#   9	Don't know/Refused (VOL.)
#   
#   RANDOMIZE ORDER OF Q.B140/Q.B140b AND Q.B141/Q.B141b IN BLOCKS
#   ASK IF EVER THOUGHT OF SELF AS REPUBLICAN (Q.B141=1):
#     Q.B141b	What about in the past ten years, have you thought of yourself as a Republican in the past ten years, or not? 
#   
#   1	Yes
#   2	No
#   9	Don't know/Refused (VOL.)
#   
#   
#   ASK ALL REGISTERED VOTERS (REG=1):
#   Q.C142	Thinking about the elections you have voted in over the past several years, including national and statewide elections. Would you say you [READ IN ORDER; REVERSE ORDER FOR RANDOM HALF OF SAMPLE]? 
#   
#   1	Always vote Republican
#   2	Usually vote Republican
#   3	Vote about equally for both parties
#   4	Usually vote Democratic [OR]
#   5	Always vote Democratic 
#   7	[VOL. DO NOT READ] Have never voted
#   8	[VOL. DO NOT READ] Don’t vote for either party/vote for other parties 
#   9	[VOL. DO NOT READ] Don't know/Refused 
#   
#   NO QUESTIONS 143-147
#   
#   ASK ALL REGISTERED VOTERS (REG=1):
#     Q.148	As you may know, primary elections, where parties select their nominees, take place in the months before general elections. Thinking about the primary elections for Congress this year, do you happen to know in what month your state’s primary will be held? [OPEN END; SINGLE PUNCH; DO NOT READ, USE PRECODES, IF RESPONDENT IS NOT SURE, DO NOT PROBE, ENTER AS DON’T KNOW] 
#   
#   PRECODES
#   1	January/February
#   2	March
#   3	April
#   4	May
#   5	June
#   6	July
#   7	August
#   8	September
#   9	October/November/December
#   99	[VOL. DO NOT READ] Don't know/Refused
#   
#   ASK ALL REGISTERED VOTERS (REG=1):
#   Q.149	And how often would you say you vote in Congressional PRIMARY elections. Would you say you vote in Congressional primary elections [READ IN ORDER]? 
#   
#   1	Always
#   2	Nearly always
#   3	Part of the time [OR]
#   4	Seldom or never
#   5	[VOL. DO NOT READ] Not registered with a party/Can’t vote in primaries
#   9	[VOL. DO NOT READ] Don’t know/Refused 
#   
#   ASK ALL:
#   TEAPARTY2	From what you know, do you agree or disagree with the Tea Party movement, or don’t you have an opinion either way? 
#   
#   1	Agree
#   2	Disagree 
#   3	No opinion either way
#   8	Haven’t heard of (VOL.)
#   9	Refused (VOL.)
#   
#   
#   ASK PHASE A IF AGREE WITH TEA PARTY (TEAPARTY2=1):
#   Q.150	Have you ever attended a Tea Party rally or meeting, or not? [IF YES: Was that in the last two years, or not?] 
#   
#   1	Yes, within the 2 years
#   2	Yes, but NOT within the last 2 years
#   3	Yes, but don’t know if within last 2 years (VOL.)
#   4	No
#   9	Don't know/Refused (VOL.)
#   
#   ASK ALL:
#     HH1	How many people, including yourself, live in your household? 
#   INTERVIEWER NOTE: HOUSEHOLD MEMBERS INCLUDE PEOPLE WHO THINK OF THIS HOUSEHOLD AS THEIR PRIMARY PLACE OF RESIDENCE, INCLUDING THOSE WHO ARE TEMPORARILY AWAY ON BUSINESS, VACATION, IN A HOSPITAL, OR AWAY AT SCHOOL. THIS INCLUDES INFANTS, CHILDREN AND ADULTS. 
#   
#   ______	Enter number 1-7
#   8	8 or more
#   9	Don’t know/Refused
#   
#   ASK IF MORE THAN ONE PERSON IN HH (HH1>1):
#     HH3	How many, including yourself, are adults, age 18 and older? 
#   
#   ______	Enter number 1-7
#   8	8 or more
#   9	Don’t know/Refused
#   
#   ASK ALL LANDLINE SAMPLE:
#     L1.	Now thinking about your telephone use… Do you have a working cell phone? 
#   
#   1	Yes, have cell phone
#   2	No, do not
#   9	Don't know/Refused (VOL.)
#   
#   ASK IF NO CELL PHONE AND MULTI-PERSON HOUSEHOLD (L1=2,9 AND HH1>1):
#   L1a.	Does anyone in your household have a working cell phone? 
#   
#   1	Yes, someone in household has cell phone
#   2	No
#   9	Don't know/Refused (VOL.) 
#   ASK ALL CELL PHONE SAMPLE:
#     C1.	Now thinking about your telephone use… Is there at least one telephone INSIDE your home that is currently working and is not a cell phone?
#   
#   1	Yes home telephone
#   2	No, home telephone
#   9	Don't know/Refused (VOL.)
#   
#   ASK IF DUAL AND SINGLE-PERSON HOUSEHOLD ((L1=1 OR C1=1) AND HH1=1):
#   LC2.	Of all the telephone calls that you receive, do you get [READ AND RANDOMIZE OPTIONS 1 AND 3—KEEP 2 ALWAYS IN THE MIDDLE]? 
#   
#   1	All or almost all calls on a cell phone
#   2	Some on a cell phone and some on a regular home phone
#   3	All or almost all calls on a regular home phone
#   9	[VOL. DO NOT READ] Don’t know/Refused 
#   
#   ASK IF DUAL AND MULTI-PERSON HOUSEHOLD ((L1=1 OR L1a=1 OR C1=1) AND HH1>1):
#   LC3.	Now thinking about all the people in your household, including yourself, of all the telephone calls that your household receives, are [READ AND RANDOMIZE OPTIONS 1 AND 3—KEEP 2 ALWAYS IN THE MIDDLE]? 
#   
#   1	All or almost all calls on a cell phone
#   2	Some on a cell phone and some on a regular home phone
#   3	All or almost all calls on a regular home phone
#   9	[VOL. DO NOT READ] Don’t know/Refused 
#   
#   ASK ALL:
#   ZIPCODE	What is your zipcode? 
#   
#   _____	Enter Zipcode
#   9	Don’t know/Refused
#   
#   ASK CELL PHONE SAMPLE:
#   MONEY 	We’d like to send you $5 for your time. Can I please have your name and a mailing address where we can send you the money? [INTERVIEWER NOTE: If R does not want to give full name, explain we only need it so we can send the $5 to them personally.] 
#   
#   1	[ENTER FULL NAME] – INTERVIEWER: PLEASE VERIFY SPELLING, MAKE SURE TO GET BOTH FIRST AND LAST NAME
#   2	[ENTER MAILING ADDRESS]
#   3	[City]
#   4	[State]
#   5	CONFIRM ZIP from above
#   9	(VOL.) Respondent does not want the money
#   
#   INTERVIEWER ASK IF RESPONDENT GAVE FULL ADDRESS IN MONEY:
#   POBOX1	INTERVIEWER: DID RESPONDENT GIVE A STREET ADDRESS OR A PO BOX?
#   
#   1	Street address
#   2	PO BOX
#   
#   
#   Thank you very much for your time. This survey is being conducted by the Pew Research Center, which will be issuing a report on the results of this survey on their website, pewresearch dot ORG, in the coming weeks. 
#   
#   I HEREBY ATTEST THAT THIS IS A TRUE AND HONEST INTERVIEW. 
#   INTERVIEWER GENDER:
#   ISEX 
#   
#   1	Male
#   2	Female
#   
#   INTERVIEWER RACE:
#   IHISP1	Are you, yourself, of Hispanic origin or descent, such as Mexican, Puerto Rican, Cuban, or some other Spanish background? 
#   
#   1	Yes
#   2	No 
#   9	Don't know/Refused (VOL.) 
#   
#   
#   IRACE1	Which of the following describes your race? You can select as many as apply. 
#   [READ LIST. RECORD UP TO FOUR RESPONSES IN ORDER MENTIONED] 
#   
#   1	White 
#   2	Black or African-American
#   3	Asian or Asian-American 
#   4	Or some other race
#   9	[VOL. DO NOT READ] Don't know/Refused 
#   
#   [PLEASE MAKE THE FOLLOWING TEXT AVAILABLE TO INTERVIEWERS ANYTIME A RESPONDENT ASKS ABOUT THE NATURE OF THE PEW RESEARCH CENTER]The Pew Research Center is an independent nonpartisan public opinion research organization that studies attitudes toward politics, the press and issues facing the nation. The Center has no connection to the government, political parties, or any campaigns. Reports about its surveys are made available free of charge on their website pewresearch dot ORG.
#   
#   