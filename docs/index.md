# Introduction

The focus of this repository is to take you through a series of *text-mining* methods 
in Finance with an ultimate objective of conducting *sentiment analysis*.

In particular, we'll be exploring these methods on a collection of earnings 
calls transcripts that were made available to us by [Motley Fool](https://www.fool.com), 
an online financial advisor. It is not known when then earnings announcement actually 
took place, but it is assumed that the calls occured either at the same time or 
right after the actual earnings announcements. Essentially, the objective of an 
earnings call is to provide investors with information regarding a company's past 
performance and future plans/projects, as well as answering questions from the media 
and related personnel. With company executives discussing pertinent matters 
related to the company's financial performace and general outlook. On a more 
technical note, earnings calls play a role in updating investors beliefs towards a 
given company, therefore allowing them to be better informed when making 
investment decisions. For this very reason, earnings calls are particularly valuable 
for investors when relevant prior non-public information is discussed.

<img src="FsdocsImages\motley_fool_front_page.png" width="70%" >

The field of text-mining is incredibly diverse and can be segmented into seven practice areas:

1.	Search and information retrieval

2.	Document clustering

3.	Document classification 

4.	Web mining

5.	Information extraction

6.	Natural Language Processing (NLP)

7.	Concept extraction

While all seven practice areas are very much in use in differing areas within 
economics and finance, a significant amount of research today is focused on 
document classification, information retrieval and NLP. 

*Sentiment analysis* is considered to fall under the umbrella of 
document classification. In a nutshell, it seeks to determine general sentiments,
opinions, and affective states of people reflected in a corpus of text.

Some well-known classification algorithms: 

- Naive Bayes
- Logistic Regression
- Decision trees
- Neural Network