# Contagion Model Documentation

The contagion model reproduces the progress of a human to human transmitted disease throgh a population distrubuted through a geographic area.

## Model structure

### Region and cells

### People

### Cycles


## Model initialization

### Populating the model

### Seeding the disease

## Running the model

### Moving people

### Updating cell virality

### Updating disease status

### End of cycle reporting

## Disease evolution

The following describes how the model treats the progress of an infectious disease.

An individual can be in one of many disease states:

- well
- infected (infectious but not symptomatic)
- symptomatic (and infectious)
- hospitalized
- dead
  
The movement of a person from one state to another can be thought of as a Markov process where given an initial disease state there is a set of probablities of moving into a new state (or staying in the same state).  These are probabilities are not necessarily well understood and the model can be used to study how they affect the progress of a disease through a population.

|               |Well           |Infected     |Symptomatic  |Hospitalized   |Dead           |
|---------------|---------------|-------------|-------------|---------------|---------------|
|**Well**       |exposure<br>virality<br>immunity|1 - Well|0|0|0
|**Infected**
|**Symptomatic**
|**Hospitalized**
|**Dead**

## Disease state transition factors

At the end of every modelling cycle each person's disease state is updated and posssibly changed to a new disease state.
This section describes the factors influencing the probability of a person moving out of one disease state into another.  

### Well

A well person can either stay well or become infected.  The probability of becoming infected is affected by their *exposure*, *immunity*, and the *virality* of the cell they currently occupy.

### Infected

An infected person can stay infected, become well become symptomatic, or perhaps die.  An infected person will not become hospitalized unless they become symptomatic first.  One critical factor affecting whether an infected person moves to another state is the number of time-cycles since infection (*days since last status change*).  The more time goes by, the greater the odds of a transition.  These odds are calculated using a cumulative Beta distribution.  The alpha and beta parameters of the distribution are influenced by a person's *vulnerability*.  

If a person does make a transition out of the infected state, they will either become well or become symptomatic.  Which of these occurs depends again on the same 2 parameters: *vulnerability* and *days since last status change*.  The less the vulnerability, the greater the odds of becoming well.  And, whatever the vulnerability, the longer a person remains in the infected state the greater the odds of becoming well.

This logic cascades one more time so that the infected person who, randomly, according to the above rules, does not remain infected or become well must consequently become symptomatic or die.  Here again the ratio bewee the two is a function of their *vulnerability* and the *days since last status change*.

### Symptomatic

A symptomatic person can stay symptomatic, become well, become hospitalized or die.  An interesting question is whether a symptomatic person can return a non-symptomatic but infected and therefore contagious state.  The model, for now, assumes not. One critical factor affecting whether an symptomatic person moves to another state is the number of time-cycles since infection (*days since last status change*).  The more time goes by, the greater the odds of a transition.  These odds are calculated using a cumulative Beta distribution.  The alpha and beta parameters of the distribution are influenced by a person's *vulnerability*.  

If a person does make a transition out of the symptomatic state, they will either become well, become hospitalized or die.  Which of these three occurs depends again on the same 2 parameters: *vulnerability* and *days since last status change*.  The less the vulnerability, the greater the odds of becoming well.  And, whatever the vulnerability, the longer a person remains in the hospitalized state the greater the odds of becoming well.

This logic cascades one more time so that the symptomatic person who, randomly, according to the above rules, does not remain symptomatic or become well must consequently become hospitalized or die.  Here again the ratio bewee the two is a function of their *vulnerability* and the *days since last status change*.
