# Truncated Voting
The code used for getting truncated voting results for our paper An Empirical Analysis of the Effect of Ballot Truncation on Ranked-Choice Electoral Outcomes by Mallory Dickerson, Erin Martin, and David McCune

## Data

Dr. McCune managed to collect 1183 ranked-choice voting (RCV) elections that we used in this project.  
* 23 elections were from the American Psychological Association (APA) which were all single-winner elections
* 82 American political elections of which 80 were single-winner elections
* 1078 Scottish local government elections of which 29 were single-winner elections and 1049 were multi-winner elections

## Questions

We answered the following questions using the election data:
1. For a fixed truncation level $TL$, what percentage of elections satisfying $TL < n-1$ have the property that the RCV winner when using $TL$ is different from the RCV winner when preferences are not truncated?
2. For $1 \leq k \leq n-1$, what percentage of elections have $k$ different winners as we increase $TL$ from $1$ to $n-1$?
3. As we increase the truncation level, does the likelihood of electing the Condorcet winner increase?

### Folders

The following folders are in this repository:
* ElectionFiles:  This contains 5 of the 1183 election data files
* Answer1:  This contains the code used to answer the first question
* Answer2:  This contains the code used to answer the second question
* BallotSampling:  This contains the code for the ballot sampling we did

