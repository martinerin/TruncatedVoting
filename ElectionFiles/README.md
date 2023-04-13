# Truncated Voting
The code used for getting truncated voting results for our paper An Empirical Analysis of the Effect of Ballot Truncation on Ranked-Choice Electoral Outcomes by Mallory Dickerson, Erin Martin, and David McCune

## Data

Dr. McCune managed to collect 1183 ranked-choice voting (RCV) elections that we used in this project.  
* 23 elections were from the American Psychological Association (APA) which were all single-winner elections
* 82 American political elections of which 80 were single-winner elections
* 1079 Scottish local government elections of which 29 were single-winner elections and 1049 were multi-winner elections

## Questions

We wanted to answer the following questions using the election data:
1. For a fixed truncation level $TL$, what percentage of elections satisfying $TL < n-1$ have the property that the RCV winner when using $TL$ is different from the RCV winner when preferences are not truncated?
2. For $1 \leq k \leq n-1$, what percentage of elections have $k$ different winners as we increase $TL$ from $1$ to $n-1$?
3. As we increase the truncation level, does the likelihood of electing the Condorcet winner increase?

### Answer 1

For the answer to question 1, see the files in the Answer1 folder.  This gives a few sample election files as well as the code we used to find the answer.

### Answer 2

For the answer to question 2, see the files in the Answer2 folder.  This gives the code we used to find the answer and our results.

### Ballot Sampling

We ran simulations to investigate the possible effects of ballot truncation in data like ours.  See the files in the BallotSampling folder for our code that generated new elections by sampling our ballots with replacement.  For this method, we generated 1000 new elections (pseudoprofiles) by
1. randomly sampling $\min \{ 1001, \mbox{number of voters} \}$ ballots from each election with replacement, and
2. calculating the number of different RCV winners in the pseudoprofile as we vary TL from $1$ to $n-1$.

Then from the 1000 pseudoprofiles, we found the maximum number of different RCV winners as we vary the TL.