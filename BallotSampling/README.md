# Ballot Sampling

We ran simulations to investigate the possible effects of ballot truncation in data like ours.  For this method, we generated 1000 new elections (pseudoprofiles) for each of our elections by
1. randomly sampling $\min \{ 1001, number of voters \}$ ballots from each election with replacement, and
2. calculating the number of different RCV winners in the pseudoprofile as we vary TL from $1$ to $n-1$.

Then from the 1000 pseudoprofiles, we found the maximum number of different RCV winners as we vary the TL.
