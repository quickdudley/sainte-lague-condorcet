Sainte-Lague+Condorcet
======================

A new voting system which combines the Sainte-Laguë method with ranked pairs.

The procedure in English:
-------------------------

Tallying votes. The result of the tally can be written as a table with ordered pairs of parties on the left and arithmetic expressions on the right. The expressions may be simplified as they are built up but variables are not substituted until step 2

  Step 0: A placeholder variable is assigned to each party
  Step 1: Repeat for each ballot:
    Step 1.1: Sort parties on the ballot according to rank.
    Step 1.2: For each party p on the ballot: q(p) = 1 / (2n + 1) where n is the sum of the placeholder variables assigned to each party ranked equal or higher.
    Step 1.3: For each pair of parties (a,b) where a is ranked better than b or a is ranked and b is unranked: find the entry for (a,b) on the tally table and add q(a)

Assigning seats

  Step 2: Repeat until all the seats have been assigned
    Step 2.1: Temporarily substitute each party's placeholder variable with the number of seats currently assigned to it and evaluate to find each pair's score.
    Step 2.2: Make a list of pairs of parties, sorted by difference in score
    Step 2.3: Build a directed graph by adding edges in the order defined in step 2.2, skipping any edge that would create a cycle.
    Step 2.4: Starting at the first edge added to the graph: follow the edges from loser to winner until a node with no outgoing edges is reached
    Step 2.5: Assign one seat to the party associated with that node.

Using the program:
------------------

The program expects 2 arguments: the first is the filename for the ballot data in csv format, the second is the total number of seats to allocate.
The ballot data should be formatted in the same manner as test.csv. The test data does not have any blank cells, but they are allowed. Extra spaces are allowed but not tabs.

Requirements:
-------------

http://hackage.haskell.org/package/numbers is required to build.

Known Issues
------------

The library I'm using for symbolic numbers doesn't do any simplification of expressions, which can affect performance if there's a large number of ballots.
