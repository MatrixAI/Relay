The first line of a composition file needs to declare the names of the
automatons, separate by white space.
eg. A B C
    apple banana crumpet

The second line indicates how many of each automaton we want separate by spaces.
eg. 1 2 3

The next lines show the compositions. Each composition is restricted to
interactions between only TWO automatons. Each composition needs to be on a new
line with spaces between compositions. In between each automaton name, there
needs to be a composition operator which will be listed below.
eg. A -> B -> C
    needs to be written as
    A -> B
    B -> C

    apple => banana

The composition operators that this experiment will deal wtih are:
o ->
  Message from left to right
o =>
Message from left to right and response from right to left

###############################################################################

We need a graph structure which is able to be modified with low overhead as we
treat automatons as ephemeral. This probably means we can't represent automaton
instances in a graph with a tensor. I also don't know how useful matrix
operations will be
