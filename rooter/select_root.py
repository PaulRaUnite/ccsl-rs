import sys

import networkx as nx

if __name__ == '__main__':
    g = nx.parse_edgelist(sys.stdin, create_using=nx.DiGraph)

    t = nx.minimum_spanning_arborescence(g, default=0)
    for n, d in t.in_degree():
        if d == 0:
            print(n)
