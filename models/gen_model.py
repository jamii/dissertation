def next(n):
    choices = ' + '.join(["(1/n):(next'=%s)" % i for i in range(0,n)])
    return """
module next
  next : [0..n-1] init 0;
  
  [next] true -> %s;
endmodule
""" % choices

def poppi_past(n):
    nodes = '\n'.join(["  node%s_past : [0..n-1] init 0;" % i for i in range(0,n)])
    updates = '&'.join(["(node%s_past'=node%s)" % (i,i) for i in range(0,n)])
    return """
module poppi_past
  next_past : [0..n-1] init 0;
  
%s

  [next] true -> (next_past'=next)&%s;
endmodule
""" % (nodes, updates)

def poppi(k, n, transitions):
    if k == 1:
        roots = "  root : [0..n-1] init 0;"
    else:
        roots = '\n'.join(["  root%s : [0..n-1] init 0;" % i for i in range(0,k)])
    nodes = '\n'.join(["  node%s : [0..n-1] init 0;" % i for i in range(0,n)])
    return """
module poppi
%s

%s
%s
endmodule
""" % (roots, nodes, transitions)

def dtmc(k, n, constants, transitions, use_next=True):
    if use_next:
        next_string = next(n)
    else:
        next_string = ""
    return """
dtmc

const int n = %s;
const int k = %s; 
%s
%s
%s
%s
""" % (n, k, constants, next_string, poppi(k, n, transitions), poppi_past(n))

def dtmc_single(n, use_next=True):
    transition = """
  //node%s contacts the root and receives a new peer selection
  [next] (next=%s) -> (root'=%s)&(node%s'=root);
"""
    transitions = ''.join([transition % (i,i,i,i) for i in range(0,n)])
    return dtmc(1, n, "", transitions, use_next=use_next)

def dtmc_multiple(n,k=2):
    clause = "(1/k):(root%s'=%s)&(node%s'=root%s)"
    def clauses(node):
        return ' + '.join([clause % (root,node,node,root) for root in range(0,k)])
    transition = """
  //node%s contacts the root and receives a new peer selection
  [next] (next=%s) -> %s;
"""
    transitions = ''.join([transition % (node,node,clauses(node)) for node in range(0,n)])
    return dtmc(k, n, "", transitions)

def dtmc_broken(n):
    clause = "  [next] (next=%s)&(node%s=%s) -> (root%s'=%s)&(node%s'=root%s);"
    def clauses(node):
        return '\n'.join([clause % (node,node,root,root,node,node,root) for root in range(0,n)])
    transition = """
  //node%s contacts the root and receives a new peer selection
%s
"""
    transitions = ''.join([transition % (node, clauses(node)) for node in range(0,n)])
    return dtmc(n, n, "", transitions)

def dtmc_full(n, mu=0.01):
    constants = """
const double lambda = 1.0;
const double mu = %s;
const double node = lambda / (mu + lambda);
const double default = mu / (mu + lambda);
""" % mu
    clause = "  [next] (next=%s)&(node%s=%s) -> node:(root%s'=%s)&(node%s'=root%s) + default:(root0'=%s)&(node%s'=root0);"
    def clauses(node):
        return '\n'.join([clause % (node,node,root,root,node,node,root,node,node) for root in range(0,n)])
    transition = """
  //node%s contacts the root and receives a new peer selection
%s
"""
    transitions = ''.join([transition % (node, clauses(node)) for node in range(0,n)])
    return dtmc(n, n, constants, transitions)

def ctmc(k, n, constants, transitions):
    return """
ctmc

const int n = %s;
const int k = %s; 
const double lambda = 1.0;
%s
%s
""" % (n, k, constants, poppi(k, n, transitions))
    
def ctmc_single(n):
    transition = """
  //node%s contacts the root and receives a new peer selection
  [] true -> lambda:(root'=%s)&(node%s'=root);
"""
    transitions = ''.join([transition % (i,i,i) for i in range(0,n)])
    return ctmc(1, n, "", transitions)

def ctmc_multiple(n,k=3):
    clause = "(lambda/k):(root%s'=%s)&(node%s'=root%s)"
    def clauses(node):
        return ' + '.join([clause % (root,node,node,root) for root in range(0,k)])
    transition = """
  //node%s contacts the root and receives a new peer selection
  [] true -> %s;
"""
    transitions = ''.join([transition % (node,clauses(node)) for node in range(0,n)])
    return ctmc(k, n, "", transitions)

def ctmc_broken(n):
    clause = "  [] (node%s=%s) -> lambda:(root%s'=%s)&(node%s'=root%s);"
    def clauses(node):
        return '\n'.join([clause % (node,root,root,node,node,root) for root in range(0,n)])
    transition = """
  //node%s contacts the root and receives a new peer selection
%s
"""
    transitions = ''.join([transition % (node, clauses(node)) for node in range(0,n)])
    return ctmc(n, n, "", transitions)

def ctmc_full(n, mu=0.01):
    constants = """
const double mu = %s;
""" % mu
    clause = "  [] (node%s=%s) -> lambda:(root%s'=%s)&(node%s'=root%s);"
    def clauses(node):
        normal = '\n'.join([clause % (node,root,root,node,node,root) for root in range(0,n)])
        default =  "  [] true -> mu:(root0'=%s)&(node%s'=root0);" % (node, node)
        return normal + "\n" + default
    transition = """
  //node%s contacts the root and receives a new peer selection
%s
"""
    transitions = ''.join([transition % (node, clauses(node)) for node in range(0,n)])
    return ctmc(n, n, constants, transitions)
