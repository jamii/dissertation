header = """
ctmc

const double forget = 1.0;
const double cache;
const double push_self;
const double push;
const double pull;

"""

def init(n):
    for i in range(0,n):
        for j in range(0,n):
            if i!=j:
                yield ("edge_%d_%d : [0..1] init 0;" % (i,j))

def forget(n):
    for i in range(0,n):
        for j in range(0,n):
            if i!=j:
                yield ("[] (forget>0) & (edge_%d_%d=1) -> forget:(edge_%d_%d'=0);" % (i,j,i,j))

def cache(n):
    for i in range(0,n):
        for j in range(0,n):
            if i!=j:
                yield ("[] (cache>0) & (edge_%d_%d=0) -> cache:(edge_%d_%d'=1);" % (i,j,i,j))

def push_self(n):
    for i in range(0,n):
        for j in range(0,n):
            if i!=j:
                yield("[] (push_self>0) & (edge_%d_%d=1) -> push_self:(edge_%d_%d'=1);" % (i,j,j,i))

# push and pull may need to be adjusted for implementation - currently proportional to number of possible 2 edge combinations

def push(n):
    for i in range(0,n):
        for j in range(0,n):
            for k in range(0,n):
                if i!=j and i!=k and j!=k:
                    yield ("[] (push>0) & (edge_%d_%d=1) & (edge_%d_%d=1) -> push:(edge_%d_%d'=1);" % (i,j,i,k,j,k))

def pull(n):
    for i in range(0,n):
        for j in range(0,n):
            for k in range(0,n):
                if i!=j and i!=k and j!=k:
                    yield ("[] (pull>0) & (edge_%d_%d=1) & (edge_%d_%d=1) -> pull:(edge_%d_%d'=1);" % (i,j,j,k,i,k))

def time():
    return """
        rewards "time"
            true:1;
        endrewards
    """

def indegree(n):
    edges_in = ["edge_%d_0" % i for i in range(1,n)]
    return """
        rewards "indegree"
            true:%s;
        endrewards
    """ % " + ".join(edges_in)

def outdegree(n):
    edges_out = ["edge_0_%d" % i for i in range(1,n)]
    return """
        rewards "outdegree"
            true:%s;
        endrewards
    """ % " + ".join(edges_out)

def density(n):
    edges = []
    for i in range(0,n):
        for j in range(0,n):
            if i!=j:
                edges.append("edge_%d_%d" % (i,j))
    return """
        rewards "density"
            true:%s;
        endrewards
    """ % " + ".join(edges)

def outfail(n):
    edges_out = ["(edge_0_%d=0)" % i for i in range(1,n)]
    return """
        rewards "outfail"
            true:(%s) ? 1 : 0;
        endrewards
    """ % " & ".join(edges_out)

def main(n):
    return "\n".join(
        [header, "module edges"] +
        list(init(n)) +
        list(forget(n)) +
        list(cache(n)) +
        list(push_self(n)) +
        list(push(n)) +
        list(pull(n)) +
        ["endmodule"] +
        [time()] +
        [indegree(n)] +
        [outdegree(n)] +
        [density(n)] +
        [outfail(n)]
    )

if __name__ == '__main__':
    print main(5)
          
