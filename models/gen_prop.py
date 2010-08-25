class FloatProp(str):
    def parse(self, value):
        return float(value)

def node_dist(node, n):
    return [FloatProp("S=? [ node%s=%s ]" % (node,i)) for i in range(0,n)]

def node_past(node, n):
    return [FloatProp("S=? [ next_past=%s & node%s=%s & node%s_past=%s ]" % (node,node,i,node,j)) for i in range(0,n) for j in range(0,n)]

def node_inter(node0, node1, n):
    return [FloatProp("S=? [ node%s=%s & node%s=%s ]" % (node0,i,node1,j)) for i in range(0,n) for j in range(0,n)]




