class FloatProp(str):
    def parse(self, value):
        return float(value)

def node_dist(node, n):
    return [FloatProp("S=? [ node%s=%s ]" % (node,i)) for i in range(0,n)]

def node_past(node, n):
    return [FloatProp("S=? [ next_past=%s & node%s=%s & node%s_past=%s ]" % (node,node,i,node,j)) for i in range(0,n) for j in range(0,n)]

def node_inter(node0, node1, n):
    return [FloatProp("S=? [ node%s=%s & node%s=%s ]" % (node0,i,node1,j)) for i in range(0,n) for j in range(0,n)]

# --- Same as above, but reward based so we can do simulation ---

def node_dist_reward(node, n):
    reward = """
rewards "node%s_%s"
  node%s=%s : 1;
endrewards
"""
    return ''.join([reward % (node,i,node,i) for i in range(0,n)])

def node_dist_r(node, n):
    return [FloatProp('R{"node%s_%s"}=? [ I=T ]' % (node,i)) for i in range(0,n)]

def node_past_reward(node, n):
    reward = """
rewards "node%s_past_%s_%s"
  (node%s=%s) & (node%s_past=%s) : 1;
endrewards
"""
    return ''.join([reward % (node,i,j,node,i,node,j) for i in range(0,n) for j in range(0,n)])

def node_past_r(node, n):
    return [FloatProp('R{"node%s_past_%s_%s"}=? [ I=T ]' % (node,i,j)) for i in range(0,n) for j in range(0,n)]

def node_inter_reward(node0, node1, n):
    reward = """
rewards "node%s_%s_inter_%s_%s"
  (node%s=%s) & (node%s=%s) : 1;
endrewards
"""
    return ''.join([reward % (node0,node1,i,j,node0,i,node1,j) for i in range(0,n) for j in range(0,n)])

def node_inter_r(node0, node1, n):
    return [FloatProp('R{"node%s_%s_inter_%s_%s"}=? [ I=T ]' % (node0,node1,i,j)) for i in range(0,n) for j in range(0,n)]




