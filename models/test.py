import prism 
from gen_model import *
from gen_prop import *

def equalish(values, tolerance=0.0):
    return (max(values) - min(values) <= tolerance)

def test_equalish(model, props, tolerance=0.0):
    results = prism.run(model, props)
    if equalish(results, tolerance=tolerance):
        print 'Passed'
        return True
    else:
        print '-' * 40
        print 'Failed:'
        print results
        print props
        print model
        print '-' * 40
        return False

def base_tests(model_gen):
    for n in [3]:
        model = model_gen(n)
        for node in range(0,n):
            test_equalish(model, node_dist(node, n), tolerance=0.001)
            for node2 in range(0,n):
                if node2 != node:
                    test_equalish(model, node_inter(node, node2, n), tolerance=0.001)

def dtmc_tests(model_gen):
    for n in [3]:
        model = model_gen(n)
        for node in range(0,n):
            test_equalish(model, node_past(node, n), tolerance=0.001)

ctmcs = [ctmc_single, ctmc_multiple, ctmc_full]
dtmcs = [dtmc_single, dtmc_multiple, dtmc_full]

def all_tests():
    for model in dtmcs + ctmcs:
        base_tests(model)
    for model in dtmcs:
        dtmc_tests(model)
