import prism 
from gen_model import *
from gen_prop import *

def equalish(values, tolerance=0.0):
    return (max(values) - min(values) <= tolerance)

def test_equalish(results, tolerance=0.0):
    if equalish(results, tolerance=tolerance):
        print 'Passed'
        return True
    else:
        print '-' * 40
        print 'Failed:'
        print results
        print tolerance
        print '-' * 40
        return False

def base_tests(model_gen):
    print "Base tests (model checker)"
    for n in [3,5]:
        model = model_gen(n)
        for node in range(0,n):
            test_equalish(prism.run(model, node_dist(node, n)), tolerance=0.001)
            for node2 in range(0,n):
                if node2 != node:
                    test_equalish(prism.run(model, node_inter(node, node2, n)), tolerance=0.001)

def dtmc_tests(model_gen):
    print "DTMC tests (model checker)"
    for n in [3,5]:
        model = model_gen(n)
        for node in range(0,n):
            test_equalish(prism.run(model, node_past(node, n)), tolerance=0.001)

def base_tests_sim(model_gen):
    print "Base tests (simulator)"
    for n in [10,20]:
        model = model_gen(n)
        #for node in range(0,n):
        node = 1
        test_equalish(prism.sim(model+node_dist_reward(node, n), node_dist_r(node, n)), tolerance=0.02)
        #for node2 in range(0,n):
        node2 = 2    
        #if node2 != node:
        test_equalish(prism.sim(model+node_inter_reward(node, node2, n), node_inter_r(node, node2, n)), tolerance=0.02)

def dtmc_tests_sim(model_gen):
    print "DTMC tests (simulator)"
    for n in [10,20]:
        model = model_gen(n)
        #for node in range(0,n):
        node = 1
        test_equalish(prism.sim(model+node_past_reward(node, n), node_past_r(node, n)), tolerance=0.02)

def remove_roots(props, n):
    """Testing message failure requires removing reference to the root node from the list of properties.
    We do this in the most hackish way possible."""
    # remove props on root
    props = props[n:]
    # remove props referring to root
    result = []
    for i in range(1,n):
        result.extend(props[i::n])
    return result

def error_tests():
    print "Error tests (model checker)"
    for p in [0.01, 0.1]:
        node = 1
        node2 = 2
        for n in [3,5]:
            model = ctmc_full_error(n)
            test_equalish(prism.run(model, node_dist(node, n))[1:], tolerance=0.02)
            test_equalish(prism.run(model, remove_roots(node_inter(node, node2, n),n)), tolerance=0.02)
        for n in [10,20]:
            model = ctmc_full_error(n)
            test_equalish(prism.sim(model+node_dist_reward(node, n), node_dist_r(node, n)[1:]), tolerance=0.02)
            test_equalish(prism.sim(model+node_inter_reward(node, node2, n), remove_roots(node_inter_r(node, node2, n))), tolerance=0.02)
            

ctmcs = [ctmc_single, ctmc_multiple, ctmc_full]
dtmcs = [dtmc_single, dtmc_multiple, dtmc_full]

def all_tests():
    for model in dtmcs + ctmcs:
        base_tests(model)
        base_tests_sim(model)
    for model in dtmcs:
        dtmc_tests(model)
        dtmc_test_sim(model)
    error_tests()

if __name__ == '__main__':
    #all_tests()
    error_tests()
