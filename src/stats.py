from scipy import stats

def read_log(log_file):
    with open(log_file) as log:
        for line in log:
            words = line.split(' ')
            if len(words) == 2:
                yield line.split(' ')[1]

def distribution(log_file):
    nodes = read_log(log_file)
    dist = {}
    for node in nodes:
        dist[node] = dist.get(node, 0) + 1
    return dist

def pair_distribution(log_file):
    nodes = read_log(log_file)
    dist = {}
    last = None
    for node in nodes:
        if last:
            dist[(last,node)] = dist.get((last,node), 0) + 1
        last = node
    return dist

# do pair counts incrementally because the memory consumption for large networks is prohibitive
def pair_count(log_file, past, current):
    nodes = read_log(log_file)
    last = None
    count = 0
    for node in nodes:
        if (last == past) and (node == current):
            count += 1
        last = node
    return count

def test_uniform(dist, n):
    total = sum(dist.values())
    expected = float(total) / n
    test = sum([((float(observed) - expected) ** 2) / expected for observed in dist.values()])
    # account for cells with 0 frequency
    test += (n - len(dist)) * expected
    pval = 1 - stats.chi2.cdf(test, n-1)
    return pval

def test_independent(log_file, dist):
    n = len(dist)
    total = float(sum(dist.values()))
    def expected(past, current):
        return float(dist[past]) * float(dist[current]) / total
    def test_one(past, current):
        ex = expected(past, current)
        ob = float(pair_count(log_file, past, current))
        return (ob-ex)**2 / ex
    test = sum([test_one(past, current) for past in dist.keys() for current in dist.keys()])
    pval = 1 - stats.chi2.cdf(test, (n-1)**2)
    return pval

def main():
    for (n,k) in [(10,1), (1000,10), (100000,100)]:
        log_file = ('experiment_%s_%s' % (n,k))
        size = n+k
        dist = distribution(log_file)
        print n, k, 'uniform (chi squared)', test_uniform(dist, size)
        print n, k, 'independent (chi squared)', test_independent(log_file, dist)
