import commands
import re

result_re = re.compile(r"Result \(.*?\): (\S*)") 

class PrismError(Exception):
    def __init__(self, reason):
        self.reason = reason

    def __str__(self):
        return "PrismError:\n" + self.reason

def scrape_result(result, prop):
    match = result_re.search(result)
    if not match:
        raise PrismError("Couldnt match result\n" + result)
    value = match.group(1)
    try:
        return prop.parse(value)
    except Exception, exc:
        raise PrismError('\n\n'.join(["Parse failed", str(exc), prop, result]))

def run(model, props):
    with open('model.sm', 'w') as file:
        file.write(model)
    with open('props.pctl', 'w') as file:
        file.write('\n'.join(map(str,props)))
    output = commands.getoutput("prism model.sm props.pctl")
    results = output.split('-' * 43)[2:]
    if len(results) != len(props):
        raise PrismError("Results mismatch\n\n" + output)
    return [scrape_result(result, prop) for (result, prop) in zip(results,props)]
