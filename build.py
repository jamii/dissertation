from models.gen_model import *

def write_model(name, contents):
    with open('models/' + name + '.sm', 'w') as file:
        file.write(contents)
    with open('writeup/' + name + '.sm', 'w') as file:
        file.write(contents)

def main():
    write_model("dtmc_single_no_past", dtmc_single(3, use_past=False))
    write_model("dtmc_single", dtmc_single(3))
    write_model("dtmc_multiple", dtmc_multiple(3))
    write_model("dtmc_broken", dtmc_broken(3))
    write_model("dtmc_full", dtmc_full(3))
    write_model("ctmc_single", ctmc_single(3))
    write_model("ctmc_multiple", ctmc_multiple(3))
    write_model("ctmc_broken", ctmc_broken(3))
    write_model("ctmc_full", ctmc_full(3))
    write_model("ctmc_full_error", ctmc_full_error(3))

if __name__ == '__main__':
    main()
