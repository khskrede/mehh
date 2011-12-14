
import os
from subprocess import call

results = {}

def do_test(test_dir):
    print ""
    print "running test: ", test_dir
    print "-----------------------------------"
    print ""

    test = "./tests/" + test_dir + "/" + test_dir

    ghc = "ghc"
    core2js = os.path.realpath("./core/core2js")
    pypy = "pypy"
    main = os.path.realpath("./main.py")

    hs = test + ".hs"
    hcr = test + ".hcr"
    hcj = test + ".hcj"

    print "-- Making external core using ghc"
    # make extcore, force recompilation, make no binary
    make_hcr = [ghc, \
               "-fext-core", \
               "-fforce-recom", \
               "-fno-code", \
               hs]
    call(make_hcr)


    print "-- Making JSCore using core2js"
    make_hcj = [core2js, \
                hcr, \
                hcj]
    call(make_hcj)


    print "-- Running program"
    run_hcj = [pypy, \
               "./main.py", \
               hcj]
    res = call(run_hcj)

    if res == 0:
        results[test_dir] = "Success"
    else:
        results[test_dir] = "Failure"

def main():

    # COMPILE core2js HERE!
   
    # Run all tests 
    for dirname in os.listdir("./tests/"):
        do_test( dirname )

    # Print results
    print ""
    print "--------------------"
    print "RESULTS"
    print "--------------------"
    print ""

    for key, value in results.items():
        print key, ": ", value

if __name__ == "__main__":
    main()

