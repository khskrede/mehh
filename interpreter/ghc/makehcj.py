#!/usr/bin/python2.7

import fnmatch
import sys
import os
from subprocess import call
 
rootPath = './'
pattern = '*.hcr'
 
def make_hcj(_file):

    core2js = "./../core/core2js"

    hcr = _file
    hcj = _file.replace(".hcr",".hcj")
    print hcr, " -> ", hcj

    make_hcj = [core2js, \
                hcr, \
                hcj]
    call(make_hcj)

def main():

    print "Making hcj files from hcr files"
    print "-----------------------------------"
    print ""

    for root, dirs, files in os.walk(rootPath):
        for filename in fnmatch.filter(files, pattern):
            make_hcj( os.path.join(root, filename) )

if __name__ == "__main__":
    main()

