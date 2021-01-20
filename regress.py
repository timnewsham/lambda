#!/usr/bin/env python

import os

def regfile(fn) :
    return 'reg-data/' + fn

update = False
#update = True

fns = (
    #'error.lam',
    'fact.lam',
    'fact2.lam',
    'hanoi-old.lam',
    'hanoi.lam',
    'main.lam',
    'prelude.lam',
    'primes.lam',
    'primes2.lam',
    'testmacro.lam',
    'tests.lam',
)

for fn in fns :
    mfn = 'reg-data/' + fn + '.out'
    if update :
        ofn = mfn
    else :
        ofn = 'reg-data/tmp'
    os.system('./lamb.py %s > %s' % (fn, ofn))
    if os.system('diff -u %s %s' % (mfn, ofn)) :
        print '%-30s failed' % fn
    else :
        print '%-30s passed' % fn

