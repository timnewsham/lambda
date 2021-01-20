#!/usr/bin/env python

import string

trace = False
#trace = True

class Error(Exception) : pass

numchars = string.digits
idchars = string.digits + string.letters + "_"
toks = '\\()[]'
def lex(s) :
    while 1 :
        s,val,n = s.strip(),0,0
        if s == '' :
            break
        elif s[0] in toks :
            n = 1
            yield s[0],s[0]
        elif s[0] in numchars+'-' :
            f = 1
            if s[0] == '-' :
                f = -1
                n += 1
            while n < len(s) :
                if s[n] not in numchars :
                    break
                val = val*10 + ord(s[n]) - 0x30
                n += 1
            yield 'num', f*val
        elif s[0] in idchars :
            while n < len(s) :
                if s[n] not in idchars :
                    break
                n += 1
            yield 'id',s[:n]
        elif s[0] == '#' :
            while n < len(s) :
                if s[n] == '\n' :
                    break
                n += 1
        else :
            raise Error("lex unexpected: %s" % s[:20])
        s = s[n:]
    yield None,None

class Parse(object) :
    def __init__(self, l) :
        self.l = l
        self.tok = 'dummy'
        self.next()
    def next(self) :
        if self.tok is not None :
            self.tok, self.val = self.l.next()
    def error(self, msg) :
        r = [self.val]
        try :
            for n in xrange(5) :
                r.append(self.l.next()[1])
        except StopIteration :
            pass
        raise Error("parse: %s before %s" % (msg, r))
    def expect(self, v) :
        if self.tok != v :
            self.error("expected %s" % v)
        val = self.val
        self.next()
        return val
    def parseList(self) :
        r = []
        while self.tok in ['\\', '(', 'id', 'num', '['] :
            r.append(self._parse(False))
        return r
    def _parse(self, allowApply=True) :
        if self.tok == '(' :
            self.next()
            r = self._parse()
            self.expect(')')
        elif self.tok == '\\' :
            self.next()
            name = self.expect('id')
            r = (name, self.parseList())
        elif self.tok == '[' :
            self.next()
            name = self.expect('id')
            if name not in macros :
                self.error("unknown macro %s" % name)
            args = self.parseList()
            self.expect(']')
            n,f = macros[name]
            if n != len(args) :
                self.error("%s requires %d args, got %s" % (name, n, args))
            r = f(*args)
        elif self.tok == 'id' :
            r = self.val
            self.next()
        elif self.tok == 'num' :
            r = self.val
            self.next()
        else :
            self.error('unexpected')
        if allowApply and self.tok in ['\\', '(', 'id', 'num', '[']:
            r = [r] + self.parseList()
        return r
    def parse(self) :
        r = self._parse()
        self.expect(None)
        return filter(None, r)
def parse(s) :
    return Parse(lex(s)).parse()

def builtinTrace(id, x) :
    x = eval(x)
    print '%s: %s' % (id, show(x))
    return x
def builtinSeq(x, y) :
    eval(x)
    return y
def mkBool(x) :
    if x :
        return ('x', ('y', 'y'))
    return ('x', ('y', 'x'))

class Builtin(object) :
    builtins = {
        # name -> (nargs, func)
        'add': (2, lambda x,y : eval(x) + eval(y)),
        'mul': (2, lambda x,y : eval(x) * eval(y)),
        'div': (2, lambda x,y : eval(x) / eval(y)),
        'mod': (2, lambda x,y : eval(x) % eval(y)),
        'eq':  (2, lambda x,y : mkBool(eval(x) == eval(y))),
        'lt':  (2, lambda x,y : mkBool(eval(x) < eval(y))),
        'seq': (2, builtinSeq),
        'trace': (2, builtinTrace),
    }
    def __init__(self, b) :
        if isinstance(b, Builtin) :
            self.name,self.func,self.nargs,self.args = b.name,b.func,b.nargs,b.args
        else :
            if b not in self.builtins :
                raise Error("unknown symbol %s" % b)
            self.name,self.args = b,[]
            self.nargs, self.func = self.builtins[b]
    def setArgs(self, args) :
        self.args = args
        return self
    def apply(self, arg) :
        args = self.args + [arg]
        if len(args) == self.nargs :
            return self.func(*args)
        return Builtin(self).setArgs(args)
    def __str__(self) :
        return '(Builtin %s%s)' % (self.name, self.args)

def defmacro(name, nargs, body) :
    if not isinstance(name, str) or not isinstance(nargs, int) :
        raise Error("bad args for defmacro: %s %s %s" % (name,nargs,body))
    for n in xrange(nargs) :    # give place holders impossible names
        body = expand(body, 'arg%d' % n, '$arg%d$' % n, macro=True)
    macros[name] = (nargs, lambda *args : expandMacro(nargs, body, args))
    return None
def expandMacro(nargs, body, args) :
    #print 'expand', body,
    for n in xrange(nargs) :
        body = expand(body, '$arg%d$' % n, args[n], macro=True)
    #print '->', body
    return body
macros = {
    # name -> (nargs, func)
    'defmacro': (3, defmacro),
}

def expand(body, var, val, macro=False) :
    if isinstance(body, list) :
        return [expand(x, var, val, macro) for x in body]
    elif isinstance(body, str) and body == var :
        return val
    elif isinstance(body, tuple) and body[0] != var :
        return body[0], expand(body[1], var, val, macro)
    elif isinstance(body, tuple) and macro :
        return expand(body[0], var, val, macro), expand(body[1], var, val, macro)
    else :
        return body

def apply(f, a) :
    if isinstance(f, str) :
        f = Builtin(f)
    if isinstance(f, tuple) :
        var,body = f
        return expand(body, var, a)
    elif isinstance(f, Builtin) :
        return f.apply(a)
    else :
        raise Error("Can't apply %s to %s" % (f, a))

def eval(x) :
    #if trace : print 'eval:', x
    if isinstance(x, list) :
        if not x :
            raise Error("empty eval")
        f = eval(x[0])
        for a in x[1:] :
            f = eval(apply(f, a))
        r = eval(f)
    else :
        r = x
    if trace : print "eval %s -> %s" % (show(x), show(r))
    return r

def show(x) :
    if isinstance(x, list) :
        return '(%s)' % ' '.join(map(show, x))
    elif isinstance(x, tuple) :
        return '(\\%s %s)' % (show(x[0]), show(x[1]))
    else :
        return str(x)

def runFile(fn, usePrelude) :
    d = file(fn).read()
    if usePrelude :
        d = file('prelude.lam').read() + d
    if trace: print "Source:\n", d
    try :
        tree = parse(d)
        if trace : print "Raw\n", show(tree), '\n'
        ret = eval(tree)
        print "Result:", show(ret)
    except Error,e :
        print e

def main() :
    import sys
    flags = filter(lambda a : a[0] == '-', sys.argv[1:])
    fns = filter(lambda a : a[0] != '-', sys.argv[1:])
    for fn in fns :
        runFile(fn, '-n' not in flags)
    print "done."

if __name__ == '__main__' :
    main()

