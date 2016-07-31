import sys, os, os.path

from runtime import OK, WARN, ERR

def construct(directory):
    concat = lambda l: [x for y in l for x in y]
    exts = ['.py', '.c', '.cpp', '.hs']
    comment_chars = [u'#', u'//', u'--']
    denoter = u'@'

    out = 'from runtime import *\n'
    out += '\n#>> BEGIN GENERATED CODE\n\n'

    for dirpath, dirname, files in os.walk(directory):
        for f in files:
            if any(f.endswith(e) for e in exts) and "%" not in f:
                path = os.path.join(directory, dirpath, f)
                temp = ''
                try:
                  lines = open(path, 'rb').readlines()
                except IOError:
                  continue
                for line_no, line in enumerate(lines, start = 1):
                    try:
                      l = line.decode('utf-8').lstrip()
                    except UnicodeDecodeError as e:
                      print("ERROR in", path, ", on line",
                            line_no, ":", line)
                    if any(l.startswith(cc + denoter) and l[len(cc) + len(denoter)] in (u' ', u'W', u'E') for cc in comment_chars):
                        l = l[l.index(denoter) + len(denoter):] # Move past comment and test denoter
                        if l[0] == 'W':
                            on_error = WARN
                        else:
                            on_error = ERR
                        l = l[1:].lstrip(' ') # Move past error/warning label
                        try:
                            name = l[:l.index('=')].strip()
                            expr = l[l.index('=') + 1:].strip()
                            raw = expr.replace('\'', '\\\'')
                            temp += """define('{0}', lambda: {1}, '{2}', on_error={3})\n""".format(
                                name, expr, raw, on_error)
                        except ValueError:
                            print ("Invalid system check test: " + path + ", line " + str(line_no))
                            print ("\t" + line)
                if temp:
                    out += '#<< BEGIN FILE: {0}\n\n{1}\n#<< END FILE {0}\n\n\n'.format(path, temp)


    out += '#>> END GENERATED CODE\n'

    open('{0}/system_check/gen.py'.format(directory), 'wb').write(out.encode('utf-8'))
