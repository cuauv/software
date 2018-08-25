#!/usr/bin/env python3

""" Common build class for generating ninjafiles. """

import os
import subprocess

from build import ninja_syntax

PLATFORM = "linux_amd64"
GOSRC = "gocode/src/cuauv.org"
GOPKG = "gocode/pkg/%s/cuauv.org" % PLATFORM

def strip_ext(filename):
    """ Remove the extension from a filename. """
    return filename.rsplit('.', 1)[0]

def _get_path(src, ext):
    return os.path.join('$builddir', strip_ext(src) + ext)

o    = lambda src: _get_path(src, '.o')
so   = lambda src: _get_path(src, '.so')
a    = lambda src: _get_path(src, '.a')
prog = lambda src: _get_path(src, '.o')

class Build:
    def __init__(self,  wd):
        ninjafile = '%s/build.ninja' % wd
        buildfile = open(ninjafile, 'w')
        self.n = ninja_syntax.Writer(buildfile)
        self.wd = wd
        self.n.variable('builddir', '%s/binaries' % wd)
        self.code_targets = []
        self.test_targets = []
        self.check_targets = []

    def __del__(self):
        """ Run at configure.py exit to emit default targets. """
        self.n.build('code-' + self.wd, 'phony', implicit=self.code_targets)
        self.n.build('tests-' + self.wd, 'phony', implicit=self.test_targets)
        self.n.build('check-' + self.wd, 'phony', implicit=self.check_targets)

    def add_code_target(self, name):
        self.code_targets.append(name)

    def add_test_target(self, name):
        self.test_targets.append(name)

    def add_check_target(self, name):
        self.check_targets.append(name)

    def build(self, src, flags=[], deps=[], order_only=[], target_name=None):
        """ Build src into binaries/src.o. """
        variables = [['cflags', '$cflags %s' % ' '.join(flags)]]
        # Tests may build objects that are used in regular builds too
        # Rename any objects used by tests so that we don't emit multiple build rules
        # for the same object
        if target_name:
            outsrc = target_name + '.objs/' + src
        else:
            outsrc = src
        return self.n.build(o(outsrc), 'cxx', inputs=[os.path.join(self.wd, src)], variables=variables, implicit=deps, order_only=order_only)

    def build_c(self, src, flags=[], deps=[], order_only=[], target_name=None):
        """ Build src into binaries/<target_name>.objs/<src>.o. """
        variables = [['cflags', '$cflags %s' % ' '.join(flags)]]
        # Tests may build objects that are used in regular builds too
        # Rename any objects used by tests so that we don't emit multiple build rules
        # for the same object
        if target_name:
            outsrc = target_name + '.objs/' + src
        else:
            outsrc = src
        return self.n.build(o(outsrc), 'cc', inputs=[os.path.join(self.wd, src)], variables=variables, implicit=deps, order_only=order_only)

    def link_so(self, name, objs, libs = [], deps=[], flags=[]):
        """ Link cuauv objs and external libs into binaries/name.so. """
        libs = ['-l%s' % l for l in libs]
        variables = [['libs', ' '.join(libs)], ['ldflags', '$ldflags %s' % ' '.join(flags)]]
        return self.n.build(so(name), 'link_shared', inputs=objs, variables=variables, implicit=deps)

    def link_static(self, name, objs, libs = [], deps = [], flags=[]):
        """ Link cuauv objs and external libs into executable binaries/name. """
        libs = ['-l%s' % l for l in libs]
        variables = [['libs', ' '.join(libs)], ['ldflags', '$ldflags %s' % ' '.join(flags)]]
        return self.n.build(a(name), 'ar', inputs=objs, variables=variables, implicit=deps)

    def link_prog(self, name, objs, libs = [], deps = [], flags=[]):
        """ Link cuauv objs and external libs into executable binaries/name. """
        libs = ['-l%s' % l for l in libs]
        variables = [['libs', ' '.join(libs)], ['ldflags', '$ldflags %s' % ' '.join(flags)]]
        return self.n.build(prog(name), 'link', inputs=objs, variables=variables, implicit=deps)

    def install(self, install_name, f = None, is_test = False):
        """ Install f as link-stage/install_name. If f is not provided, install_name is used instead. """
        if f == None:
            f = os.path.join(self.wd, install_name)
        target_name = 'link-stage/' + install_name
        self.n.build(install_name, 'phony', target_name)
        if is_test:
            self.add_test_target(target_name)
        else:
            self.add_code_target(target_name)
        return self.n.build(target_name, 'install', f)

    def build_common(self, ctype, ltype, out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit, static_extra_deps=[], is_test=False):
        """ Build something using the provided sources and linked to any external libraries.
            Install that something in link-stage.
        """
        if ctype == 'c':
            buildfunc = self.build_c
        else:
            buildfunc = self.build

        also_static = False

        if ltype == 'shared':
            cflags = cflags + ['-fPIC']
            linkfunc = self.link_so
            target_name = 'lib' + out_name + '.so'
        elif ltype == 'static':
            cflags = cflags + ['-fPIC']
            linkfunc = self.link_static
            target_name = 'lib' + out_name + '.a'
        elif ltype == 'shared+static':
            # start with shared mode, will also link static as a last step
            cflags = cflags + ['-fPIC']
            linkfunc = self.link_so
            target_name = 'lib' + out_name + '.so'
            also_static = True
            static_linkfunc = self.link_static
            static_target_name = 'lib' + out_name + '.a'
        elif ltype == 'prog+static':
            linkfunc = self.link_prog
            target_name = out_name
            also_static = True
            static_linkfunc = self.link_prog
            static_target_name = out_name + '-static'
        else:
            linkfunc = self.link_prog
            target_name = out_name

        installfunc = self.install

        auv_dep_as=['link-stage/lib%s.a' % d for d in auv_static_deps]
        auv_dep_sos=['link-stage/lib%s.so' % d for d in auv_deps]
        for pkg_conf in pkg_confs:
            cflags.append('`pkg-config --cflags %s`' % pkg_conf)
            lflags.append('`pkg-config --libs %s`' % pkg_conf)

        # Build each source and store a flattened array of names of build objects.
        objs = sum([buildfunc(s, flags=cflags, deps=auv_dep_as+implicit, order_only=auv_dep_sos, target_name=target_name) for s in sources], [])

        # Link the objects together and store the output.
        lib = linkfunc(target_name,
                       objs,
                       libs = deps + auv_deps + auv_static_deps,
                       flags=lflags,
                       deps=auv_dep_as + auv_dep_sos)[0]

        # Install the new file.
        installfunc(target_name, lib, is_test)

        if also_static:
            auv_dep_as += ['link-stage/lib%s.a' % d for d in auv_deps]
            lib = static_linkfunc(static_target_name,
                           objs + auv_dep_as,
                           libs = deps + static_extra_deps,
                           flags=lflags,
                           deps=auv_dep_as)[0]

            installfunc(static_target_name, lib, is_test)


    def run_test(self, test_name, args=None):
        self.check_targets.append('run-' + test_name)
        return self.n.build('run-' + test_name, 'run', 'link-stage/' + test_name, variables={'args': args})

    """
        Functions below this point are for use in configuration scripts.
    """

    # Each of these just calls build_common. If only there were a way to, say, store a
    # reference to a function with some of the arguments already filled out...
    def build_c_shared(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('c', 'shared', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_shared(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('cpp', 'shared', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_c_static(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('c', 'static', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_static(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('cpp', 'static', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_c_shared_static(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('c', 'shared+static', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_shared_static(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('cpp', 'shared+static', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_c_cmd(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('c', 'prog', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_cmd(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        self.build_common('cpp', 'prog', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit)
    def build_c_cmd_with_static(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[], static_extra_deps=[]):
        self.build_common('c', 'prog+static', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit, static_extra_deps)
    def build_cmd_with_static(self, out_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[], static_extra_deps=[]):
        self.build_common('cpp', 'prog+static', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit, static_extra_deps)

    def test_gtest(self, test_name, sources, auv_deps=[], auv_static_deps=[], deps=[], cflags=[], lflags=[], pkg_confs=[], implicit=[]):
        out_name = 'auv-test-' + test_name
        # Automatically pull in the gtest libraries for gtest tests
        deps.append('gtest')
        deps.append('gtest_main')
        self.build_common('cpp', 'prog', out_name, sources, auv_deps, auv_static_deps, deps, cflags, lflags, pkg_confs, implicit, is_test=True)
        self.run_test(out_name, args=['--gtest_color=yes'])

    def go_install(self, pkg, inputs, go_deps, auv_deps=None, auv_static_deps=None):
        self.go_common(pkg, inputs, go_deps, auv_deps, auv_static_deps)

    def go_build(self, exe, pkg, inputs, go_deps, auv_deps=None, auv_static_deps=None):
        self.go_common(pkg, inputs, go_deps, auv_deps, auv_static_deps, exe=exe)

    def go_common(self, pkg, inputs, go_deps, auv_deps=None, auv_static_deps=None, exe=None):
        inputs = ['%s/%s/%s' % (GOSRC, pkg, f) for f in inputs]
        inputs += ['%s/%s.a' % (GOPKG, d) for d in go_deps]

        if auv_deps is not None:
            inputs += ['link-stage/lib%s.so' % d for d in auv_deps]
        if auv_static_deps is not None:
            inputs += ['link-stage/lib%s.a' % d for d in auv_static_deps]

        if exe is not None:
            self.n.build("link-stage/" + exe, 'go-build', inputs, variables={"pkg": "cuauv.org/" + pkg})
            self.add_code_target("link-stage/" + exe)
        else:
            self.n.build('%s/%s.a' % (GOPKG, pkg), 'go-install', inputs, variables={"pkg": "cuauv.org/" + pkg})
            self.add_code_target('%s/%s.a' % (GOPKG, pkg))

    def stack(self, output, input_directory, auv_deps = []):
        cfg = input_directory + '/stack.yaml'
        if self.stack_unavailable(cfg):
          return None

        def walkFiles(root):
          files = []
          for (subdir, _, fs) in os.walk(root):
            for f in fs:
              joined = os.path.join(subdir, f)
              if os.path.isfile(joined):
                files.append(joined)
          return files

        implicit = [x for x in walkFiles(self.wd) if x[-3:] == '.hs' or x[-5:] == '.yaml' or x[-6:] == '.cabal']
        implicit += ['link-stage/lib%s.so' % d for d in auv_deps]
        self.n.build(output, 'stack', variables={'bin': 'bin', 'config': cfg}, implicit = implicit)

        return True

    def command_available(self, command):
        return not subprocess.call(["which", command],
                                   stdout=subprocess.DEVNULL,
                                   stderr=subprocess.DEVNULL)

    def stack_unavailable(self, cfg):
        exists = self.command_available("stack")
        if not exists:
          print('Could not build Haskell/Stack @ {} because you are missing `stack`!'.format(cfg))
        return not exists

    def chicken_available(self, output, prefix):
        exists = self.command_available("chicken-install")

        if not exists:
            print("Could not build chicken %s \"%s\" because you are missing "
                  "\"chicken-install\"" % (prefix, output))

        return exists

    def chicken_lib(self, output, inputs, where=None, chicken_deps=None, auv_deps=None, cflags="", lflags=""):
        if not self.chicken_available(output, "library"):
            return

        in_files = [os.path.join(self.wd, f) for f in inputs]
        implicit = []
        if chicken_deps is not None:
            implicit += ["link-stage/%s.fake" % d for d in chicken_deps]
        if auv_deps is not None:
            implicit += ["link-stage/lib%s.so" % d for d in auv_deps]

        fake = "link-stage/" + output + ".fake"

        if where is None:
            where = output

        self.n.build(fake, "chicken-lib", in_files, variables={"where": where, "fake": fake}, implicit=implicit)
        self.add_code_target(fake)

    def chicken_exe(self, output, inputs, module=None, chicken_deps=None, auv_deps=None, cflags="", lflags=""):
        if not self.chicken_available(output, "executable"):
            return

        in_files = [os.path.join(self.wd, f) for f in inputs]
        implicit = []
        if chicken_deps is not None:
            implicit += ["link-stage/%s.fake" % d for d in chicken_deps]
        if auv_deps is not None:
            implicit += ["link-stage/lib%s.so" % d for d in auv_deps]

        # Annoying _BSD_SOURCE to _DEFAULT_SOURCE error message, remove in the
        # future.
        cflags += " -C -Wno-cpp"

        self.n.build("link-stage/" + output, "chicken-exe", in_files, variables={"cflags": cflags, "lflags": lflags}, implicit=implicit)
        self.add_code_target("link-stage/" + output)

    def webpack_availible(self, name):
        exists = self.command_available("webpack")

        if not exists:
            print("Could not webpack \"%s\" because you are missing "
                  "\"webpack\"" % (name))
        return exists

    def webpack(self, output, config, src_dir, package_json):
        if not self.webpack_availible(output):
            return

        # Create "fake" output file for dependency on npm-installed packages. The output file allows
        # us to specify the dependency of the webpack rule on the npm-install rule and allows us to
        # avoid evaluating npm-install every build.
        npm_install_marker = os.path.join(self.wd, "npm-install.fake")
        self.npm_install(npm_install_marker, package_json)
        def walkFiles(root):
            files = []
            for (subdir, _, fs) in os.walk(root):
                for f in fs:
                    joined = os.path.join(subdir, f)
                    if os.path.isfile(joined):
                        files.append(joined)
            return files

        implicit = [npm_install_marker] + [x for x in walkFiles(os.path.join(self.wd, src_dir)) if x.endswith('.jsx')]

        output_path = os.path.join(self.wd, output)
        config_path = os.path.join(self.wd, config)

        self.n.build(output_path, "webpack", variables={"config": config_path}, implicit=implicit)
        self.add_code_target(output_path)

    def npm_install(self, output_name, package_json):
        self.n.build(output_name, "npm-install", variables={"config_path": self.wd}, implicit=[package_json])

    def generate(self, out_files, script, in_files=[], depends=[], requires=None):
        """ Generate out_files from a script.
            in_files is a list of dependencies for the script (templates, config files).
        """
        if requires is None:
          requires = []

        for require in requires:
          if not self.command_available(require):
            print("Could not generate %s because \"%s\" is not available." % \
                  (out_files, require))
            return

        out_files = [os.path.join(self.wd, f) for f in out_files]
        in_files = [os.path.join(self.wd, f) for f in in_files]
        variables = [['args', ' '.join(in_files)]]
        in_files.append(str(script))
        in_files += depends
        self.add_code_target(out_files[0]) # pick one of the out files and add it as an always built target.
        return self.n.build(out_files, 'generate', script, implicit=in_files, variables=variables)

    def general(self, name, out_files, script, in_files=[]):
        out_files = [os.path.join(self.wd, f) for f in out_files]
        in_files = [os.path.join(self.wd, f) for f in in_files]
        variables = {"args": " ".join(in_files), "name": name}
        in_files.append(script)
        return self.n.build(out_files, "general", script, implicit=in_files, variables=variables)


    def subninja(self, ninja_file):
        return self.n.subninja(ninja_file)
