__all__ = ['watchers',
    <!--(for g in groups)-->
           '$!g['groupname']!$',
    <!--(end)-->
           ]

for module in __all__:
    __import__("shm.{0}".format(module))

from shm.base import auv_var_lib

_init = auv_var_lib.shm_init
_init.argtypes = []

_init()


class ShmEvalError(Exception):
    pass


def _eval(x):
    """
    Get a shared group or variable.

    :param x: Name of the group or variable. Must be of type str, e.g. desires.depth.
    :return: Group or variable named x.
    :raise ShmEvalError: Cannot evaluate input.
    """
    assert isinstance(x, str), "_eval parameter must be a string"
    sval = x.split(".")

    if len(sval) > 2:
        raise ShmEvalError(str(x) + " - malformed shared variable")

    grp = sval[0]
    if grp not in __all__:
        raise ShmEvalError(str(x) + " - group not found")

    mod = eval(grp)
    if len(sval) == 2:
        vr = sval[1]
        if vr not in mod.__dict__:
            raise ShmEvalError(str(x) + " - variable not found")
        return eval(grp + "." + vr)

    else:
        return mod
