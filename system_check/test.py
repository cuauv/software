import types, inspect, collections

OK, WARN, ERR = 0, 1, 2
MAINSUB, MINISUB = 0, 1
LAND, WATER = 0, 1

test = collections.namedtuple('test', ['name', 'expr', 'raw', 'on_success', 'on_error'])

class Test:
    # Creates a Test namedtuple from a function
    def test_for_function(f):
        name = f.__qualname__

        if hasattr(f, '_level'):
            on_error = f._level
        else:
            on_error = ERR

        raw = inspect.getsource(f)

        return test(name, f, raw, OK, on_error)

    def test_selector(tests, attr, attr_value):
        sel_tests = []
        for v in tests:
            if not (hasattr(v, attr) and getattr(v, attr) != attr_value):
                sel_tests.append(v)
        return sel_tests

    # Returns the lists of tests to run for a vehicle and environment
    def active_tests(vehicle_id, environment_id):
        tests = []
        for c in Test.__subclasses__():
            for v in c.__dict__.values():
                if (isinstance(v, types.FunctionType)):
                    tests.append(v)

        tests = Test.test_selector(tests, '_vehicle', vehicle_id)
        if(environment_id != -1):
            tests = Test.test_selector(tests, '_environment', environment_id)
        test_set  = []
        for t in tests:
            test_set.append(Test.test_for_function(t))
        return test_set


# Decorator which sets a value on an object
def setter_decorator(attr, value):
    def decorator(obj):
        setattr(obj, attr, value)

        # Recursively set the attr on all children [Test]s (if not already set).
        for child_attr in obj.__dict__.values():
        #for child_attr in map(lambda attr: getattr(obj, attr), dir(obj)):
            if child_attr.__class__ == type and issubclass(child_attr, Test):
                if not hasattr(child_attr, attr):
                    decorator(child_attr)
            if (isinstance(child_attr, types.FunctionType)):
                if not hasattr(child_attr, attr):
                    setattr(child_attr, attr, value)

        return obj

    return decorator

# Vehicle decorator
def vehicle(vehicle_id):
    return setter_decorator('_vehicle', vehicle_id)

# Level decorator
def level(level_id):
    return setter_decorator('_level', level_id)

# Environment decorator
def environment(environment_id):
    return setter_decorator('_environment', environment_id)
