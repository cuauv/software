registered_tags = {}
registered_reqs = {}

#decorator for registering tag types with CAVE
def register_tag_type(obj):
    registered_tags[obj.get_name()] = obj
    return obj

#decorator to ensure that this tagtype will only be applied to
#mission elements implementing these methods
def require_mission_elements_to_implement(*required_functions):
    def register_wrapper(obj):
        obj._req_funct = list(required_functions)
        return obj
    return register_wrapper


#Called by main on startup to load all tag modules automagically
def load_tag_modules():
    import os
    #Load in all .py files within the this directory to pick up all
    #tags that might have been use our decorator. this is hackish
    #python black magic.
    my_folder = os.path.dirname(__file__) 
    py_files = [f for f in os.listdir(my_folder) if f.endswith(".py")]
    for py in py_files:
        __import__("libcave.tags." + py[:-3])

def get_tag_types():
    return registered_tags.values()

def get_tagtype_names():
    return registered_tags.keys()

def get_class_from_tagtype(type_string):
    return registered_tags[type_string]

def get_required_functions_of_tag(tag_name):
    return registered_tags[tag_name]._req_funct

