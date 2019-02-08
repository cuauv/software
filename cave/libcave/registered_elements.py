#Maintains a list of registered mission elements to use in CAVE
registered_elements = {}

#Decorator registers a mission element to be used in CAVE
def register_mission_element(obj):
    try:
        name = obj.name
    except:
        raise Exception("Registered mission elements must implement a \"name\" class variable")
    registered_elements[name] = obj
    return obj

#Carry out registration by loading this class
import cave.libcave.mission_elements
import inspect

#returns all mission elements registered with CAVE
def get_registered_elements():
    return registered_elements

#returns registered elements that contain implementations for
#all function names specified in the function_name_list
def get_registered_elements_implementing(function_name_list):
    d = dict(registered_elements)
    for k,v in d.items():
        members = [a for (a,b) in inspect.getmembers(v)]
        if not all(m in members for m in function_name_list):
            del d[k]
    return d

def get_compatible_tags(self):
    pass
