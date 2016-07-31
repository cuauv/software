import re

class ParseError(Exception):
    def __init__(self, message, line, line_num):
        formatted_msg = "{}\nLine {}: {}".format(message, line_num, line)
        super(ParseError, self).__init__(formatted_msg)

def parse(name):

    r_decomment = re.compile(r"^(.*?)\s*(?:\/\/.*)?$")
    r_group = re.compile(r"^(\w+)$")
    r_var = re.compile(r"^(\s\s*|\t)(.+)$")
    r_var_split = re.compile(r"^(\w+)\s+(\w+)(?:\s*=\s*(\S+))?$")
    r_var_str = re.compile(r"^string\[(\d+)\]\s*(\w+)(?:\s*=\s*(\S+))?$")

    res = []
    group = None
    variables = None

    f = open(name, 'r')
    line = f.readline()
    line_num = 1
    while line:
        line_decomment = r_decomment.search(line).group(1)
        group_match = r_group.search(line_decomment)
        var_match = r_var.search(line_decomment)
        if group_match:
            group = {}
            variables = {}
            group['groupname'] = group_match.group(1)
            group['vars'] = variables
            if len(group)>0:
                res.append(group)
        elif var_match:
            if group is None:
                raise ParseError("Variable found outside of group", line, line_num)
            var = var_match.group(2)
            var_split = r_var_split.search(var)
            str_split = r_var_str.search(var)
            if var_split:
                var = {'type' : var_split.group(1), 'name' : var_split.group(2)}
                if var_split.group(3):
                    var['default'] = var_split.group(3)
                variables[var_split.group(2)]=var
            elif str_split:
                var = {'type' : 'string', 'name' : str_split.group(2), 'length' : str_split.group(1)}
                if str_split.group(3):
                    var['default'] = str_split.group(3)
                variables[str_split.group(2)]=var
            else:
                raise ParseError("Invalid variable declaration", line, line_num)
        elif line_decomment != "":
            raise ParseError("Unrecognized line content", line, line_num)

        line=f.readline()
        line_num = line_num + 1
    f.close()
    return res
