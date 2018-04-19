import struct

_int_format_str = 'i?ii'
_double_format_str = 'd?dd'
_bool_format_str = '?'
_str_format_str = '256s'

def option_from_value(name, format_str, value):
    if format_str == _int_format_str:
        default, has_min_max, min_value, max_value = value
        min_value = min_value if has_min_max else None
        max_value = max_value if has_min_max else None
        return IntOption(name, default, min_value, max_value)
    elif format_str == _double_format_str:
        default, has_min_max, min_value, max_value = value
        min_value = min_value if has_min_max else None
        max_value = max_value if has_min_max else None
        return DoubleOption(name, default, min_value, max_value)
    elif format_str == _bool_format_str:
        default, = value
        return BoolOption(name, default)
    elif format_str == _str_format_str:
        default, = value
        return StrOption(name, default)
    else:
        raise ValueError('Invalid format str')

class _OptionBase:
    def __init__(self, name, value, format_str, validator=None):
        assert(len(name) <= 80)
        self.name = name
        self.value = value
        self.default = value
        self.validator = validator if validator else lambda x: True
        self.format_str = format_str

    def update(self, value):
        if self.validator(value):
            self.value = value

class _NumberOption(_OptionBase):
    def __init__(self, name, default, format_str, min_value=None, max_value=None,
                 validator=None):
        self.min_value = min_value
        self.max_value = max_value
        self.has_min_max = self.min_value is not None and self.max_value is not None
        validator = validator if validator else lambda x: True
        if self.has_min_max:
            assert min_value <= max_value
            new_validator = lambda x: x >= min_value and x <= max_value and validator(x)
            super().__init__(name, default, format_str, new_validator)
        else:
            super().__init__(name, default, format_str, validator)

    def get_pack_values(self):
        if self.has_min_max:
            return self.value, True, self.min_value, self.max_value
        else:
            return self.value, False, 0, 0

    def populate_value_dict(self, d):
        d['value'] = self.value
        if self.has_min_max:
            d['min_value'] = self.min_value
            d['max_value'] = self.max_value


class IntOption(_NumberOption):
    def __init__(self, name, default, min_value=None, max_value=None, validator=None):
        super().__init__(name, default, _int_format_str, min_value,
                         max_value, validator)

    def populate_value_dict(self, d):
        super().populate_value_dict(d)
        d['type'] = 'int'

class DoubleOption(_NumberOption):
    def __init__(self, name, default, min_value=None, max_value=None,
                 validator=None):
        super().__init__(name, default, _double_format_str, min_value,
                         max_value, validator)

    def populate_value_dict(self, d):
        super().populate_value_dict(d)
        d['type'] = 'double'

class BoolOption(_OptionBase):
    def __init__(self, name, default):
        super().__init__(name, default, _bool_format_str)

    def get_pack_values(self):
        return self.value,

    def populate_value_dict(self, d):
        d['type'] = 'bool'
        d['value'] = self.value

class StrOption(_OptionBase):
    def __init__(self, name, default):
        super().__init__(name, default, _str_format_str)

    def get_pack_values(self):
        return self.value,

    def populate_value_dict(self, d):
        d['type'] = 'str'
        d['value'] = self.value
