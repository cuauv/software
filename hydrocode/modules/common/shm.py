class _Group:
    pass

class _Var:
    def __init__(self, label, x):
        self._label = label

        self.set(x)

    def set(self, x):
        self._val = x
        #print('SHM set: ' + self._label + ' = ' + str(x))

    def get(self):
        return self._val

hydrophones_pinger_settings = _Group()
hydrophones_pinger_settings.frequency = _Var(
    'hydrophones_pinger_settings.frequency', 25000)
hydrophones_pinger_settings.gain_control_mode = _Var(
    'hydrophones_pinger_settings.gain_control_mode', 0)
hydrophones_pinger_settings.user_gain_lvl = _Var(
    'hydrophones_pinger_settings.user_gain_lvl', 0)

hydrophones_pinger_status = _Group()
hydrophones_pinger_status.packet_number = _Var(
    'hydrophones_pinger_status.packet_number', 0)

hydrophones_pinger_results = _Group()
hydrophones_pinger_results.heading = _Var(
    'hydrophones_pinger_results.heading', 0)
hydrophones_pinger_results.elevation = _Var(
    'hydrophones_pinger_results.elevation', 0)

hydrophones_comms_settings = _Group()
hydrophones_comms_settings.gain_control_mode = _Var(
    'hydrophones_comms_settings.gain_control_mode', 0)
hydrophones_comms_settings.user_gain_lvl = _Var(
    'hydrophones_comms_settings.user_gain_lvl', 0)

hydrophones_comms_status = _Group()
hydrophones_comms_status.packet_number = _Var(
    'hydrophones_comms_status.packet_number', 0)

gx4 = _Group()
gx4.heading = _Var('gx4.heading', 0)
gx4.pitch = _Var('gx4.pitch', 0)

transmit_settings = _Group()
transmit_settings.symbol_size = _Var('transmit_settings.symbol_size', 0)
transmit_settings.symbol_rate = _Var('transmit_settings.symbol_rate', 0)
transmit_settings.freq = _Var('transmit_settings.freq', 0)
transmit_settings.bandwidth = _Var('transmit_settings.bandwidth', 0)

transmit_streaming = _Group()
transmit_streaming.word = _Var('transmit_streaming.word', 0)
transmit_streaming.new_data = _Var('transmit_streaming.new_data', False)
transmit_streaming.ack = _Var('transmit_streaming.ack', False)