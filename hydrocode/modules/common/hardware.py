from enum import auto, Enum
import queue
import socket
from threading import Thread
import time

try:
    import cupy as xp
except ImportError:
    import numpy as xp
import numpy as np

import common.const
from common.retry import retry
import comms.const
import pinger.const
try:
    import shm
except ImportError:
    from common import shm

class HydrophonesSection(Enum):
    PINGER = auto()
    COMMS = auto()

class HydrophonesBoard:
    def __init__(self, section, pkts_per_recv, dump=False):
        assert pkts_per_recv >= 1, (
            'Must get at least one packet per reception')

        self._pkts_per_recv = pkts_per_recv

        if section is HydrophonesSection.PINGER:
            section_const = pinger.const
            self._shm_status = shm.hydrophones_pinger_status
            self._switch_ch_order = True
        else:
            assert section is HydrophonesSection.COMMS, (
                'Hydrophones board has two sections, PINGER and COMMS')
            section_const = comms.const
            self._shm_status = shm.hydrophones_comms_status
            self._switch_ch_order = False

        self._dump_file = open('dump.dat', 'wb') if dump else None

        self._gain_val_array = np.array(common.const.GAIN_VALUES)

        recv_addr = ('', section_const.RECV_PORT)
        self._recv_q = queue.Queue(maxsize=1000)
        self._recv_thread = Thread(target=self._receive_daemon,
            args=(self._recv_q, recv_addr, pkts_per_recv), daemon=True)

        send_addr = (common.const.BOARD_ADDR, section_const.SEND_PORT)
        self._send_q = queue.Queue(maxsize=10)
        self._send_thread = Thread(target=self._send_daemon,
            args=(self._send_q, send_addr), daemon=True)

        self._recv_thread.start()
        self._send_thread.start()

        self.config()

        (recv_buff, _) = retry(self._recv_q.get, queue.Empty)(timeout=0.1)
        (pkt_num, _, _) = self._unpack_recv_buff(recv_buff)
        self._shm_status.packet_number.set(pkt_num)

    def receive(self):
        (buff, sub_hdgs) = retry(self._recv_q.get, queue.Empty)(timeout=0.1)
        if self._dump_file is not None:
            self._dump_file.write(buff)

        (pkt_num, gains, sig) = self._unpack_recv_buff(buff)
        sub_hdgs = np.asarray(sub_hdgs).reshape(1, -1).repeat(
            common.const.L_PKT, axis=1)

        self._check_pkt_num(pkt_num, self._shm_status.packet_number.get())
        self._shm_status.packet_number.set(pkt_num)

        return (xp.asarray(sig), xp.asarray(gains), xp.asarray(sub_hdgs))

    def config(self, reset=False, autogain=False, man_gain_lvl=0):
        assert 0 <= man_gain_lvl < 14, 'Gain level must be within [0, 13]'

        buff = np.array((reset, autogain, man_gain_lvl),
            dtype=common.const.CONF_PKT_DTYPE).tobytes()
        try:
            self._send_q.put_nowait(buff)
        except queue.Full:
            pass

    def _unpack_recv_buff(self, buff):
        pkts = np.frombuffer(buff, dtype=common.const.SAMPLE_PKT_DTYPE)

        pkt_num = pkts['pkt_num'][-1]
        gains = self._gain_val_array[pkts['gain_lvl']].reshape(1, -1).repeat(
            common.const.L_PKT, axis=1)
        samples = np.concatenate(pkts['samples'], axis=1)
        if self._switch_ch_order:
            samples[[0, 1]] = samples[[1, 0]]
            samples[[2, 3]] = samples[[3, 2]]
        samples = samples[: 4 if common.const.USE_4CHS else 3]

        return (pkt_num, gains, samples)

    def _check_pkt_num(self, curr, last):
        if curr < self._pkts_per_recv:
            print('\nHydrophones board resetted\n')

        lost = curr - last - self._pkts_per_recv
        if lost > 0:
            print('\nLost ' + str(lost) + ' packets\n')

    @staticmethod
    def _receive_daemon(q, addr, pkts_per_recv):
        pkt_size = common.const.SAMPLE_PKT_DTYPE.itemsize

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.bind(addr)

        while True:
            buff = bytearray(pkts_per_recv * pkt_size)
            view = memoryview(buff)
            sub_hdgs = []
            for pkt_num in range(pkts_per_recv):
                sock.recv_into(view, nbytes=pkt_size)
                view = (view[pkt_size :])
                sub_hdgs.append(shm.gx4.heading.get())
            try:
                q.put_nowait((buff, sub_hdgs))
            except queue.Full:
                pass

    @staticmethod
    def _send_daemon(q, addr):
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

        while True:
            buff = q.get()

            sock.sendto(buff, addr)

class TransmitBoard:
    def __init__(self):
        shm.transmit_settings.symbol_size.set(comms.const.SYMBOL_SIZE)
        shm.transmit_settings.symbol_rate.set(common.const.SAMPLE_RATE /
            comms.const.DECIM_FACTOR / comms.const.L_SYM)
        shm.transmit_settings.freq.set(comms.const.FREQUENCY)
        shm.transmit_settings.bandwidth.set(comms.const.BANDWIDTH)

        shm.transmit_streaming.word.set(0)
        shm.transmit_streaming.new_data.set(False)

    def send(self, data):
        for byte in data:
            shm.transmit_streaming.word.set(byte)

            shm.transmit_streaming.new_data.set(True)
            while not shm.transmit_streaming.ack.get():
                time.sleep(comms.const.SERIAL_UPDATE_TIME)

            shm.transmit_streaming.new_data.set(False)
            while shm.transmit_streaming.ack.get():
                time.sleep(comms.const.SERIAL_UPDATE_TIME)