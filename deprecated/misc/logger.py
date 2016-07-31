import logging
import sys

if __name__ == "__main__":
    logger = logging.getLogger("test")

    formatstr = '%(asctime)s.%(msecs)d  %(message)s'
    datefmt = '%H:%M:%S'

    fname = sys.argv[1]

    logfilehandler = logging.FileHandler(fname, 'w')
    logfilehandler.setLevel(logging.DEBUG)
    logfilehandler.setFormatter(logging.Formatter(formatstr, datefmt))

    # Configure the logger
    logging.getLogger('').setLevel(logging.DEBUG)
    logging.getLogger('').addHandler(logfilehandler)

    while(True):
        try:
            line = raw_input()
            logging.info(line)
        except:
            exit()
