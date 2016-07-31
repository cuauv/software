import logging

BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE = range(8)

#The background is set with 40 plus the number of the color, and the foreground with 30

#These are the sequences need to get colored ouput
RESET_SEQ = "\033[0m"
COLOR_SEQ = "\033[1;%dm"
BOLD_SEQ = "\033[1m"

def formatter_message(message, use_color = True):
    if use_color:
        message = message.replace("$RESET", RESET_SEQ).replace("$BOLD", BOLD_SEQ)
    else:
        message = message.replace("$RESET", "").replace("$BOLD", "")
    return message

COLORS = {
    'WARNING': MAGENTA,
    'INFO': GREEN,
    'DEBUG': BLUE,
    'CRITICAL': YELLOW,
    'ERROR': RED,
    'TRACE': RED
}

#Adds custom log levels to the input logger
def add_custom_logging(logger):
    def logging_factory(logger, debug_level):
        def custom_debug(msg, *args, **kwargs):
            if logger.level >= debug_level:
               return
            logger._log(debug_level, msg, args, kwargs)
        return custom_debug 
    
    #Define custom log levels with unique IDs
    logging.addLevelName(logging.CRITICAL+1, 'TRACE')
    setattr(logger, 'trace', logging_factory(logger, logging.CRITICAL+1))


class ColoredFormatter(logging.Formatter):
    def __init__(self, msg, date, use_color = True):
        logging.Formatter.__init__(self, msg, date)
        self.use_color = use_color

    def format(self, record):
        levelname = record.levelname
        if self.use_color and levelname in COLORS:
            levelname_color = COLOR_SEQ % (30 + COLORS[levelname]) + levelname + RESET_SEQ
            record.levelname = levelname_color
        return logging.Formatter.format(self, record)
