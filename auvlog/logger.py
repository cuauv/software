#!/usr/bin/env python3

import sys
from auvlog.client import log

log.manual(' '.join(sys.argv[1:]))
