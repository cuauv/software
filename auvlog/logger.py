#!/usr/bin/env python

import sys
from auvlog.client import log

log.manual(' '.join(sys.argv[1:]))
