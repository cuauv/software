#!/usr/bin/env python3
import os
import sys
from pathlib import Path
from flask import Flask, render_template


DIR = os.getenv('CUAUV_SOFTWARE')
VEHICLE = os.getenv('CUAUV_VEHICLE')

if DIR is None:
    sys.stderr.write('CUAUV_SOFTWARE must be set to the root of the software repository.\n')
    sys.exit(1)
if VEHICLE is None:
    sys.stderr.write('CUAUV_VEHICLE must be set.\n')
    sys.exit(1)


app = Flask(__name__, root_path=os.path.dirname(os.path.realpath(__file__)))


@app.route('/')
def index():
    return render_template('index.html')


@app.route('/config', methods=['GET'])
def get_config():
    with open(str(Path(DIR) / 'conf' / '{}.json'.format(VEHICLE))) as f:
        return f.read()


if __name__ == '__main__':
    print(app.url_map)
    app.run(host='0.0.0.0', port='8100', debug=False)

