#!/usr/bin/env python3

import argparse
import atexit
import multiprocessing
from multiprocessing.pool import ThreadPool
import os
import subprocess
import threading

MIN_LOG_SIZE = 500000

_tmp_files = set()
_tmp_file_lock = threading.Lock()

def add_tmp_file(f):
    with _tmp_file_lock:
        _tmp_files.add(f)

def rem_tmp_file(f):
    with _tmp_file_lock:
        _tmp_files.remove(f)

def cleanup_tmp():
    with _tmp_file_lock:
        for f in _tmp_files:
            try:
                os.remove(f)
            except:
                pass

atexit.register(cleanup_tmp)

def gen_ffprobe_args(infile):
    return ['ffprobe', '-v', 'quiet', '-select_streams', 'v:0', '-show_entries', 'stream=codec_name', '-of', 'default=noprint_wrappers=1:nokey=1', infile]

def gen_ffmpeg_args(infile, outfile):
    # only printing out errors, take the input file, truncate the output to even integer size (required by h264), rencode with h264 to outfile
    return ['ffmpeg', '-v', 'error', '-i', infile, '-vf', 'scale=trunc(iw/2)*2:trunc(ih/2)*2', '-vcodec', 'libx264', outfile]

def should_convert(infile):
    try:
        if os.path.getsize(infile) < MIN_LOG_SIZE:
            return False
        ffprobe_output = subprocess.check_output(gen_ffprobe_args(infile)).decode('utf8').strip().lower()
        if ffprobe_output == 'h264' or ffprobe_output == '':
            return False
        else:
            return True
    except subprocess.CalledProcessError:
        return False
    except:
        return False

def convert_file(infile, debug):
    outfile = infile + '.tmp'
    # make sure that we're not overwriting any files (and that ffmpeg doesn't need any more input)
    while os.path.exists(outfile):
        outfile += '.tmp'
    # tell ffmpeg we want an avi
    outfile += '.avi'

    if debug:
        print('Converting {}'.format(infile))
    add_tmp_file(outfile)
    if subprocess.call(gen_ffmpeg_args(infile, outfile)) == 0:
        os.rename(outfile, infile)
        rem_tmp_file(outfile)
        if debug:
            print('Converted {}'.format(infile))
        return True
    else:
        print('Failed to convert {}'.format(infile))
        os.remove(outfile)
        rem_tmp_file(outfile)
        return False

def recursive_ls(directory):
    for (dirname, _, filenames) in os.walk(directory):
        for filename in filenames:
            yield os.path.join(dirname, filename)

def convert_all(directory, debug):
    if not os.path.isdir(directory):
        raise ValueError('{} is not a directory!'.format(directory))
    files = filter(should_convert, recursive_ls(directory))
    with ThreadPool(multiprocessing.cpu_count() * 5 // 4) as p:
        converted = p.map(lambda infile: convert_file(infile, debug), files)
    print('Converted {} files!'.format(sum(converted)))
    print('Failed to convert {} files.'.format(len(converted) - sum(converted)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Recursively convert every video in a directory')
    parser.add_argument('directory', help='The directory to recursively descend')
    parser.add_argument('debug', nargs='?', type=bool, default=False, help='Whether to print debugging info')
    args = parser.parse_args()
    convert_all(args.directory, args.debug)
