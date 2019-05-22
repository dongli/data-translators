#!/usr/bin/env python3

import argparse
from glob import glob
import os
import re
from multiprocessing import Pool

parser = argparse.ArgumentParser(description='Translate multiple files.', formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-r', '--reader', help='Reader type')
parser.add_argument('-w', '--writer', help='Writer type', choices=('odb', 'netcdf', 'littler'))
parser.add_argument('-i', '--input', help='Input observation files.')
parser.add_argument('-p', '--process-size', dest='process_size', help='Process size to use', default=4, type=int)
args = parser.parse_args()

file_paths = glob(args.input)
if len(file_paths) == 0:
	print(f'[Error]: There is no file matches {args.input}!')
	exit(0)

data_translate_exe = os.path.dirname(os.path.realpath(__file__)) + '/../build/data_translate.exe'

def run(file_path):
	os.system(f'{data_translate_exe} -r {args.reader} -w {args.writer} -i {file_path} -o {os.path.basename(file_path)}.{args.reader.split("_")[0]}.odb')

pool = Pool(args.process_size)
pool.map(run, glob(args.input))
pool.close()
pool.join()
