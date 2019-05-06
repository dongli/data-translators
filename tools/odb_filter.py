#!/usr/bin/env python3

import argparse
from glob import glob
import os
from subprocess import run, PIPE
import tempfile
from multiprocessing import Pool

parser = argparse.ArgumentParser(description='Filter multiple ODB files.', formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('--sql', help='SQL statement')
parser.add_argument('-i', '--input', help='Input ODB files')
parser.add_argument('-o', '--output', help='Output ODB file')
parser.add_argument('-p', '--process-size', dest='process_size', help='Process size to use', default=4, type=int)
args = parser.parse_args()

file_paths = glob(args.input)

# Write column header.
res = run(f'odb header -ddl {file_paths[0]}', shell=True, stdout=PIPE, stderr=PIPE)
if args.output:
	tmp = tempfile.NamedTemporaryFile(mode='w')
	tmp.write('\t'.join([x.strip() for x in res.stdout.decode('utf-8').split('\n')[1:-2]]).replace(',', '').replace(' ', ':'))
	tmp.write('\n')
else:
	print(res.stdout.decode('utf-8').strip())

def odb_sql(file_path):
	res = run(f'odb sql \'{args.sql}\' -T -i {file_path}', shell=True, stdout=PIPE, stderr=PIPE)
	return res

def gather_results(results):
	for res in results:
		out = res.stdout.decode('utf-8').strip()
		if out != '':
			if args.output:
				tmp.write(out)
				tmp.write('\n')
			else:
				print(out)

# Spawn multiple processes to run ODB query across files.
pool = Pool(args.process_size)
pool.map_async(odb_sql, file_paths, callback=gather_results)
pool.close()
pool.join()

if args.output:
	run(f'odb import -d TAB {tmp.name} {args.output}', shell=True, stdout=PIPE, stderr=PIPE)
