#!/usr/bin/env python3

import argparse
from glob import glob
import os
import re
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
if len(file_paths) == 0:
	print(f'[Error]: Input ODB files {args.input} are invalid!')
	exit(1)

def table_columns(file_path):
	res = run(f'odb header -ddl {file_path}', shell=True, stdout=PIPE, stderr=PIPE)
	if res.returncode != 0:
		print(res.stderr.decode('utf-8'))
	columns = {}
	for column in res.stdout.decode('utf-8').split('\n')[1:-2]:
		match = re.search('(\w+)@(\w+) (\w+)', column)
		column_name = match[1]
		table_name  = match[2]
		column_type = match[3]
		columns[column_name] = { 'table': table_name, 'type': column_type }
	return columns

columns = table_columns(file_paths[0])

def create_header(sql, columns):
	if sql:
		try:
			select_sql = re.search('select\s+(.*)\s*(?=where)', sql)[1]
		except:
			select_sql = re.search('select\s+(.*)', sql)[1]
	else:
		select_sql = '*'
	header = []
	for match in re.findall('(\w+)\s*(\([^\)]*\))?,?\s*', select_sql):
		if match[0] in columns:
			header.append(f'{match[0]}@{columns[match[0]]["table"]}:{columns[match[0]]["type"]}')
		elif match[0] in ('count', 'avg', 'min', 'max'):
			if match[1] == '(*)':
				name = 'record'
			else:
				name = match[1][1:-1]
			header.append(f'{name}_{match[0]}@stats:REAL')
		elif match[0] in ('first', 'last'):
			name = match[1][1:-1]
			if name in columns:
				header.append(f'{name}@stats:{columns[name]["type"]}')
			else:
				print('[Error]: Under construction!')
				exit(1)
		else:
			print('[Error]: Not matched!')
			exit(1)
	if len(header) == 0 and select_sql.strip() == '*':
		for key, value in columns.items():
			header.append(f'{key}@{value["table"]}:{value["type"]}')
	return '\t'.join(header)

header = create_header(args.sql, columns)

# Write column header.
if args.output:
	tmp = tempfile.NamedTemporaryFile(mode='w', dir='.')
	tmp.write(header)
	tmp.write('\n')
else:
	print(header)

def odb_sql(file_path):
	if args.sql:
		res = run(f'odb sql \'{args.sql}\' -T -i {file_path}', shell=True, stdout=PIPE, stderr=PIPE)
	else:
		res = run(f'odb sql \'select *\' -T -i {file_path}', shell=True, stdout=PIPE, stderr=PIPE)
	return (file_path, res)

def gather_results(results):
	print('Gather result ...')
	for res in results:
		lines = res[1].stdout.decode('utf-8').strip().split('\n')
		print(f'==> Found {len(lines)} records in {res[0]}.')
		if len(lines) == 0: continue
		if args.output:
			tmp.write('\n'.join(lines))
			tmp.write('\n')
		else:
			print('\n'.join(lines))

# Spawn multiple processes to run ODB query across files.
pool = Pool(args.process_size)
pool.map_async(odb_sql, file_paths, callback=gather_results)
pool.close()
pool.join()

if args.output:
	tmp.flush()
	res = run(f'odb import -d TAB {tmp.name} {args.output}', shell=True, stdout=PIPE, stderr=PIPE)
	if res.returncode != 0:
		print(res.stderr.decode('utf-8'))
