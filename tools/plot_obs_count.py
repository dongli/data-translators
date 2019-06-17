#!/usr/bin/env python3

import argparse
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import os
import re
import pendulum
from subprocess import run, PIPE

os.environ['LC_ALL'] = 'C'

var_info = {
	'u':  { 'name': 'wind_u', 'title': 'Wind U component (m/s)' },
	'v':  { 'name': 'wind_v', 'title': 'Wind V component (m/s)' },
	'T':  { 'name': 'temperature', 'title': 'Temperature (degC)' },
	'Td': { 'name': 'dewpoint', 'title': 'Dewpoint (degC)' },
	'RH': { 'name': 'relative_humidity', 'title': 'Relative humidity (%)' },
	'p':  { 'name': 'pressure', 'title': 'Pressure (Pa)' }
}

parser = argparse.ArgumentParser(description="Plot observation count in ODB file.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input', help='Input ODB file path')
parser.add_argument(      '--where', help='SQL query condition')
parser.add_argument('-o', '--output', help='Output figure path')
parser.add_argument('-v', '--var', help='Variable to plot', choices=var_info.keys())
args = parser.parse_args()

if not args.output:
	args.output = f'{os.path.basename(args.input)}.{args.var}.count.pdf'

sql = f"select {var_info[args.var]['name']} as var, date, time"

if args.where:
	sql = f'{sql} where {args.where}'

sql = f'{sql} order by date, time'

print(f'[Notice]: Query ODB file {args.input} ...')
cmd = f'odb sql "{sql}" -i {args.input}'
res = run(cmd, shell=True, stdout=PIPE, stderr=PIPE)

lines = res.stdout.decode('utf-8').split('\n')

if type(lines) == list and len(lines) > 4:
	print(f'[Notice]: Process data ...')
	count = {}
	for i, line in enumerate(lines):
		if i > 0 and line != '':
			var, date, time = line.split()
			if var == 'NULL': continue
			# NOTE: Use integer key is much more efficient than string key.
			key = int(date) * 100 + int(int(time) / 10000)
			if not key in count: count[key] = 0.0
			count[key] += 1.0
else:
	print(cmd)
	print('[Error]: Bad data!')
	exit(1)

pdf = PdfPages(args.output)

fig = plt.figure(figsize=(8, 5))
plt.title(f'Data Count')
plt.gca().xaxis_date()
plt.gca().set_xlabel('Time')
plt.gca().set_ylabel('Count')
plt.grid(True)
plt.plot([pendulum.from_format(str(key), 'YYYYMMDDHH') for key in list(count.keys())], list(count.values()), color='blue')
pdf.savefig()

pdf.close()
