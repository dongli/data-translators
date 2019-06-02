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

parser = argparse.ArgumentParser(description="Plot observation QC rate in ODB file.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input', help='Input ODB file path')
parser.add_argument('-o', '--output', help='Output figure path')
parser.add_argument('-v', '--var', help='Variable to plot', choices=var_info.keys())
args = parser.parse_args()

if not args.output:
	args.output = f'{os.path.basename(args.input)}.{args.var}.qc_rate.pdf'

odb_ddl = f"select {var_info[args.var]['name']} as var, {var_info[args.var]['name']}_qc as qc, date, time order by date, time"

print(f'[Notice]: Query ODB file {args.input} ...')
cmd = f'odb sql "{odb_ddl}" -i {args.input}'
res = run(cmd, shell=True, stdout=PIPE, stderr=PIPE)

lines = res.stdout.decode('utf-8').split('\n')

if type(lines) == list and len(lines) > 4:
	print(f'[Notice]: Process data ...')
	total_count = {}
	qc_count = {}
	for i, line in enumerate(lines):
		if i > 0 and line != '':
			var, qc, date, time = line.split()
			if var == 'NULL' or qc == 'NULL': continue
			# NOTE: Use integer key is much more efficient than string key.
			key = int(date) * 100 + int(int(time) / 10000)
			if not key in total_count:
				total_count[key] = 0.0
				qc_count[key] = 0.0
			total_count[key] += 1.0
			if int(qc) >= 4:
				qc_count[key] += 1.0
else:
	print('[Error]: Bad data!')
	exit(1)

qc_rate = np.array(list(qc_count.values())) / np.array(list(total_count.values()))

pdf = PdfPages(args.output)

fig = plt.figure(figsize=(8, 5))
plt.title(f'Data Count')
plt.gca().xaxis_date()
plt.gca().set_xlabel('Time')
plt.gca().set_ylabel('Count')
plt.grid(True)
plt.plot([pendulum.from_format(str(key), 'YYYYMMDDHH') for key in list(total_count.keys())], qc_rate, color='blue')
pdf.savefig()

pdf.close()
