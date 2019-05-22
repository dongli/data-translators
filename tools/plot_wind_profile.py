#!/usr/bin/env python3

import argparse
from glob import glob
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import os
import re
import pendulum
from datetime import datetime, timedelta
from subprocess import run, PIPE

def parse_time_range(string):
	match = re.match(r'^(\d{4}\d{2}\d{2}\d{2}\d{2})-(\d{4}\d{2}\d{2}\d{2}\d{2})$', string)
	if match:
		return (pendulum.from_format(match[1], 'YYYYMMDDHHmm'), pendulum.from_format(match[2], 'YYYYMMDDHHmm'))
	else:
		print(f'[Error]: Failed to parse time range "{string}"!')
		exit(1)

parser = argparse.ArgumentParser(description="Plot wind profile with time axis from ODB file.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input', help='Input ODB file')
parser.add_argument('-o', '--output', help='Output figure path')
parser.add_argument('-s', '--station', help='Station ID')
parser.add_argument('-l', '--level-type', dest='level_type', help='Level type of RAOB data', choices=('man', 'sigt', 'sigw', 'trop'))
parser.add_argument('-t', '--time-range', dest='time_range', help='Observation time range (YYYYMMDDHHmm-YYYYMMDDHHmm)', type=parse_time_range)
args = parser.parse_args()

if not args.output:
	if args.level_type:
		args.output = f'{os.path.basename(args.input)}.{args.station}.wind.{args.level_type}.pdf'
	else:
		args.output = f'{os.path.basename(args.input)}.{args.station}.wind.pdf'

min_date = args.time_range[0].format('YYYYMMDD')
max_date = args.time_range[1].format('YYYYMMDD')
min_time = args.time_range[0].format('HHmmss')
max_time = args.time_range[1].format('HHmmss')

if args.level_type:
	cmd = f'odb sql "select wind_u as u, wind_v as v, pressure as p, date, time where platform_id=\'{args.station}\' and level_type=\'{args.level_type}\' and tdiff(date, time, {min_date}, {min_time})>=0 and tdiff(date, time, {max_date}, {max_time})<=0 order by date, time, pressure" -T -i {args.input}'
else:
	cmd = f'odb sql "select wind_u as u, wind_v as v, pressure as p, date, time where platform_id=\'{args.station}\' and tdiff(date, time, {min_date}, {min_time})>=0 and tdiff(date, time, {max_date}, {max_time})<=0 order by date, time, pressure" -T -i {args.input}'
res = run(cmd, shell=True, stdout=PIPE, stderr=PIPE)
if res.returncode != 0:
	print(f'[Error]: Failed to run {cmd}!')
	exit(1)

X = []
Y = []
U = []
V = []
for line in res.stdout.decode('utf-8').strip().split('\n'):
	tmp = line.split()
	u = float(tmp[0])         if tmp[0] != 'NULL' else -999999
	v = float(tmp[1])         if tmp[1] != 'NULL' else -999999
	p = float(tmp[2]) / 100.0 if tmp[2] != 'NULL' else -999999
	if tmp[3] != 'NULL' and tmp[4] != 'NULL':
		d = int(tmp[3])
		t = int(tmp[4])
		time = datetime(int(d / 10000), int((d % 10000) / 100), int(d % 100), int(t / 10000), int((t % 10000) / 100), int(t % 100), int(t % 100))
	X.append(time)
	Y.append(p)
	U.append(u)
	V.append(v)

U = np.array(U)
V = np.array(V)

pdf = PdfPages(args.output)

fig = plt.figure(figsize=(8, 5))
plt.title(f'Station {args.station}')
plt.gca().xaxis_date()
plt.gca().set_xlim(X[0] - timedelta(hours=1), X[-1] + timedelta(hours=1))
plt.gca().invert_yaxis()
plt.gca().set_xlabel('Time')
plt.gca().set_ylabel('Pressure (hPa)')
im = plt.barbs(X, Y, U, V, np.sqrt(U ** 2 + V ** 2), zorder=2, pivot='middle', sizes={ 'emptybarb': 0.01 })
plt.grid(True, zorder=1)
plt.gcf().autofmt_xdate()
plt.colorbar(im)
pdf.savefig()

pdf.close()
