#!/usr/bin/env python3

import argparse
from Magics.macro import *
import numpy as np
import re
import pendulum
import subprocess

def parse_time(string):
	match = re.match(r'(\d{4}\d{2}\d{2}\d{2})(\d{2})?', string)
	if match.group(2):
		return pendulum.from_format(string, 'YYYYMMDDHHmm')
	else:
		return pendulum.from_format(string, 'YYYYMMDDHH')

os.environ['LC_ALL'] = 'C'

var_info = {
	'u':  { 'name': 'wind_u', 'title': 'Wind U component (m/s)' },
	'v':  { 'name': 'wind_v', 'title': 'Wind V component (m/s)' },
	'T':  { 'name': 'temperature', 'title': 'Temperature (degC)' },
	'Td': { 'name': 'dewpoint', 'title': 'Dewpoint (degC)' },
	'RH': { 'name': 'relative_humidity', 'title': 'Relative humidity (%)' },
	'p':  { 'name': 'pressure', 'title': 'Pressure (Pa)' }
}

parser = argparse.ArgumentParser(description="Plot RAOB vertical profile.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input',  help='Input ODB file path')
parser.add_argument('-s', '--station', help='Station (platform) ID')
parser.add_argument('-l', '--level-type', dest='level_type', help='Level type', choices=('man', 'sigt', 'sigw', 'trop'))
parser.add_argument('-v', '--var', help='Variable name', choices=var_info.keys())
parser.add_argument('-t', '--time', help='Observation time (YYYYMMDDHHmm)', type=parse_time)
args = parser.parse_args()

min_date = args.time.subtract(minutes=2).format('YYYYMMDD')
max_date = args.time.add(minutes=2).format('YYYYMMDD')
min_time = args.time.subtract(minutes=2).format('HHmmss')
max_time = args.time.add(minutes=2).format('HHmmss')

odb_ddl = f"select {var_info[args.var]['name']} as var, pressure as p where date>={min_date} and date<={max_date} and time>={min_time} and time<={max_time} and level_type='{args.level_type}' and platform_id='{args.station}'"

cmd = f'odb sql "{odb_ddl}" -i {args.input}'
res = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

lines = res.stdout.decode('utf-8').split('\n')

if type(lines) == list and len(lines) > 4:
	var = []
	p = []
	for i, line in enumerate(lines):
		if i > 0 and line != '':
			var1, p1 = line.split()
			var.append(float(var1))
			p.append(float(p1))
	var = np.array(var)
	p = np.array(p) / 100.
else:
	print(cmd)
	print(res.stdout.decode('utf-8'))
	print('[Error]: Bad data!')
	exit(1)

print(var, p)

data = minput(
	input_x_values=var,
	input_y_values=p
)

graph = mgraph(
	graph_type='curve',
	graph_line_thickness=3
)

output = output(
	output_formats=['pdf'],
	output_name=f'{args.input}.{args.station}.{args.var}'
)

page = mmap(
	layout='positional',
	page_x_length=10.,
	page_y_length=15.,
	page_x_position=10.,
	page_y_position=2.,
	subpage_map_projection='cartesian',
	subpage_x_min=np.min(var) - (np.max(var) - np.min(var)) * 0.1,
	subpage_x_max=np.max(var) + (np.max(var) - np.min(var)) * 0.1,
	subpage_y_min=np.max(p) * 1.1,
	subpage_y_max=100.,
	page_id_line='off'
)

axis_v = maxis(
	axis_orientation='vertical',
	axis_grid='on',
	axis_tick_label_height=0.4,
	axis_tick_label_colour='charcoal',
	axis_grid_colour='charcoal',
	axis_grid_line_style='dash',
	axis_title='on',
	axis_title_text='Pressure (hPa)',
	axis_title_font='arial',
	axis_title_height=0.8
)

axis_h = maxis(
	axis_orientation='horizontal',
	axis_tick_label='on',
	axis_grid='on',
	axis_grid_colour='charcoal',
	axis_grid_thickness=1.,
	axis_grid_line_style='dash',
	axis_title='on',
	axis_title_text=var_info[args.var]['title'],
	axis_title_font='arial',
	axis_title_font_style='bold'
)

title = mtext(
	text_lines=[f'Station {args.station}', args.time.isoformat()]
)

plot(output, page, axis_v, axis_h, data, graph, title)
