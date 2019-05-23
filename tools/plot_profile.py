#!/usr/bin/env python3

import argparse
from Magics.macro import *
import numpy as np
import numpy.ma as ma
import re
import pendulum
import subprocess

def parse_time_range(string):
	match = re.match(r'(\d{4}\d{2}\d{2}\d{2}\d{2})-(\d{4}\d{2}\d{2}\d{2}\d{2})', string)
	if match:
		return (pendulum.from_format(match[1], 'YYYYMMDDHHmm'), pendulum.from_format(match[2], 'YYYYMMDDHHmm'))
	else:
		print(f'[Error]: Failed to parse time range "{string}"!')
		exit(1)

os.environ['LC_ALL'] = 'C'
missing_value = -1e10

var_info = {
	'u':  { 'name': 'wind_u', 'title': 'Wind U component (m/s)' },
	'v':  { 'name': 'wind_v', 'title': 'Wind V component (m/s)' },
	'T':  { 'name': 'temperature', 'title': 'Temperature (degC)' },
	'Td': { 'name': 'dewpoint', 'title': 'Dewpoint (degC)' },
	'RH': { 'name': 'relative_humidity', 'title': 'Relative humidity (%)' },
	'p':  { 'name': 'pressure', 'title': 'Pressure (Pa)' }
}

parser = argparse.ArgumentParser(description="Plot RAOB vertical profile.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input', help='Input ODB file path')
parser.add_argument('-o', '--output', help='Output figure path')
parser.add_argument('-s', '--station', help='Station (platform) ID')
parser.add_argument('-l', '--level-type', dest='level_type', help='Level type of RAOB data', choices=('man', 'sigt', 'sigw', 'trop'))
parser.add_argument('-v', '--var', help='Variable name', choices=var_info.keys())
parser.add_argument('-t', '--time-range', dest='time_range', help='Observation time range (YYYYMMDDHHmm-YYYYMMDDHHmm)', type=parse_time_range)
args = parser.parse_args()

if not args.output:
	if args.level_type:
		args.output = f'{os.path.basename(args.input)}.{args.station}.{args.level_type}.{args.var}'
	else:
		args.output = f'{os.path.basename(args.input)}.{args.station}.{args.var}'

min_date = args.time_range[0].format('YYYYMMDD')
max_date = args.time_range[1].format('YYYYMMDD')
min_time = args.time_range[0].format('HHmmss')
max_time = args.time_range[1].format('HHmmss')

odb_ddl = f"select {var_info[args.var]['name']} as var, pressure as p where platform_id='{args.station}' and level_type='{args.level_type}' and tdiff(date, time, {min_date}, {min_time})>=0 and tdiff(date, time, {max_date}, {max_time})<=0 order by pressure"

cmd = f'odb sql "{odb_ddl}" -i {args.input}'
res = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

lines = res.stdout.decode('utf-8').split('\n')

if type(lines) == list and len(lines) > 4:
	var = []
	p = []
	for i, line in enumerate(lines):
		if i > 0 and line != '':
			var1, p1 = line.split()
			if p1 == 'NULL': continue
			if var1 == 'NULL':
				var.append(missing_value)
			else:
				var.append(float(var1))
			p.append(float(p1))
	var = np.array(var)
	p = np.array(p) / 100.
else:
	print(cmd)
	print(res.stdout.decode('utf-8'))
	print('[Error]: Bad data!')
	exit(1)

data = minput(
	input_x_values=var,
	input_y_values=p
)

graph = mgraph(
	graph_type='curve',
	graph_line_thickness=3,
	graph_x_suppress_below=missing_value
)

output = output(
	output_formats=['pdf'],
	output_name=args.output
)

min_var = np.min(ma.masked_values(var, missing_value))
max_var = np.max(ma.masked_values(var, missing_value))

page = mmap(
	layout='positional',
	page_x_length=10.,
	page_y_length=15.,
	page_x_position=10.,
	page_y_position=2.,
	subpage_map_projection='cartesian',
	subpage_x_min=min_var - (max_var - min_var) * 0.1,
	subpage_x_max=max_var + (max_var - min_var) * 0.1,
	subpage_y_min=np.max(p) * 1.1,
	subpage_y_max=np.min(p) * 0.9,
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
	text_lines=[f'Station {args.station}', f'{args.time_range[0].isoformat()} - {args.time_range[1].isoformat()}']
)

plot(output, page, axis_v, axis_h, data, graph, title)
