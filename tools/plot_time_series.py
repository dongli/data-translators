#!/usr/bin/env python3

import argparse
from Magics.macro import *
import numpy as np
import re
import pendulum
import subprocess

os.environ['LC_ALL'] = 'C'

var_info = {
	'u':  { 'name': 'wind_u', 'title': 'Wind U component (m/s)' },
	'v':  { 'name': 'wind_v', 'title': 'Wind V component (m/s)' },
	'T':  { 'name': 'temperature', 'title': 'Temperature (degC)' },
	'Td': { 'name': 'dewpoint', 'title': 'Dewpoint (degC)' },
	'RH': { 'name': 'relative_humidity', 'title': 'Relative humidity (%)' },
	'p':  { 'name': 'pressure', 'title': 'Pressure (Pa)' },
	'count': { 'name': 'record_count', 'title': 'Observation count' }
}

parser = argparse.ArgumentParser(description="Plot time series from one ODB file.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input',  help='Input ODB file path')
parser.add_argument('-s', '--station', help='Station (platform) ID')
parser.add_argument('-v', '--var', help='Variable to plot', choices=var_info.keys())
args = parser.parse_args()

if args.station:
	odb_ddl = f"select {var_info[args.var]['name']} as var, date, time where platform_id='{args.station}' order by date, time"
else:
	odb_ddl = f"select {var_info[args.var]['name']} as var, date, time order by date, time"

cmd = f'odb sql "{odb_ddl}" -i {args.input}'
res = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

lines = res.stdout.decode('utf-8').split('\n')

if type(lines) == list and len(lines) > 4:
	var = []
	time = []
	for i, line in enumerate(lines):
		if i > 0 and line != '':
			var1, date1, time1 = line.split()
			var.append(float(var1))
			time.append(pendulum.from_format("{}{:06}".format(date1, int(time1)), 'YYYYMMDDHHmmss').format('YYYY-MM-DD HH:mm:ss'))
	var = np.array(var)
else:
	print(lines)
	print('[Error]: Bad data!')
	exit(1)

data = minput(
	input_date_x_values=time,
	input_y_values=var
)

graph = mgraph(
	graph_type='curve',
	graph_line_thickness=3
)

if args.station:
	output = output(
		output_formats=['pdf'],
		output_name=f'{args.input}.{args.station}.{args.var}'
	)
else:
	output = output(
		output_formats=['pdf'],
		output_name=f'{args.input}.{args.var}'
	)

page = mmap(
	layout='positional',
	page_x_length=20.,
	page_y_length=15.,
	page_x_position=5.,
	page_y_position=2.,
	subpage_map_projection='cartesian',
	subpage_x_axis_type='date',
	subpage_y_axis_type='regular',
	subpage_x_date_min=time[0],
	subpage_x_date_max=time[-1],
	subpage_y_min=np.min(var) - (np.max(var) - np.min(var)) * 0.1,
	subpage_y_max=np.max(var) + (np.max(var) - np.min(var)) * 0.1,
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
	axis_title_text=var_info[args.var]['title'],
	axis_title_font='arial',
	axis_title_height=0.8
)

axis_h = maxis(
	axis_orientation='horizontal',
	axis_type='date',
	axis_date_type='months',
	axis_tick_label='on',
	axis_grid='on',
	axis_grid_colour='charcoal',
	axis_grid_thickness=1.,
	axis_grid_line_style='dash',
	axis_title='on',
	axis_title_text='Time',
	axis_title_font='arial',
	axis_title_font_style='bold'
)

title = mtext(
	text_lines=[f'Station {args.station}' if args.station else '']
)

plot(output, page, axis_v, axis_h, data, graph, title)
