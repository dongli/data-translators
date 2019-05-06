#!/usr/bin/env python3

import argparse
from Magics.macro import *
import numpy as np
import re
import pendulum
import subprocess

os.environ['LC_ALL'] = 'C'

def parse_time(string):
	match = re.match(r'(\d{4}\d{2}\d{2}\d{2})(\d{2})?', string)
	if match.group(2):
		return pendulum.from_format(string, 'YYYYMMDDHHmm')
	else:
		return pendulum.from_format(string, 'YYYYMMDDHH')

parser = argparse.ArgumentParser(description="Plot vertical wind profile.", formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-i', '--input', help='Input ODB file path')
parser.add_argument('-s', '--station', help='Station (platform) ID')
parser.add_argument('-t', '--time', help='Observation time (YYYYMMDDHH[mm]', type=parse_time)
args = parser.parse_args()

min_date = args.time.subtract(minutes=2).format('YYYYMMDD')
max_date = args.time.add(minutes=2).format('YYYYMMDD')
min_time = args.time.subtract(minutes=2).format('HHmmss')
max_time = args.time.add(minutes=2).format('HHmmss')

odb_ddl = f"select wind_u as u, wind_v as v, pressure as p where platform_id='{args.station}' and date>={min_date} and date<={max_date} and time>={min_time} and time<={max_time}"

cmd = f'odb sql "{odb_ddl}" -i {args.input}'
res = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

lines = res.stdout.decode('utf-8').split('\n')

if type(lines) == list and len(lines) > 4:
	u = []
	v = []
	p = []
	for i, line in enumerate(lines):
		if i > 0 and line != '':
			u1, v1, p1 = line.split()
			u.append(float(u1))
			v.append(float(v1))
			p.append(float(p1))
	u = np.array(u)
	v = np.array(v)
	p = np.array(p) / 100.
else:
	print('[Error]: Bad data!')
	exit(1)

data = minput(
	input_x_component_values=u,
	input_x_values=[0.] * len(p),
	input_y_component_values=v,
	input_y_values=p
)

graph = mgraph(
	graph_type='flag',
	graph_flag_length=1.,
	legend='on'
)

output = output(
	output_formats=['pdf'],
	output_name=f'{args.input}.wind'
)

page = mmap(
	page_x_length=10.,
	layout='positional',
	page_x_position=10.,
	subpage_map_projection='cartesian',
	subpage_x_min=-1.,
	subpage_x_max=1.,
	subpage_y_min=1020.,
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
	axis_tick_label='off',
	axis_grid='on',
	axis_grid_colour='charcoal',
	axis_grid_thickness=1.,
	axis_grid_line_style='dash',
	axis_title='on',
	axis_title_text='Wind vector',
	axis_title_font='arial',
	axis_title_font_style='bold'
)

title = mtext(
	text_lines=[f'Station {args.station}', args.time.isoformat()]
)

plot(output, page, axis_v, axis_h, data, graph, title)
