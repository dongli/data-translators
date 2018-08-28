#!/usr/bin/env python3

import psycopg2

database_name = 'metdata'
schema_name = 'obs'

conn = psycopg2.connect('')
conn.autocommit = True

cur = conn.cursor()

cur.execute(f'select datname from pg_database where datname = \'{database_name}\';')

if len(cur.fetchall()) == 0:
	print(f'[Notice]: Create database {database_name}.')
	cur.execute(f'create database {database_name};')

cur.close()
conn.close()

conn = psycopg2.connect(f'dbname={database_name}')

cur = conn.cursor()

cur.execute(f'select schema_name from information_schema.schemata where schema_name = \'{schema_name}\';')

if len(cur.fetchall()) == 0:
	print(f'[Notice]: Create schema {schema_name}.')
	cur.execute(f'create schema {schema_name};')

# Use PostGIS extension.
cur.execute('create extension postgis;')

cur.execute(f'''
	select table_name from information_schema.tables
	where table_schema = \'{schema_name}\' and table_name = \'sfc_station\';
''')

if len(cur.fetchall()) == 0:
	print(f'[Notice]: Create table {schema_name}.sfc_station.')
	cur.execute(f'''
		create table {schema_name}.sfc_station(
			id                serial      not null primary key,
			sfc_station_name  varchar(30) not null,
			sfc_station_coord geography(POINTZ),
			sfc_station_type varchar(30) not null
		);
	''')

cur.execute(f'''
	select table_name from information_schema.tables
	where table_schema = \'{schema_name}\' and table_name = \'sfc_record\';
''')

if len(cur.fetchall()) == 0:
	print(f'[Notice]: Create table {schema_name}.sfc_record.')
	cur.execute(f'''
		create table {schema_name}.sfc_record(
			id                    serial      not null primary key,
			sfc_station_id        serial      not null,
			time                  timestamptz not null,
			sfc_temperature       real        default -99999.0,
			sfc_dewpoint          real        default -99999.0,
			sfc_relative_humidity real        default -99999.0,
			sfc_pressure          real        default -99999.0,
			sfc_wind_direction    real        default -99999.0,
			sfc_wind_speed        real        default -99999.0,
			sfc_rain_01h          real        default -99999.0,
			sfc_rain_03h          real        default -99999.0,
			sfc_rain_06h          real        default -99999.0,
			sfc_rain_12h          real        default -99999.0,
			sfc_rain_24h          real        default -99999.0,
			sfc_cloud_cover       real        default -99999.0
		);
	''')

cur.execute(f'''
	select table_name from information_schema.tables
	where table_schema = \'{schema_name}\' and table_name = \'sfc_record_qc\';
''')

if len(cur.fetchall()) == 0:
	print(f'[Notice]: Create table {schema_name}.sfc_record_qc.')
	# FIXME: Add quality control columns for each variable.
	cur.execute(f'''
		create table {schema_name}.sfc_record_qc(
			id                           serial      not null primary key,
			sfc_record_id                serial      not null,
			sfc_temperature_stack        real[],
			sfc_temperature_qc           smallint[],
			sfc_dewpoint_stack           real[],
			sfc_dewpoint_qc              smallint[],
			sfc_relative_humidity_stack  real[],
			sfc_relative_humidity_qc     smallint[],
			sfc_pressure_stack           real[],
			sfc_pressure_qc              smallint[],
			sfc_wind_direction_stack     real[],
			sfc_wind_direction_qc        smallint[],
			sfc_wind_speed_stack         real[],
			sfc_wind_speed_qc            smallint[],
			sfc_rain_01h_stack           real[],
			sfc_rain_01h_qc              smallint[],
			sfc_rain_03h_stack           real[],
			sfc_rain_03h_qc              smallint[],
			sfc_rain_06h_stack           real[],
			sfc_rain_06h_qc              smallint[],
			sfc_rain_12h_stack           real[],
			sfc_rain_12h_qc              smallint[],
			sfc_rain_24h_stack           real[],
			sfc_rain_24h_qc              smallint[],
			sfc_cloud_cover_stack        real[],
			sfc_cloud_cover_qc           smallint[]
		);
	''')

conn.commit()

cur.close()
conn.close()

