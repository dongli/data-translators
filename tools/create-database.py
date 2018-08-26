#!/usr/bin/env python3

import psycopg2

database_name = 'metdata'
schema_name = 'obs'

conn = psycopg2.connect('')

conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)

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

cur.execute(f'''
	select table_name from information_schema.tables
	where table_schema = \'{schema_name}\' and table_name = \'sfc_station\';
''')

if len(cur.fetchall()) == 0:
	print(f'[Notice]: Create table {schema_name}.sfc_station.')
	cur.execute(f'''
		create table {schema_name}.sfc_station(
			id               serial      not null primary key,
			sfc_station_name varchar(30) not null,
			sfc_station_lon  real        not null,
			sfc_station_lat  real        not null,
			sfc_station_z    real        not null,
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
			sfc_temperature       real        not null,
			sfc_dewpoint          real        not null,
			sfc_relative_humidity real        not null,
			sfc_pressure          real        not null,
			sfc_wind_direction    real        not null,
			sfc_wind_speed        real        not null,
			sfc_rain_01h          real        not null,
			sfc_rain_03h          real        not null,
			sfc_rain_06h          real        not null,
			sfc_rain_12h          real        not null,
			sfc_rain_24h          real        not null,
			sfc_cloud_cover       real        not null
		);
	''')

conn.commit()

cur.close()
conn.close()

