import os
import subprocess

base_dir = '/div/no-backup-nac/PATHFINDER/CHELSA'
year = '2016'
variables_to_download = ['TASMIN', 'TASMAX','PET', 'RSDS','TAS', 'PR', 'HURS']  # Add the variables you want to download. 'PET', 'RSDS','TAS', 'PR', 'HURS'

for variable in variables_to_download:
    print(f'Checking {variable}')
    variable_dir = os.path.join(base_dir, variable)
    if os.path.isdir(variable_dir):
        input_file = os.path.join(variable_dir, f'envidatS3paths_{variable}_{year}.txt')
        if os.path.isfile(input_file):
            print(f'Starting download for {variable}')
            command = ['wget', '--no-host-directories', '--force-directories', '--input-file', input_file]
            subprocess.run(command, cwd=variable_dir)