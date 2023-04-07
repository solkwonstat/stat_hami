#!/bin/bash

echo "Running data converter from Public data API"
pip install -r requirements.txt
python get_public_data.py --config "public_data_conf.yaml"
echo "Processing completed"