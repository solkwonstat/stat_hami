"""main script for running public data getter"""
import argparse
import logging
import os
import sys
import sys

sys.path.append(os.getcwd())
from get_public_data import (DataType, PublicDataGetterConfig,
 PublicDataGetter)

logger = logging.getLogger(__name__)


def run(config_file):
    config = PublicDataGetterConfig.load_with_yaml_file(config_file)
    serviceKey = config["serviceKey"]
    data_type = config["dataType"]
    if data_type == DataType.JSON_TYPE.value:
        config_val = config["payment"]
        url = config_val["url"]
        page = config_val["page"]
        perpage = config_val["perPage"]
        output = config_val["output_file"]
        getter = PublicDataGetter(url, DataType.JSON_TYPE, output)
        data = getter.query(serviceKey=serviceKey,
                            page=page,
                            perPage=perpage)
    
    elif data_type == DataType.XML_TYPE.value:
        config_val = config["local_gift"]
        url = config_val["url"]
        pageno = config_val["pageNo"]
        numofrows = config_val["numOfRows"]
        output = config_val["output_file"]
        getter = PublicDataGetter(url, DataType.XML_TYPE, output)
        data = getter.query(serviceKey=serviceKey,
                            pageNo=pageno,
                            numOfRows=numofrows)
    df = getter.convert_to_df(data)
    return df


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s :: %(levelname)s :: %(name)s :: %(message)s"
    )
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", required=True)
    args = parser.parse_args()
    run(args.config)

