"""Making request to ROK public data using REST API"""
import argparse
import logging
import requests
import yaml

import pandas as pd

logger = logging.getLogger(__name__)


class PublicDataGetterConfig:

    @classmethod
    def load_with_yaml_file(self, config_to_load):
        with open(config_to_load, "r", encoding="utf-8") as f:
            config = yaml.safe_load(f)
        return config


class PublicDataGetter:
    def __init__(self, url, output_file=None) -> None:
        self._url = url
        if output_file is not None:
            self.output_file = output_file

    @property
    def url(self):
        return self._url

    @url.setter
    def url(self, url):
        self._url = url

    def _make_request_json(self, serviceKey, page, perPage):
        self._session = requests.Session()
        ress = self._session.get(
            url=self._url,
            params={
                "serviceKey": serviceKey,
                "page": page,
                "perPage": perPage
            }
        )

        if ress.status_code != 200:
            logger.warning(
                "Request failed with status code %s", ress.status_code)
            return None
        logger.info("Request successful")
        return ress.json()

    def _make_request_xml(self, serviceKey, pageNo, numOfRows):
        self._session = requests.Session()
        self.url = f"{self._url}?ServiceKey={serviceKey}&pageNo={pageNo}&numOfRows={numOfRows}"
        ress = self._session.get(
            url=self.url
        )
        if ress.status_code != 200:
            logger.warning(
                "Request failed with status code %s", ress.status_code)
            return None
        logger.info("Request successful")
        return ress.content

    def query_as_json_to_df(
            self,
            serviceKey,
            page,
            perPage,
            save_result=True
        ) -> pd.DataFrame:
        data = self._make_request_json(serviceKey, page, perPage)
        df = pd.DataFrame(data["data"])
        if self.output_file is None:
            save_result = False
        if save_result:
            logger.info("Saving result into csv file with %d rows", df.shape[0])
            df.to_csv(self.output_file)
        return df

    def query_as_xml_to_df(
        self,
        serviceKey,
        pageNo,
        numOfRows,
        save_result=True
        ) -> pd.DataFrame:
        data = self._make_request_xml(serviceKey, pageNo, numOfRows)
        df = pd.read_xml(data, xpath=".//row")
        if self.output_file is None:
            save_result = False
        if save_result:
            logger.info("Saving result into csv file with %d rows", df.shape[0])
            df.to_csv(self.output_file)
        return df


def run_json(url, output_file, serviceKey, page, perPage):
    data_getter = PublicDataGetter(url, output_file)
    df1 = data_getter.query_as_json_to_df(
        serviceKey=serviceKey,
        page=page,
        perPage=perPage
    )
    return df1


def run_xml(url, output_file, serviceKey, pageNo, numOfRows):
    data_getter2 = PublicDataGetter(url, output_file)
    df2 = data_getter2.query_as_xml_to_df(
        serviceKey=serviceKey,
        pageNo=pageNo,
        numOfRows=numOfRows
    )
    return df2


def run(config_file):
    config = PublicDataGetterConfig.load_with_yaml_file(config_file)
    serviceKey = config["serviceKey"]
    payment_url = config["payment"]["url"]
    payment_page = config["payment"]["page"]
    payment_perpage = config["payment"]["perPage"]
    payment_output = config["payment"]["output_file"]
    local_gift_url = config["local_gift"]["url"]
    local_gift_pageno = config["local_gift"]["pageNo"]
    local_gift_numofrows = config["local_gift"]["numOfRows"]
    local_gift_output = config["local_gift"]["output_file"]

    df1 = run_json(url=payment_url,
                   output_file=payment_output,
                   serviceKey=serviceKey,
                   page=payment_page,
                   perPage=payment_perpage)
    print(df1.head())
    
    df2 = run_xml(url=local_gift_url,
                  output_file=local_gift_output,
                  serviceKey=serviceKey,
                  pageNo=local_gift_pageno,
                  numOfRows=local_gift_numofrows)
    print(df2.head())


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s :: %(levelname)s :: %(name)s :: %(message)s"
    )
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", required=True)
    args = parser.parse_args()
    run(args.config)
