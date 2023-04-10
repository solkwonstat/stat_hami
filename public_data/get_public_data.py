"""Making request to ROK public data using REST API"""
import argparse
import enum
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


class DataType(enum.Enum):
    JSON_TYPE = "json"
    XML_TYPE = "xml"


class PublicDataGetter:
    def __init__(self,
                 url,
                 datatype: DataType,
                 output_file=None) -> None:
        self._url = url
        self.datatype = datatype
        if output_file is not None:
            self.output_file = output_file

    @property
    def url(self):
        return self._url

    @url.setter
    def url(self, url):
        self._url = url

    def query(self, **kwargs):
        self._session = requests.Session()
        if self.datatype == DataType.JSON_TYPE:
            ress = self._session.get(
                url=self._url,
                params={
                    "serviceKey": kwargs["serviceKey"],
                    "page": kwargs["page"],
                    "perPage": kwargs["perPage"]
                }
            )
            if ress.status_code != 200:
                logger.warning(
                    "Request failed with status code %s", ress.status_code)
                return None
            logger.info("Request successful with %s", kwargs)
            return ress.json()
        elif self.datatype == DataType.XML_TYPE:
            self.url = f"{self._url}?ServiceKey={kwargs['serviceKey']}&pageNo={kwargs['pageNo']}&numOfRows={kwargs['numOfRows']}"
            ress = self._session.get(
            url=self.url
            )
            if ress.status_code != 200:
                logger.warning(
                    "Request failed with status code %s", ress.status_code)
                return None
            logger.info("Request successful %s", kwargs)
            return ress.content
        else:
            raise NotImplementedError

    def convert_to_df(self, data, save_result=True):
        if self.datatype == DataType.JSON_TYPE:
            df = pd.DataFrame(data["data"])
        elif self.datatype == DataType.XML_TYPE:
            df = pd.read_xml(data, xpath=".//row")
        else:
            raise NotImplementedError

        if self.output_file is None:
            save_result = False
        if save_result:
            logger.info("Saving result into csv file with %d rows", df.shape[0])
            df.to_csv(self.output_file)
        return df
