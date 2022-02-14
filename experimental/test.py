import logging
import time

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    logging.info("hello world")
    logging.info("Current time is %s", int(time.time()))
