# Public Data Getter
This module is to get public data from open API.

## How to use
- Get service Key from https://www.data.go.kr/
- Modify serviceKey section in config file "public_data_conf.yaml" accordingly.
- Pass your input parameters, such as page, perPage, pageNo, numOfRows in config file "public_data_conf.yaml"
- If you are using Windows OS, you have to enable bash.
    - The easiest way is to install git and use "Git Bash Here" mode.
- Run `bash run.sh`
- In your directory you passed into the config file, you can find the result data in csv format.