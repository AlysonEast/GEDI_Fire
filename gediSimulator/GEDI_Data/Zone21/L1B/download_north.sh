#!/bin/bash

GREP_OPTIONS=''

cookiejar=$(mktemp cookies.XXXXXXXXXX)
netrc=$(mktemp netrc.XXXXXXXXXX)
chmod 0600 "$cookiejar" "$netrc"
function finish {
  rm -rf "$cookiejar" "$netrc"
}

trap finish EXIT
WGETRC="$wgetrc"

prompt_credentials() {
    echo "Enter your Earthdata Login or other provider supplied credentials"
    read -p "Username (aly3213): " username
    username=${username:-aly3213}
    read -s -p "Password: " password
    echo "machine urs.earthdata.nasa.gov login $username password $password" >> $netrc
    echo
}

exit_with_error() {
    echo
    echo "Unable to Retrieve Data"
    echo
    echo $1
    echo
    echo "https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.07.08/GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5"
    echo
    exit 1
}

prompt_credentials
  detect_app_approval() {
    approved=`curl -s -b "$cookiejar" -c "$cookiejar" -L --max-redirs 5 --netrc-file "$netrc" https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.07.08/GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5 -w '\n%{http_code}' | tail  -1`
    if [ "$approved" -ne "200" ] && [ "$approved" -ne "301" ] && [ "$approved" -ne "302" ]; then
        # User didn't approve the app. Direct users to approve the app in URS
        exit_with_error "Please ensure that you have authorized the remote application by visiting the link below "
    fi
}

setup_auth_curl() {
    # Firstly, check if it require URS authentication
    status=$(curl -s -z "$(date)" -w '\n%{http_code}' https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.07.08/GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5 | tail -1)
    if [[ "$status" -ne "200" && "$status" -ne "304" ]]; then
        # URS authentication is required. Now further check if the application/remote service is approved.
        detect_app_approval
    fi
}

setup_auth_wget() {
    # The safest way to auth via curl is netrc. Note: there's no checking or feedback
    # if login is unsuccessful
    touch ~/.netrc
    chmod 0600 ~/.netrc
    credentials=$(grep 'machine urs.earthdata.nasa.gov' ~/.netrc)
    if [ -z "$credentials" ]; then
        cat "$netrc" >> ~/.netrc
    fi
}

fetch_urls() {
  if command -v curl >/dev/null 2>&1; then
      setup_auth_curl
      while read -r line; do
        # Get everything after the last '/'
        filename="${line##*/}"

        # Strip everything after '?'
        stripped_query_params="${filename%%\?*}"

        curl -f -b "$cookiejar" -c "$cookiejar" -L --netrc-file "$netrc" -g -o $stripped_query_params -- $line && echo || exit_with_error "Command failed with error. Please retrieve the data manually."
      done;
  elif command -v wget >/dev/null 2>&1; then
      # We can't use wget to poke provider server to get info whether or not URS was integrated without download at least one of the files.
      echo
      echo "WARNING: Can't find curl, use wget instead."
      echo "WARNING: Script may not correctly identify Earthdata Login integrations."
      echo
      setup_auth_wget
      while read -r line; do
        # Get everything after the last '/'
        filename="${line##*/}"

        # Strip everything after '?'
        stripped_query_params="${filename%%\?*}"

        wget --load-cookies "$cookiejar" --save-cookies "$cookiejar" --output-document $stripped_query_params --keep-session-cookies -- $line && echo || exit_with_error "Command failed with error. Please retrieve the data manually."
      done;
  else
      exit_with_error "Error: Could not find a command-line downloader.  Please install curl or wget"
  fi
}

fetch_urls <<'EDSCEOF'
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.07.08/GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.07.01/GEDI01_B_2020183030532_O08786_01_T04282_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.27/GEDI01_B_2020179044014_O08725_01_T01436_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.23/GEDI01_B_2020175061454_O08664_01_T00977_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.22/GEDI01_B_2020174175154_O08656_04_T05544_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.19/GEDI01_B_2020171074932_O08603_01_T00824_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.18/GEDI01_B_2020170192631_O08595_04_T03968_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.14/GEDI01_B_2020166210108_O08534_04_T03815_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.10/GEDI01_B_2020162223544_O08473_04_T00663_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.07/GEDI01_B_2020159001018_O08412_04_T04932_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.06.03/GEDI01_B_2020155014450_O08351_04_T03356_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2020.05.30/GEDI01_B_2020151031918_O08290_04_T01933_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.12.03/GEDI01_B_2019337142221_O05522_01_T02859_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.11.25/GEDI01_B_2019329051323_O05392_04_T05238_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.11.03/GEDI01_B_2019307022954_O05049_01_T05399_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.10.29/GEDI01_B_2019302154448_O04980_04_T00816_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.10.11/GEDI01_B_2019284112653_O04698_01_T03976_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.10.07/GEDI01_B_2019280004138_O04629_04_T02392_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.09.14/GEDI01_B_2019257093638_O04278_04_T04121_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.09.10/GEDI01_B_2019253111453_O04217_04_T02086_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.08.08/GEDI01_B_2019220124906_O03706_01_T03670_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.08.08/GEDI01_B_2019220002652_O03698_04_T05391_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.07.28/GEDI01_B_2019209170158_O03538_01_T04435_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.07.24/GEDI01_B_2019205062002_O03469_04_T03509_02_005_02_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.07.13/GEDI01_B_2019194225448_O03309_01_T05552_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.07.09/GEDI01_B_2019190121243_O03240_04_T00510_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.06.29/GEDI01_B_2019180044819_O03080_01_T02400_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.06.28/GEDI01_B_2019179162610_O03072_04_T01122_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.06.18/GEDI01_B_2019169090309_O02912_01_T01283_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.06.13/GEDI01_B_2019164222202_O02843_04_T03203_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.05.23/GEDI01_B_2019143191251_O02515_01_T01130_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.05.23/GEDI01_B_2019143065114_O02507_04_T01275_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.05.16/GEDI01_B_2019136215550_O02408_01_T04129_02_005_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI01_B.002/2019.05.10/GEDI01_B_2019130003848_O02301_01_T01589_02_005_01_V002.h5
EDSCEOF
