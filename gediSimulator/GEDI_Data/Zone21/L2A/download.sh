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
    echo "https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.12.31/GEDI02_A_2020366140542_O11630_04_T07472_02_003_02_V002.h5"
    echo
    exit 1
}

prompt_credentials
  detect_app_approval() {
    approved=`curl -s -b "$cookiejar" -c "$cookiejar" -L --max-redirs 5 --netrc-file "$netrc" https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.12.31/GEDI02_A_2020366140542_O11630_04_T07472_02_003_02_V002.h5 -w '\n%{http_code}' | tail  -1`
    if [ "$approved" -ne "200" ] && [ "$approved" -ne "301" ] && [ "$approved" -ne "302" ]; then
        # User didn't approve the app. Direct users to approve the app in URS
        exit_with_error "Please ensure that you have authorized the remote application by visiting the link below "
    fi
}

setup_auth_curl() {
    # Firstly, check if it require URS authentication
    status=$(curl -s -z "$(date)" -w '\n%{http_code}' https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.12.31/GEDI02_A_2020366140542_O11630_04_T07472_02_003_02_V002.h5 | tail -1)
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
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.11.13/GEDI02_A_2020318093417_O10883_04_T07916_02_003_02_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.10.04/GEDI02_A_2020278125206_O10265_01_T08566_02_003_02_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.07.21/GEDI02_A_2020203182648_O09106_01_T00028_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.07.17/GEDI02_A_2020199200141_O09045_01_T03991_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.07.08/GEDI02_A_2020190235733_O08908_01_T00013_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.07.01/GEDI02_A_2020183030532_O08786_01_T04282_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.06.27/GEDI02_A_2020179044014_O08725_01_T01436_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.06.17/GEDI02_A_2020169201324_O08580_04_T03142_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.06.14/GEDI02_A_2020166210108_O08534_04_T03815_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.06.13/GEDI02_A_2020165214801_O08519_04_T01719_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.06.10/GEDI02_A_2020162223544_O08473_04_T00663_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.06.03/GEDI02_A_2020155014450_O08351_04_T03356_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2020.01.11/GEDI02_A_2020011222012_O06132_01_T04297_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.11.25/GEDI02_A_2019329051323_O05392_04_T05238_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.11.08/GEDI02_A_2019312000445_O05125_01_T01145_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.11.03/GEDI02_A_2019307022954_O05049_01_T05399_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.10.29/GEDI02_A_2019302154448_O04980_04_T00816_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.10.16/GEDI02_A_2019289090159_O04774_01_T02721_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.10.11/GEDI02_A_2019284112653_O04698_01_T03976_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.10.06/GEDI02_A_2019279012908_O04614_04_T04259_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.09.23/GEDI02_A_2019266175745_O04423_01_T01604_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.09.19/GEDI02_A_2019262193446_O04362_01_T00992_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.09.10/GEDI02_A_2019253111453_O04217_04_T02086_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.09.01/GEDI02_A_2019244025635_O04072_01_T01451_02_003_02_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.08.13/GEDI02_A_2019225102018_O03782_01_T00028_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.08.08/GEDI02_A_2019220124906_O03706_01_T03670_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.08.07/GEDI02_A_2019219011510_O03683_04_T00143_02_003_02_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.07.24/GEDI02_A_2019205062002_O03469_04_T03509_02_003_02_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.07.13/GEDI02_A_2019194225448_O03309_01_T05552_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.06.27/GEDI02_A_2019178171438_O03057_04_T04412_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.06.18/GEDI02_A_2019169090309_O02912_01_T01283_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.06.08/GEDI02_A_2019159122919_O02759_01_T04144_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.05.22/GEDI02_A_2019142074101_O02492_04_T01566_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.05.17/GEDI02_A_2019137210602_O02423_01_T05414_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.05.15/GEDI02_A_2019135102408_O02385_04_T04565_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.05.10/GEDI02_A_2019130234903_O02316_01_T05567_02_003_01_V002.h5
https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.05.08/GEDI02_A_2019128130705_O02278_04_T00449_02_003_01_V002.h5
EDSCEOF
