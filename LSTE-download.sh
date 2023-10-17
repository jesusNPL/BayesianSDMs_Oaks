#!/bin/sh

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
    read -p "Username (jpintole): " username
    username=${username:-jpintole}
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
    echo "https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.11.01/MOD11C3.A2020306.006.2020338103603.hdf"
    echo
    exit 1
}

prompt_credentials
  detect_app_approval() {
    approved=`curl -s -b "$cookiejar" -c "$cookiejar" -L --max-redirs 2 --netrc-file "$netrc" https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.11.01/MOD11C3.A2020306.006.2020338103603.hdf -w %{http_code} | tail  -1`
    if [ "$approved" -ne "302" ]; then
        # User didn't approve the app. Direct users to approve the app in URS
        exit_with_error "Please ensure that you have authorized the remote application by visiting the link below "
    fi
}

setup_auth_curl() {
    # Firstly, check if it require URS authentication
    status=$(curl -s -z "$(date)" -w %{http_code} https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.11.01/MOD11C3.A2020306.006.2020338103603.hdf | tail -1)
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
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.11.01/MOD11C3.A2020306.006.2020338103603.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.10.01/MOD11C3.A2020275.006.2020306085445.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.09.01/MOD11C3.A2020245.006.2020283130606.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.08.01/MOD11C3.A2020214.006.2020245104504.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.07.01/MOD11C3.A2020183.006.2020214095409.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.06.01/MOD11C3.A2020153.006.2020183164401.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.05.01/MOD11C3.A2020122.006.2020153200133.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.04.01/MOD11C3.A2020092.006.2020126012105.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.03.01/MOD11C3.A2020061.006.2020092085955.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.02.01/MOD11C3.A2020032.006.2020063044908.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2020.01.01/MOD11C3.A2020001.006.2020032085800.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.12.01/MOD11C3.A2019335.006.2020003002230.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.11.01/MOD11C3.A2019305.006.2019335090127.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.10.01/MOD11C3.A2019274.006.2019305093058.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.09.01/MOD11C3.A2019244.006.2019275220210.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.08.01/MOD11C3.A2019213.006.2019248213841.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.07.01/MOD11C3.A2019182.006.2019213085210.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.06.01/MOD11C3.A2019152.006.2019184070427.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.05.01/MOD11C3.A2019121.006.2019152083229.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.04.01/MOD11C3.A2019091.006.2019121091211.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.03.01/MOD11C3.A2019060.006.2019091090500.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.02.01/MOD11C3.A2019032.006.2019082041423.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2019.01.01/MOD11C3.A2019001.006.2019038152143.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.12.01/MOD11C3.A2018335.006.2019032183326.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.11.01/MOD11C3.A2018305.006.2018335100941.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.10.01/MOD11C3.A2018274.006.2018313170053.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.09.01/MOD11C3.A2018244.006.2018278151628.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.08.01/MOD11C3.A2018213.006.2018247183632.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.07.01/MOD11C3.A2018182.006.2018213093743.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.06.01/MOD11C3.A2018152.006.2018182091649.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.05.01/MOD11C3.A2018121.006.2018153093002.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.04.01/MOD11C3.A2018091.006.2018121150952.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.03.01/MOD11C3.A2018060.006.2018093182920.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.02.01/MOD11C3.A2018032.006.2018060085728.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2018.01.01/MOD11C3.A2018001.006.2018032091151.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.12.01/MOD11C3.A2017335.006.2018003155403.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.11.01/MOD11C3.A2017305.006.2017335083953.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.10.01/MOD11C3.A2017274.006.2017305192434.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.09.01/MOD11C3.A2017244.006.2017275195152.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.08.01/MOD11C3.A2017213.006.2017249231323.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.07.01/MOD11C3.A2017182.006.2017213085355.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.06.01/MOD11C3.A2017152.006.2017187193442.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.05.01/MOD11C3.A2017121.006.2017162222451.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.04.01/MOD11C3.A2017091.006.2017128142739.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.03.01/MOD11C3.A2017060.006.2017092184027.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.02.01/MOD11C3.A2017032.006.2017062155051.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2017.01.01/MOD11C3.A2017001.006.2017032204847.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.12.01/MOD11C3.A2016336.006.2017003134141.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.11.01/MOD11C3.A2016306.006.2016340145445.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.10.01/MOD11C3.A2016275.006.2016306101141.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.09.01/MOD11C3.A2016245.006.2016275092314.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.08.01/MOD11C3.A2016214.006.2016286174519.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.07.01/MOD11C3.A2016183.006.2016243201931.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.06.01/MOD11C3.A2016153.006.2016242203102.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.05.01/MOD11C3.A2016122.006.2016242202235.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.04.01/MOD11C3.A2016092.006.2016242201829.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.03.01/MOD11C3.A2016061.006.2016242201416.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.02.01/MOD11C3.A2016032.006.2016235185442.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2016.01.01/MOD11C3.A2016001.006.2016234032549.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.12.01/MOD11C3.A2015335.006.2016234032021.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.11.01/MOD11C3.A2015305.006.2016230145029.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.10.01/MOD11C3.A2015274.006.2016229085916.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.09.01/MOD11C3.A2015244.006.2016228040328.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.08.01/MOD11C3.A2015213.006.2016227013226.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.07.01/MOD11C3.A2015182.006.2016224093218.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.06.01/MOD11C3.A2015152.006.2016223171353.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.05.01/MOD11C3.A2015121.006.2016223171406.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.04.01/MOD11C3.A2015091.006.2016223171359.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.03.01/MOD11C3.A2015060.006.2016223171408.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.02.01/MOD11C3.A2015032.006.2016223171450.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2015.01.01/MOD11C3.A2015001.006.2016223171404.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.12.01/MOD11C3.A2014335.006.2016223165244.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.11.01/MOD11C3.A2014305.006.2016211185741.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.10.01/MOD11C3.A2014274.006.2016210130514.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.09.01/MOD11C3.A2014244.006.2016210011609.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.08.01/MOD11C3.A2014213.006.2016210010611.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.07.01/MOD11C3.A2014182.006.2016210010600.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.06.01/MOD11C3.A2014152.006.2016210010556.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.05.01/MOD11C3.A2014121.006.2016204075046.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.04.01/MOD11C3.A2014091.006.2016203043447.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.03.01/MOD11C3.A2014060.006.2016201175112.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.02.01/MOD11C3.A2014032.006.2016198015912.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2014.01.01/MOD11C3.A2014001.006.2016198015922.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.12.01/MOD11C3.A2013335.006.2016198014908.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.11.01/MOD11C3.A2013305.006.2016175045738.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.10.01/MOD11C3.A2013274.006.2016173163728.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.09.01/MOD11C3.A2013244.006.2016170001630.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.08.01/MOD11C3.A2013213.006.2016166234557.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.07.01/MOD11C3.A2013182.006.2016166234606.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.06.01/MOD11C3.A2013152.006.2016162061523.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.05.01/MOD11C3.A2013121.006.2016161003505.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.04.01/MOD11C3.A2013091.006.2016157005759.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.03.01/MOD11C3.A2013060.006.2016156075058.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.02.01/MOD11C3.A2013032.006.2016156075039.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2013.01.01/MOD11C3.A2013001.006.2016156075046.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.12.01/MOD11C3.A2012336.006.2016141171948.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.11.01/MOD11C3.A2012306.006.2016135203526.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.10.01/MOD11C3.A2012275.006.2016135201732.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.09.01/MOD11C3.A2012245.006.2016131164644.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.08.01/MOD11C3.A2012214.006.2016131164637.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.07.01/MOD11C3.A2012183.006.2016131164636.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.06.01/MOD11C3.A2012153.006.2016127181639.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.05.01/MOD11C3.A2012122.006.2016127181622.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.04.01/MOD11C3.A2012092.006.2016127182625.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.03.01/MOD11C3.A2012061.006.2016107052016.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.02.01/MOD11C3.A2012032.006.2016105152522.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2012.01.01/MOD11C3.A2012001.006.2016104190154.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.12.01/MOD11C3.A2011335.006.2016083053717.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.11.01/MOD11C3.A2011305.006.2016083042644.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.10.01/MOD11C3.A2011274.006.2016078065636.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.09.01/MOD11C3.A2011244.006.2016077043853.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.08.01/MOD11C3.A2011213.006.2016077043853.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.07.01/MOD11C3.A2011182.006.2016077044003.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.06.01/MOD11C3.A2011152.006.2016077043841.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.05.01/MOD11C3.A2011121.006.2016077043850.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.04.01/MOD11C3.A2011091.006.2016055034833.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.03.01/MOD11C3.A2011060.006.2016053233141.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.02.01/MOD11C3.A2011032.006.2016053232552.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2011.01.01/MOD11C3.A2011001.006.2016053231853.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.12.01/MOD11C3.A2010335.006.2016053231052.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.11.01/MOD11C3.A2010305.006.2016047174756.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.10.01/MOD11C3.A2010274.006.2016046134041.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.09.01/MOD11C3.A2010244.006.2016045073352.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.08.01/MOD11C3.A2010213.006.2016044004340.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.07.01/MOD11C3.A2010182.006.2016042163707.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.06.01/MOD11C3.A2010152.006.2016041073454.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.05.01/MOD11C3.A2010121.006.2016040020754.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.04.01/MOD11C3.A2010091.006.2016035165008.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.03.01/MOD11C3.A2010060.006.2016035030520.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.02.01/MOD11C3.A2010032.006.2016035025659.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2010.01.01/MOD11C3.A2010001.006.2016035025508.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.12.01/MOD11C3.A2009335.006.2016024230123.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.11.01/MOD11C3.A2009305.006.2016024153834.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.10.01/MOD11C3.A2009274.006.2016023182148.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.09.01/MOD11C3.A2009244.006.2016022155629.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.08.01/MOD11C3.A2009213.006.2016020233504.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.07.01/MOD11C3.A2009182.006.2016019234526.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.06.01/MOD11C3.A2009152.006.2016019233643.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.05.01/MOD11C3.A2009121.006.2016019232855.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.04.01/MOD11C3.A2009091.006.2016012210226.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.03.01/MOD11C3.A2009060.006.2016009020234.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.02.01/MOD11C3.A2009032.006.2016007161930.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2009.01.01/MOD11C3.A2009001.006.2016006051904.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.12.01/MOD11C3.A2008336.006.2016004225320.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.11.01/MOD11C3.A2008306.006.2015365193623.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.10.01/MOD11C3.A2008275.006.2015364014154.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.09.01/MOD11C3.A2008245.006.2015358153859.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.08.01/MOD11C3.A2008214.006.2015357223909.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.07.01/MOD11C3.A2008183.006.2015357222956.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.06.01/MOD11C3.A2008153.006.2015357222417.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.05.01/MOD11C3.A2008122.006.2015357222103.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.04.01/MOD11C3.A2008092.006.2015349111408.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.03.01/MOD11C3.A2008061.006.2015344142644.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.02.01/MOD11C3.A2008032.006.2015343142031.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2008.01.01/MOD11C3.A2008001.006.2015338162249.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.12.01/MOD11C3.A2007335.006.2015337030259.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.11.01/MOD11C3.A2007305.006.2015329174118.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.10.01/MOD11C3.A2007274.006.2015327192531.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.09.01/MOD11C3.A2007244.006.2015324212410.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.08.01/MOD11C3.A2007213.006.2015323085321.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.07.01/MOD11C3.A2007182.006.2015321160040.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.06.01/MOD11C3.A2007152.006.2015319092830.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.05.01/MOD11C3.A2007121.006.2015317210238.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.04.01/MOD11C3.A2007091.006.2015317181332.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.03.01/MOD11C3.A2007060.006.2015312170244.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.02.01/MOD11C3.A2007032.006.2015311055620.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2007.01.01/MOD11C3.A2007001.006.2015309193510.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.12.01/MOD11C3.A2006335.006.2015308210726.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.11.01/MOD11C3.A2006305.006.2015306231709.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.10.01/MOD11C3.A2006274.006.2015304013920.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.09.01/MOD11C3.A2006244.006.2015301181148.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.08.01/MOD11C3.A2006213.006.2015300064518.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.07.01/MOD11C3.A2006182.006.2015296002755.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.06.01/MOD11C3.A2006152.006.2015294171121.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.05.01/MOD11C3.A2006121.006.2015292062509.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.04.01/MOD11C3.A2006091.006.2015289132354.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.03.01/MOD11C3.A2006060.006.2015286193236.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.02.01/MOD11C3.A2006032.006.2015286191958.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2006.01.01/MOD11C3.A2006001.006.2015274093914.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.12.01/MOD11C3.A2005335.006.2015271024503.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.11.01/MOD11C3.A2005305.006.2015270182016.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.10.01/MOD11C3.A2005274.006.2015266065502.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.09.01/MOD11C3.A2005244.006.2015264012409.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.08.01/MOD11C3.A2005213.006.2015261045434.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.07.01/MOD11C3.A2005182.006.2015257141535.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.06.01/MOD11C3.A2005152.006.2015254220416.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.05.01/MOD11C3.A2005121.006.2015253022613.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.04.01/MOD11C3.A2005091.006.2015249023011.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.03.01/MOD11C3.A2005060.006.2015247165209.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.02.01/MOD11C3.A2005032.006.2015245145938.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2005.01.01/MOD11C3.A2005001.006.2015243152039.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.12.01/MOD11C3.A2004336.006.2015240195145.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.11.01/MOD11C3.A2004306.006.2015239215242.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.10.01/MOD11C3.A2004275.006.2015237082923.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.09.01/MOD11C3.A2004245.006.2015232081503.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.08.01/MOD11C3.A2004214.006.2015230194326.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.07.01/MOD11C3.A2004183.006.2015228011859.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.06.01/MOD11C3.A2004153.006.2015226172412.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.05.01/MOD11C3.A2004122.006.2015221084616.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.04.01/MOD11C3.A2004092.006.2015219233820.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.03.01/MOD11C3.A2004061.006.2015218062843.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.02.01/MOD11C3.A2004032.006.2015217023039.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2004.01.01/MOD11C3.A2004001.006.2015213011324.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.12.01/MOD11C3.A2003335.006.2015212180427.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.11.01/MOD11C3.A2003305.006.2015210142627.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.10.01/MOD11C3.A2003274.006.2015208231633.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.09.01/MOD11C3.A2003244.006.2015206075849.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.08.01/MOD11C3.A2003213.006.2015204221025.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.07.01/MOD11C3.A2003182.006.2015203200900.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.06.01/MOD11C3.A2003152.006.2015199083522.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.05.01/MOD11C3.A2003121.006.2015197050250.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.04.01/MOD11C3.A2003091.006.2015196011629.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.03.01/MOD11C3.A2003060.006.2015188021128.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.02.01/MOD11C3.A2003032.006.2015184095028.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2003.01.01/MOD11C3.A2003001.006.2015182172300.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.12.01/MOD11C3.A2002335.006.2015168204837.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.11.01/MOD11C3.A2002305.006.2015166184010.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.10.01/MOD11C3.A2002274.006.2015163030241.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.09.01/MOD11C3.A2002244.006.2015161104513.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.08.01/MOD11C3.A2002213.006.2015150095919.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.07.01/MOD11C3.A2002182.006.2015149014716.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.06.01/MOD11C3.A2002152.006.2015145004928.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.05.01/MOD11C3.A2002121.006.2015144002130.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.04.01/MOD11C3.A2002091.006.2015142174958.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.03.01/MOD11C3.A2002060.006.2015141141446.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.02.01/MOD11C3.A2002032.006.2015140145439.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2002.01.01/MOD11C3.A2002001.006.2015138153432.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.12.01/MOD11C3.A2001335.006.2015128013822.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.11.01/MOD11C3.A2001305.006.2015127182103.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.10.01/MOD11C3.A2001274.006.2015126234048.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.09.01/MOD11C3.A2001244.006.2015125230801.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.08.01/MOD11C3.A2001213.006.2015125000018.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.07.01/MOD11C3.A2001182.006.2015122075417.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.06.01/MOD11C3.A2001152.006.2015120023648.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.05.01/MOD11C3.A2001121.006.2015118202358.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.04.01/MOD11C3.A2001091.006.2015116210803.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.03.01/MOD11C3.A2001060.006.2015115205713.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.02.01/MOD11C3.A2001032.006.2015114195201.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2001.01.01/MOD11C3.A2001001.006.2015112224542.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.12.01/MOD11C3.A2000336.006.2015111162742.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.11.01/MOD11C3.A2000306.006.2015111040233.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.10.01/MOD11C3.A2000275.006.2015075090705.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.09.01/MOD11C3.A2000245.006.2015071235605.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.08.01/MOD11C3.A2000214.006.2015069162120.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.07.01/MOD11C3.A2000183.006.2015066033219.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.06.01/MOD11C3.A2000153.006.2015064110836.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.05.01/MOD11C3.A2000122.006.2015062180458.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.04.01/MOD11C3.A2000092.006.2015060123950.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.03.01/MOD11C3.A2000061.006.2015058070048.hdf
https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOLT/MOD11C3.006/2000.02.01/MOD11C3.A2000032.006.2015058203309.hdf
EDSCEOF